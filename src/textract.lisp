(in-package :warflagger)

(defun extract-opinml-meta-from-html (stream)
  (let ((nodes (lquery:$ (initialize stream) "[property^=opinml:]")))
    (loop for node across nodes
          for attribs = (plump:attributes node)
          collect (cons (gethash "property" attribs) (gethash "content" attribs)))))

(defun url-has-opinml-metadata? (url)
  (let ((meta (wf/ipfs:ipfs-extracted-metadata url)))
    (when meta
      (getf meta :opinml-metadata))))

(defun url-metadata-points-to-opinml-source? (url)
  (let ((res (url-has-opinml-metadata? url)))
    (and (assoc "opinml:opinion" res :test #'equal)
         (< (length (gadgets:assoc-all "opinml:opinion" res :test #'equal)) 2)
         (assoc-cdr "opinml:opinion" res :test #'equal))))

;;FIXME: need offset info
(defun extract-links-from-plump-object (pobj)
  ;; "a" -> "p[href]" for readability processed stuff
  (let ((links (lquery:with-master-document (pobj) (lquery:$ "p[href]"))))
    (cl-utilities:collecting
        (loop for link across links
             do (cl-utilities:collect (list :excerpt (plump:text link)
                                            :target (plump:attribute link "href")))))))

(defun tt-extract-html (page)
  (let* (;;(page (flexi-streams:make-flexi-stream page :external-format :utf-8))
         (pobj (plump:parse page))
         (title (string-strip (readability::get-article-title pobj)))
         (meta (extract-opinml-meta-from-html page))
         (article (readability::grab-article pobj))
         (links (extract-links-from-plump-object article))
         ;(simple-page (plump:serialize article nil))
         (text (string-strip (readability::inner-text article))))
    (values title text meta links article)))

(defun tt-extract-text (page)
  (let* (;(page (flexi-streams:make-flexi-stream page :external-format :utf-8))
         (text (string-strip (read-stream-content-into-string page)))
         (title (gadgets:part-on-true
                 (lambda (x) (member x '(#\Linefeed #\Newline #\Return)))
                 text))
         ;;FIXME: Extract links
         (links nil))
    (values title nil links nil)))

(defun tt-get-page-from-archive (url)
  ;;Try common crawl first
  (let* ((captures (crawly:url-search url :limit 1 :source :common-crawl))
         (newurl (crawly:capture-url (car captures)))
         (warc (when captures (crawly:get-archive-from-capture :common-crawl (car captures)))))
    (multiple-value-bind (page header) (when warc (crawly:get-record-for-url warc newurl))
      (if page
          (values page (gethash :content-type header))
          (progn
            (log:warn "~a"
                      (cond ((and captures warc) "Common Crawl: unable to extract page from WARC")
                            (captures "Common Crawl: unable to fetch WARC for URL")
                            (t "Common Crawl: URL not found")))
            (let ((captures (crawly:url-search url :limit 1 :source :internet-archive))
                  (content-type (when captures (gadgets:assoc-cdr :mime (car captures))))
                  (page (when captures
                          (crawly:get-archive-from-capture :internet-archive (car captures)))))
              (if page
                  (values page content-type)
                  (progn
                    (log:warn "~a" (cond (captures "Internet Archive: unable to fetch page")
                                         (t "Internet Archive: URL not found")))
                    nil))))))))

;; Default to /dev/null
(defvar *string-stream* (make-broadcast-stream))
(defclass stream-appender2 (log4cl:stream-appender) ())
(defmethod log4cl:appender-stream ((this stream-appender2)) *string-stream*)
(defparameter *handle-extraction-errors* t)

(defun call-with-log-dump (callable)
  (let ((appender (make-instance 'stream-appender2))
        (res nil))
    (log4cl:add-appender log4cl:*root-logger* appender)
    (let
        ((errlog
           (with-output-to-string (s)
             (let ((*string-stream* s))
               (if *handle-extraction-errors*
                   (handler-case
                       (progn
                         (setf res (multiple-value-list (funcall callable)))
                         (log4cl:remove-appender log4cl:*root-logger* appender))
                     (error (e)
                       (log:error "~a" e)
                       (log4cl:remove-appender log4cl:*root-logger* appender)))
                   (unwind-protect
                        (setf res (multiple-value-list (funcall callable)))
                     (log4cl:remove-appender log4cl:*root-logger* appender)))))))
      (values-list (list* errlog res)))))

(defun tt-process-page (url page mime-type)
  (let ((mime-type (car (split-sequence:split-sequence #\; mime-type))))
    (if page
        (multiple-value-bind (title text metadata links)
            (cond
              ((equal mime-type "application/pdf")
               (error "PDF extraction not currently supported"))
              ((equal mime-type "text/html")
               (tt-extract-html page))
              ((equal mime-type "text/plain")
               (tt-extract-text page))
              (t (error (format nil "Extraction of mime type ~a not currently supported" mime-type))))
          (values text `(:title
                         ,title
                         ,@(when metadata (list :opinml-metadata metadata))
                         ,@(when links (list :links links)))))
        (log:error "Page not available for extraction"))))

(defun tt-process (url path mime-type)
  (if path
      (with-open-file (s path)
        (tt-process-page url s mime-type))
      (multiple-value-bind (page type) (tt-get-page-from-archive url)
        (tt-process-page url page (or mime-type type)))))

(defun tt-update-page-data (url &optional path mime-type)
  (restart-case
      (multiple-value-bind (log text meta)
          (call-with-log-dump (lambda () (tt-process url path mime-type)))
        (when text (wf/ipfs:ipfs-write-extracted-text url text))
        (wf/ipfs:ipfs-write-extracted-metadata url (list* :errors log meta))
        (wf/ipfs:ipfs-write-partial-rooturl-data url))
    (skip-extraction () nil)))

(defun tt-is-cached (url)
  (or (wf/ipfs:ipfs-file-exists-p (wf/ipfs:ipfs-rooturl-path url "extracted-text.txt"))
      (wf/ipfs:ipfs-file-exists-p (wf/ipfs:ipfs-rooturl-path url "extracted-metadata.data"))))

(defun tt-has-failure (url)
  (getf (wf/ipfs:ipfs-extracted-metadata url) :errors))

(defun text-server (url)
  "This function, served as JSON, is the text server. Url is the address of the desired text"
  (hu:collecting-hash-table
      (:mode :replace)
    (cond
      ((or (null url) (eq 0 (length url)))
       (hu:collect :text "")
       (hu:collect :title "")
       (hu:collect :status "failure")
       (hu:collect :message "No URL provided"))
      ((not (ratify:url-p url))
       (hu:collect :text "")
       (hu:collect :title "")
       (hu:collect :status "failure")
       (hu:collect :message "Not a valid URL"))
      (t
       (let* ((meta (when (wf/ipfs:ipfs-file-exists-p
                           (wf/ipfs:ipfs-rooturl-path url "extracted-metadata.data"))
                      (wf/ipfs:ipfs-extracted-metadata url)))
              (text (if
                     (wf/ipfs:ipfs-file-exists-p (wf/ipfs:ipfs-rooturl-path url "extracted-text.txt"))
                     (wf/ipfs:ipfs-extracted-text url)
                     "")))
         (cond
           ((getf meta :errors)
            (hu:collect :text text)
            (hu:collect :title (getf meta :title ""))
            (hu:collect :status "failure")
            (hu:collect :message (getf meta :errors)))
           ((or (not-empty text) (getf meta :title))
            (hu:collect :text text)
            (hu:collect :title (getf meta :title ""))
            (hu:collect :status "success")
            (hu:collect :message ""))
           (t
            ;;Can't detect pending now. Change that if needed?
            ;;(unless (is-pending url) (update-page url))
            (tt-update-page-data url)
            (hu:collect :text text)
            (hu:collect :title (getf meta :title ""))
            (hu:collect :status "wait")
            (hu:collect :message "Loading page text..."))))))))
