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
  (let ((links (lquery:$ (lquery:initialize pobj) "a")))
    (cl-utilities:collect
        (loop for link across links
             do (cl-utilities:collect (list :excerpt (plump:text link)
                                            :target (plump:attribute link "href")))))))

(defun tt-extract (page)
  (let* ((pobj (plump:parse page))
         (title (readability::get-article-title pobj))
         (meta (extract-opinml-meta-from-html page))
         (article (readability::grab-article pobj))
         (links (extract-links-from-plump-object article))
         (simple-page (plump:serialize article nil))
         (text (readability::inner-text article)))
    (values title text meta links article)))

(defun tt-get-page-from-archive (url)
  ;;Try common crawl first
  (let* ((errors nil)
         (captures (crawly:url-search url :limit 1 :source :common-crawl))
         (warc (when captures (crawly:get-archive-from-capture :common-crawl (car captures))))
         (page (when warc (crawly:get-record-for-url warc url))))
    (if page
        (return-from tt-get-page-from-archive (values nil page))
      (push
       (cond ((and captures warc) "Common Crawl: unable to extract page from WARC")
             (captures "Common Crawl: unable to fetch WARC for URL")
             (t "Common Crawl: URL not found"))
       errors))
    (let ((captures (crawly:url-search url :limit 1 :source :internet-archive))
          (page (when captures (crawly:get-archive-from-capture :internet-archive (car captures)))))
      (if page
          (return-from tt-get-page-from-archive (values (nreverse errors) page))
          (push
           (cond (captures "Internet Archive: unable to fetch page")
                 (t "Internet Archive: URL not found"))
           errors))
      (values (nreverse errors) page))))

(defun join-errors (&rest erratum)
  (funcall
   #'gadgets:string-join
   #\Newline
   (cl-utilities:collecting
     (dolist (err erratum)
       (if (stringp err)
           (cl-utilities:collect err)
           (mapcar #'cl-utilities:collect err))))))

(defun tt-write-page-data (url errors page)
  (if page
      (multiple-value-bind (title text metadata links) (tt-extract page)
        (if text
            (progn
              (wf/ipfs:ipfs-write-extracted-metadata
               url
               `(list
                 :title ,title
                 ,@(when metadata (list :opinml-metadata metadata))
                 ,@(when links (list :links links))))
              (wf/ipfs:ipfs-write-extracted-text url text))
            (wf/ipfs:ipfs-write-extracted-metadata
             url
             (list :errors
                   (join-errors errors "Warflagger: Couldn't extract text from page")))))
      (wf/ipfs:ipfs-write-extracted-metadata url (list :errors (join-errors errors)))))

(defun tt-update-page-data (url)
  (multiple-value-bind (errors page)
      (tt-get-page-from-archive url)
    (tt-write-page-data url errors page)))

(defun tt-update-page-data-from-file (url path)
  (with-open-file (s path)
    (tt-write-page-data url nil s)))

(defun tt-is-cached (url)
  (or (wf/ipfs:ipfs-file-exists-p (wf/ipfs:ipfs-rooturl-path url "extracted-text.txt"))
      (wf/ipfs:ipfs-file-exists-p (wf/ipfs:ipfs-rooturl-path url "extracted-metadata.txt"))))

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
                          (wf/ipfs:ipfs-rooturl-path url "extracted-metadata.txt"))
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
          ((or text (getf meta :title))
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
