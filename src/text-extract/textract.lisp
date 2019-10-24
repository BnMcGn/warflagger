;;;
;;; textract.lisp
;;;
;;; Service to extract and cache the text content of web pages.
;;;

(defpackage :wf/text-extract
  (:use #:cl #:gadgets #:alexandria #:wf/local-settings)
  (:export
   #:grab-page
   #:grab-text
   #:grab-title
   #:clean-locks
   #:is-cached
   #:is-fresh
   #:has-failure
   #:fresh-failure
   #:is-pending
   #:old-page-available
   #:*cache-path*
   #:*cache-age*
   #:*bynum*
   #:*byurl*
   #:*text-extractor-script*
   #:initialize-indices
   #:update-page
   #:text-server
   #:grab-links
   #:grab-failed-message
   #:grab-messages))

(in-package :wf/text-extract)

;;;(defparameter *cache-path* "/home/ben/opinml/")
(defparameter *cache-age* (encode-time-delta 0 10 0 0))
(defvar *bynum*)
(defvar *byurl*)
;;(defparameter *text-extractor-script*
;;  "/home/ben/quicklisp/local-projects/warflagger/src/text-extract/textract.py")

(defun cache-loc (url)
  (concatenate
   'string (princ-to-string *cache-path*) (princ-to-string (gethash url *byurl*)) "/"))

(defun page-loc (url)
  (concatenate 'string (cache-loc url) "page.html"))

(defun text-loc (url)
  (concatenate 'string (cache-loc url) "text"))

(defun title-loc (url)
  (concatenate 'string (cache-loc url) "title"))

(defun failure-loc (url)
  (concatenate 'string (cache-loc url) "failed"))

(defun links-loc (url)
  (concatenate 'string (cache-loc url) "links"))

(defun messages-loc (url)
  (concatenate 'string (cache-loc url) "messages"))

(defun update-page (url &key force)
  (unless (and (is-fresh url) (not force))
    (save-page-to-cache url)))

;;;FIXME: Doesn't account for .pdf, etc
;;;FIXME: SECURITY: Block file urls, at least when live
;;;UPD: assumes html
(defun grab-page (url &key (update t))
  (when update (update-page url))
  (with-file-lock ((make-pathname :directory (cache-loc url) :name "main"))
    (open (make-pathname :directory (cache-loc url) :name "page.html"))))

;;;FIXME: Need to clean up after crash that leaves main.lock in place. 
(defun grab-text (url &key (update t))
  (when update (update-page url))
  (let ((tname
          (make-pathname
           :directory (cache-loc url)
           :name
           (if (probe-file
                (make-pathname :directory (cache-loc url) :name "text.locked"))
               "text.locked" "text"))))
    (if (probe-file tname)
        (with-file-lock ((make-pathname :directory (cache-loc url) :name "main"))
          (read-file-into-string tname))
        (error (format nil "Text not available: ~a" (grab-messages url))))))

(defun grab-title (url &key (alternate "[No Title]") (update t))
  (when update (update-page url))
  (let ((tfile (make-pathname :directory (cache-loc url) :name "title")))
    (if (probe-file tfile)
        (with-file-lock (tfile)
          (apply #'concatenate 'string
                 (map-file-by-line #'identity (title-loc url))))
        alternate)))

(defun grab-links (url &key update)
  (when update (update-page url))
  "The links file is optional. It is only generated if the source document is html."
  (let ((lfile (links-loc url)))
    (when (probe-file lfile)
      (mapcar (lambda (x)
                (mapcar (lambda (y) (if (emptyp y) nil y)) x))
              (with-file-lock (lfile)
                (with-open-file (s lfile)
                  (json:decode-json s)))))))

(defun grab-failed-message (url)
  (unless (probe-file (failure-loc url))
    (error "No failure message found"))
  (read-file-into-string (failure-loc url)))

(defun grab-messages (url)
  (if (probe-file (messages-loc url))
      (read-file-into-string (messages-loc url))
      ""))

(defun index-file-name ()
  (merge-pathnames "urlindex.inf" *cache-path*))

(defun read-index-file (fname)
  (let ((data
          (multiple-value-apply
           #'pairlis
           (with-collectors (url< num<)
             (do-file-by-line (line fname)
               (destructuring-bind (num url) (split-sequence #\space line)
                 (num< (parse-integer num))
                 (url< (string-trim '(#\space #\newline) url))))))))
    (values
     (collecting-hash-table (:mode :append)
       (mapc (lambda (x)
               (collect (cdr x) (car x)))
             data))
     (alist-hash-table data :test #'equal))))

(defun write-index-file (fname bynum)
  (with-open-file (s fname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (do-hash-table (k v bynum)
      (dolist (url v)
        (format s "~d ~a~%" k url)))))

(let ((highest nil))
  (defun new-index (&optional set)
    (declare (type (or null integer) set))
    (if set
        (setf highest set)
        (if highest
            (let ((new (1+ (apply #'max highest (hash-table-keys *bynum*)))))
              (setf highest new)
              new)
            (error "Attempt to fetch new index before initialized")))))

(defun highest-existing ()
  (apply
   #'max -1
   (remove-if
    #'null
    (mapcar (lambda (d) (or (parse-integer (lastcar (pathname-directory d)) :junk-allowed t)))
            (directory (merge-pathnames "*" *cache-path*))))))

(defun initialize-indices ()
  (if (probe-file (index-file-name))
      (multiple-value-bind (bynum byurl)
          (read-index-file (index-file-name))
        (setf *byurl* byurl)
        (setf *bynum* bynum))
      (progn
        (setf *byurl* (make-hash-table :test #'equal))
        (setf *bynum* (make-hash-table))))
  ;; Initialize new-index so as not to get mixed up with existing items in cache
  (new-index (highest-existing)))

;;;FIXME: More checking of urls required. No file/local urls.
(defun valid-url-p (url)
  (ratify:url-p url))

(defun get-url-index (url)
  (aif2 (gethash url *byurl*)
        it
        (let ((newkey (new-index)))
          ;;FIXME: No check to see if loc is used. Either avoid or erase.
          (setf (gethash newkey *bynum*) (list url))
          (setf (gethash url *byurl*) newkey)
          (write-index-file (index-file-name) *bynum*)
          newkey)))

(defun save-page-to-cache (url)
  (let ((index (get-url-index url)))
    (ensure-directories-exist (make-pathname :directory (cache-loc url)))
    ;;FIXME: Need a way to not cache non-existent urls. Otherwise will get major clutter
    ;; from half-typed urls and malicious users.
    (unless (probe-file *text-extractor-script*)
      (error "Couldn't find text extractor script."))
    (when (probe-file (messages-loc url))
      (delete-file (messages-loc url)))
    (let ((process (external-program:start *text-extractor-script*
                                           (list (cache-loc url))
                                           :input :stream
                                           :error (messages-loc url))))
      (write-line url (external-program:process-input-stream process))
      (close (external-program:process-input-stream process))
      index)))

(defun clean-locks ()
  "Remove all lock files under *cache-path*"
  (dolist (file (directory (concatenate 'string *cache-path* "*/*.lock")))
    (delete-file file)))

(defun is-cached (url)
  (nth-value 1 (gethash url *byurl*)))

(defun fresh-file-p (path)
  (> *cache-age* (- (get-universal-time) (file-write-date path))))

(defun is-fresh (url)
  (and
   (is-cached url)
   (probe-file (text-loc url))
   (fresh-file-p (text-loc url))))

(defun has-failure (url)
  (and (is-cached url) (probe-file (failure-loc url))))

(defun fresh-failure (url)
  (and (has-failure url) (fresh-file-p (failure-loc url))))

(defun is-pending (url)
  (and (is-cached url)
       (probe-file (concatenate 'string (cache-loc url) "processing.lock"))))

(defun old-page-available (url)
  (and (is-cached url) (probe-file (page-loc url))))

(defun text-server (url)
  "This function, served as JSON, is the text server. Url is the address of the desired text"
  (cl-hash-util:collecting-hash-table (:mode :replace)
    (labels ((get-old ()
               (if (old-page-available url)
                   (grab-text url :update nil)
                   "")))
      (cond
        ((or (null url) (eq 0 (length url)))
         (cl-hash-util:collect :text "")
         (cl-hash-util:collect :title "")
         (cl-hash-util:collect :status "failure")
         (cl-hash-util:collect :message "No URL provided"))
        ((not (valid-url-p url))
         (cl-hash-util:collect :text "")
         (cl-hash-util:collect :title "")
         (cl-hash-util:collect :status "failure")
         (cl-hash-util:collect :message "Not a valid URL"))
        ((is-fresh url)
         (cl-hash-util:collect :text (grab-text url))
         (cl-hash-util:collect :title (grab-title url :alternate "" :update nil))
         (cl-hash-util:collect :status "success")
         (cl-hash-util:collect :message ""))
        ((fresh-failure url)
         (cl-hash-util:collect :text (get-old))
         (cl-hash-util:collect :title (grab-title url :alternate "" :update nil))
         (cl-hash-util:collect :status "failure")
         (cl-hash-util:collect :message "Failed to load URL"))
        (t
         (unless (is-pending url) (update-page url))
         (cl-hash-util:collect :text (get-old))
         (cl-hash-util:collect :title (grab-title url :alternate "" :update nil))
         (cl-hash-util:collect :status "wait")
         (cl-hash-util:collect :message "Loading page text..."))))))

