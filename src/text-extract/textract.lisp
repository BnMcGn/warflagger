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
   #:text-server))

(in-package :wf/text-extract)

;;;(defparameter *cache-path* "/home/ben/opinml/")
(defparameter *cache-age* (encode-time-delta 0 0 1 0))
(defvar *bynum*)
(defvar *byurl*)
;;(defparameter *text-extractor-script*
;;  "/home/ben/quicklisp/local-projects/warflagger/src/text-extract/textract.py")

(defun cache-loc (url)
  (concatenate
   'string *cache-path* (princ-to-string (gethash url *byurl*)) "/"))

(defun page-loc (url)
  (concatenate 'string (cache-loc url) "page.html"))

(defun text-loc (url)
  (concatenate 'string (cache-loc url) "text"))

(defun title-loc (url)
  (concatenate 'string (cache-loc url) "title"))

(defun failure-loc (url)
  (concatenate 'string (cache-loc url) "failed"))

(defun update-page (url &key force)
  (unless (and (is-fresh url) (not force))
    (save-page-to-cache url)))

;;;FIXME: Doesn't account for .pdf, etc
;;;FIXME: SECURITY: Block file urls, at least when live
(defun grab-page (url &key (update t))
  (when update (update-page url))
  (with-file-lock ((make-pathname :directory (cache-loc url) :name "main"))
    (open (make-pathname :directory (cache-loc url) :name "page.html"))))

(defun grab-text (url &key (update t))
  (when update (update-page url))
  (with-file-lock ((make-pathname :directory (cache-loc url) :name "main"))
    (read-file-into-string
     (make-pathname :directory (cache-loc url) :name "text"))))

(defun grab-title (url &key (alternate "[No Title]") (update t))
  (when update (update-page url))
  (let ((tfile (make-pathname :directory (cache-loc url) :name "title")))
    (if (probe-file tfile)
        (with-file-lock (tfile)
          (apply #'concatenate 'string
                 (map-file-by-line #'identity (title-loc url))))
        alternate)))

(defun index-file-name ()
  (make-pathname :directory *cache-path* :name "urlindex.inf"))

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
  (with-open-file (s fname :direction :output :if-exists :supersede)
    (do-hash-table (k v bynum)
      (dolist (url v)
        (format s "~d ~a~%" k url)))))

(defun initialize-indices ()
  (if (probe-file (index-file-name))
      (multiple-value-bind (bynum byurl)
          (read-index-file (index-file-name))
        (setf *byurl* byurl)
        (setf *bynum* bynum))
      (progn
        (setf *byurl* (make-hash-table :test #'equal))
        (setf *bynum* (make-hash-table)))))

;;;FIXME: More checking of urls required. No file/local urls.
(defun valid-url-p (url)
  (ratify:url-p url))

(defun get-url-index (url)
  (aif2 (gethash url *byurl*)
        it
        (let ((newkey (1+ (apply #'max -1 (hash-table-keys *bynum*)))))
          (setf (gethash newkey *bynum*) (list url))
          (setf (gethash url *byurl*) newkey)
          (write-index-file (index-file-name) *bynum*)
          newkey)))

(defun save-page-to-cache (url)
  (let ((index (get-url-index url)))
    (ensure-directories-exist (make-pathname :directory (cache-loc url)))
    (unless (probe-file *text-extractor-script*)
      (error "Couldn't find text extractor script."))
    (let ((process (external-program:start *text-extractor-script*
                                           (list (cache-loc url))
                                           :input :stream)))
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
         (cl-hash-util:collect :status "failure")
         (cl-hash-util:collect :message "No URL provided"))
        ((not (valid-url-p url))
         (cl-hash-util:collect :text "")
         (cl-hash-util:collect :status "failure")
         (cl-hash-util:collect :message "Not a valid URL"))
        ((is-fresh url)
         (cl-hash-util:collect :text (grab-text url))
         (cl-hash-util:collect :status "success")
         (cl-hash-util:collect :message ""))
        ((fresh-failure url)
         (cl-hash-util:collect :text (get-old))
         (cl-hash-util:collect :status "failure")
         (cl-hash-util:collect :message "Failed to load URL"))
        (t
         (unless (is-pending url) (update-page url))
         (cl-hash-util:collect :text (get-old))
         (cl-hash-util:collect :status "wait")
         (cl-hash-util:collect :message "Loading page text..."))))))

