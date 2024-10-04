(in-package #:cl-user)

(defpackage #:wf/admin
  (:use #:cl #:gadgets #:alexandria #:access #:sql-stuff #:clsql)
  )

(in-package #:wf/admin)



;;Thread tool

(defun all-worker-threads ()
  (remove-if-not
   (lambda (x)
     (search "hunchentoot-worker-" (bt:thread-name x)))
   (bt:all-threads)))

(defun all-ipns-threads ()
  (remove-if-not
   (lambda (x)
     (search "update ipns" (bt:thread-name x)))
   (bt:all-threads)))

(defun clear-worker-threads ()
  (mapcar #'bordeaux-threads:destroy-thread (all-worker-threads)))

(defun clear-ipns-threads ()
  (mapcar #'bordeaux-threads:destroy-thread (all-ipns-threads)))

;;Posting tools


(defparameter *user* nil)
(defparameter *target* nil)
(defparameter *posting-method* nil)
(defparameter *posting-directory* nil)
(defparameter *opinion-queue* nil)
(defparameter *alternate-base-url* nil)

(defun save-to-opinion-store (key opinion)
  (unless warflagger:*opinion-store*
    (setf warflagger:*opinion-store* (make-hash-table :test #'equal)))
  (setf (gethash key warflagger:*opinion-store*)
        ;;FIXME: extend opinion?
        opinion))

(defun post (opinion) (%post opinion *posting-method*))
(defgeneric %post (opinion posting-method))
(defmethod %post (opinion (posting-method (eql :directory)))
  (if *posting-directory*
      (save-to-opinion-store
       (warflagger::save-opinion-to-folder opinion *posting-directory*)
       opinion)
      (error "Can't post to directory with *posting-directory* unset")))
;;FIXME:: add a non-post, save in memory

;;Opinion makers

(defun prep-opinion (&key comment excerpt excerpt-offset flag reference
                          (target *target*) (author *user*))
  (prep-opinion-alist
   (list*
    (when comment (cons :comment comment))
    (when excerpt (cons :excerpt excerpt))
    (when excerpt-offset (cons :excerpt-offset excerpt-offset))
    (when flag (cons :flag flag))
    (when reference (cons :reference reference)))
   :target target :author author))

(defun prep-opinion-alist (opinion &key (target *target*) (author *user*))
  (let ((have-author (assoc :author opinion))
        (have-target (assoc :target opinion)))
    (list*
     (unless have-target
       (if target (cons :target target) (error "No target available")))
     (unless have-author
       (if author (cons :author author) (error "No author available")))
     (cons :url
           (let ((wf/local-settings:*base-url* (or *alternate-base-url* wf/local-settings:*base-url*)))
             (warflagger:make-opinion-url opinion)))
     (cons :rooturl
           (warflagger:get-rooturl-by-id (warflagger:find/store-root-url target)))
     opinion)))

(defun comment (comment)
  (post (prep-opinion :flag (list :custodial :blank) :comment comment)))

(defun suggest-text (text)
  (comment (format nil "#(suggest-target-text)~%~a" text)))

(defun suggest-title (text)
  (comment (format nil "#(suggest-target-title)~%~a" text)))

;;FIXME: always fetches original text, not updates. Also, should we do :clean-comment?
(defun target-text (&optional (target *target*))
  "Target is specified by url or iid, generally in the *target* var."
  (if (warflagger:iid-p target)
      (or (and *local-opinion-store*
               (assoc-cdr :comment (gethash target *local-opinion-store*)))
          (error "IID target not found"))
      (gethash :text (warflagger:text-server-dispatcher target))))


;;CSV input format:
;; - first row is header
;; Fields: flag, optional: comment, reference

(defun %parse-csv-row (row)
  (list
   :flag
   (let ((flag (warflagger::flag-to-lisp (car row))))
     (if (warflagger:recognized-flag-p flag)
         flag
         (error "Bad Flag")))
   :comment
   (when (second row)
     (unless (stringp (second row))
       (error "Comment should be string or nil"))
     (if (< 5000 (length (second row)))
         (error "Comment too long")
         (second row)))
   :reference
   (when (third row)
     (if (warflagger:url-p (third row))
         (third row)
         (error "Reference is not an URL")))))

(defun %parse-csv-opinion-input (source)
  (mapcar #'%parse-csv-row (cdr (cl-csv:read-csv source))))

;;Assumes a db backend
(def-query iid-search (search)
  (mapcar
   #'car
   (query-marker
    (select (colm 'opinion 'iid)
      :from (tabl 'opinion)
      :where (sql-like (colm 'opinion 'iid) (strcat "%" search "%"))))))

(def-query rooturl-search (search)
  (mapcar
   #'car
   (query-marker
    (select (colm 'rooturl 'rooturl)
      :from (tabl 'rooturl)
      :where (sql-like (colm 'rooturl 'rooturl) (strcat "%" search "%"))))))

(def-query reference-search (search)
  (mapcar
   #'car
   (query-marker
    (select (colm 'reference 'reference)
      :from (tabl 'reference)
      :where (sql-like (colm 'reference 'reference) (strcat "%" search "%"))))))


;;(with-open-file (s #p"~/tmp/file.edn")
;;  (ipfs:with-files-write
;;      (sout (wf/ipfs:ipfs-opinion-path tiid "hiccup.edn") :create t :truncate t)
;;    (uiop:copy-stream-to-stream s sout)))


;; Dump tool

(defun display-opinion (iid stream)
  (gadgets:with-alist-keys ((:flag :authorname :comment) (warflagger::add-extras-to-opinion (warflagger:opinion-by-id iid) ""))
    (princ iid stream)
    (terpri stream)
    (format stream "~a ~a" flag authorname)
    ;;FIXME: use :clean-comment?
    (when comment
      (print comment stream))))

(defun display-rooturl (rooturl stream)
  (format stream "Article: ~a" rooturl)
  (let ((tinfo (handler-case
                   (wf/ipfs:ipfs-title-info-for-rooturl rooturl)
                 (t (e)
                   (declare (ignore e))
                   (hu:hash (:title "NO TITLE FOUND"))))))
    (alexandria:when-let ((title (gethash :title tinfo)))
      (princ title stream))))

(defun rooturl-discussion-summary (rooturl &optional (stream *standard-output*))
  (display-rooturl rooturl stream)
  (dolist (iid (let ((warflagger:*id-return-type* :iid))
                 (warflagger:opinion-ids-for-rooturl rooturl)))
    (terpri stream)
    (terpri stream)
    (display-opinion iid stream)))

(defun discussion-summary (rooturl &optional (stream *standard-output*))
  (let* ((discroot (warflagger:discussion-root-of rooturl))
         (disctree (nth-value 1 (warflagger:discussion-tree-for-root discroot nil)))
         (disclist (gadgets:flatten-when #'identity disctree)))
    (rooturl-discussion-summary (warflagger:get-rooturl-by-id (car disclist)) stream)
    (dolist (rootid (cdr disclist))
      (terpri stream)
      (terpri stream)
      (terpri stream)
      (rooturl-discussion-summary (warflagger:get-rooturl-by-id rootid) stream))))
