(in-package #:cl-user)

(defpackage #:wf/admin
  (:use #:cl #:gadgets #:alexandria #:access)
  )

(in-package #:wf/admin)



;;Thread tool

(defun all-worker-threads ()
  (remove-if-not
   (lambda (x)
     (search "hunchentoot-worker-" (bt:thread-name x)))
   (bt:all-threads)))

(defun clear-worker-threads ()
  (mapcar #'bordeaux-threads:destroy-thread (all-worker-threads)))

;;Posting tools


(defparameter *user* nil)
(defparameter *target* nil)
(defparameter *posting-method* nil)
(defparameter *posting-directory* nil)
(defparameter *opinion-queue* nil)
(defparameter *local-opinion-store* nil)


(defun post (opinion) (%post opinion *posting-method*))
(defgeneric %post (opinion posting-method))
(defmethod %post (opinion (posting-method (eql :directory)))
  (if *posting-directory*
      (warflagger::save-opinion-to-folder opinion *posting-directory*)
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
     opinion)))

(defun comment (comment)
  (post (prep-opinion :flag (list :custodial :blank) comment)))

;;FIXME: always fetches original text, not updates. Also, should we do :clean-comment?
(defun target-text (&optional target *target*)
  "Target is specified by url or iid, generally in the *target* var."
  (if (warflagger:iid-p target)
      (or (and *local-opinion-store*
               (gethash target *local-opinion-store*))
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







