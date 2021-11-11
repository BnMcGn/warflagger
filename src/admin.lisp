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



