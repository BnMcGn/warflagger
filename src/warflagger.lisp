;;;; warflagger.lisp

(in-package #:warflagger)

;;;FIXME: Needs to attempt to load externals and test.
;;;FIXME: Should error if url not found anywhere?
;;;FIXME: Is this redundant? Check db.lisp
(defun is-location-opinml? (url)
  (or (exists (select (colm 'id) :from (tabl 'opinion)
		      :where (sql-= (colm 'url) url)))
      (not (and (is-cached url) (old-page-available url)))))

(defun make-user-url (username)
  (strcat *base-url* "u/" username "/"))


;;FIXME: URL should have username, shorter formatting.
;;(defun make-opinion-url (userid opinid)
;;  (format nil "~a~d" (make-user-url userid) opinid))

(defun make-opinion-url (userid opinid)
  (declare (ignore userid))
  (strcat *base-url* "things/thing/opinion/" (princ-to-string opinid)))

;;FIXME: rethink user urls
(defun warflagger-user-from-url (url)
  (aref (nth-value
         1 (ppcre:scan-to-strings (strcat *base-url* "u/([^/]+)/") url))
        0))

(defmacro if-production (true-clause false-clause)
  (if wf/local-settings:*production* true-clause false-clause))

(defmacro when-production (clause)
  (when wf/local-settings:*production* clause))

(defmacro unless-production (clause)
  (unless wf/local-settings:*production* clause))
