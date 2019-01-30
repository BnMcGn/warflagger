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

(defun make-rootid-url (rid)
  (strcat *base-url* "target/" (princ-to-string rid)))

(defun make-missing-rootid-url (url)
  "Sometimes we want to display a target that is not yet entered into WarFlagger. Make a link that will result in such a display."
  (strcat *base-url* "target/?newurl=" (quri:url-encode url)))

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

(eval-always
  (defmacro if-travis (true-clause false-clause)
   (if (uiop:getenv "TRAVIS") true-clause false-clause)))

(defmacro when-travis (clause) `(if-travis ,clause nil))

(defmacro unless-travis (clause) `(if-travis nil ,clause))

(defun url-p (thing)
  (quri:uri-host (quri:uri thing)))

(defun warstats-url-server (url)
  "Return, if it exists, the warstats URL for a rooturl. For lookups from the browser extension"
  (if (rooturl-p url)
      (list
       :warstats (make-warstats-url (get-rooturl-id url) :warstats)
       :status :success)
      (list :status :failure)))
