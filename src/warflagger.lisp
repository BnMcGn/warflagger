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

;;FIXME: This should handle SameThing mirroring and url variants.
;;FIXME: Need to handle other bad URL types than 'unlisted'. Some URLs should be left alone. Non-permalinks
;; non-applicable, maybe some non-web. Need a separate function to handle this.
(defun target-seek-server (url)
  "This service is designed for searches from the browser extension. It lets the browser know if WarFlagger has anything to say about an URL, presumably the one that the user is visiting. It does not return the data, instead it returns the URL for the appropriate warstats."
  (let ((id (rooturl-p url)))
    (hu:plist->hash
     (if id
         (list :status "success"
               :message ""
               :warstats (make-warstats-url id :warstats))
         (list :status "missing"
               :message "URL not listed on server")))))


