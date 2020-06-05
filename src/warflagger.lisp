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
  (strcat *base-url* "new-target/?url=" (quri:url-encode url)))

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

(defun url-p (thing)
  (quri:uri-host (quri:uri thing)))

;;FIXME: This should handle SameThing mirroring and url variants.
;;FIXME: Need to handle other bad URL types than 'unlisted'. Some URLs should be left alone. Non-permalinks
;; non-applicable, maybe some non-web. Need a separate function to handle this.
(defun target-seek-server (url)
  "This service is designed for searches from the browser extension. It lets the browser know if WarFlagger has anything to say about an URL, presumably the one that the user is visiting. It does not return the data, instead it returns the URL for the appropriate warstats."
  (let ((id (caar (rooturl-p url))))
    (hu:plist->hash
     (if id
         (list :status "success"
               :message ""
               :warstats (make-warstats-url id :warstats))
         (list :status "missing"
               :message "URL not listed on server")))))


;; These methods are defined here because local-time and clsql are available here.
(defmethod ps-gadgets:as-ps-data ((item clsql-sys:wall-time))
  (format nil "~a+0000" (clsql:format-time nil item :format :iso8601)))

(defmethod ps-gadgets:as-ps-data ((item local-time:timestamp))
  (local-time:format-timestring
   nil item
   :format '((:year 4) #\- (:month 2) #\- (:day 2) #\T
             (:hour 2) #\: (:min 2) #\: (:sec 2) :gmt-offset-hhmm)))

;;; Tools for managing a change of flag name

(eval-always
  (defun recognized-flag-p (flag)
    (when-let ((i (first-match-index (curry #'eq (car flag)) *flag-category-keys*)))
      (getf (nth i *flag-types-source*) (second flag)))))

(defmacro check-recognized-flag (flag)
  (unless (recognized-flag-p flag)
    (error "Flag not found")))

(defun rename-flag (old new)
  "A function to aid in renaming an existing recogized flag. Renaming should first be done in the constants.lisp file."
  (when (or (recognized-flag-p old)
            (getf *flag-colors* old))
    (error "Old flag is still extant. Please manually replace it in *flag-types-source* and *flag-colors* (see constants.lisp) before running rename-flag"))
  (unless (recognized-flag-p new)
    (error "New flag should be in *flag-types-source*, *flag-colors* and *flag-labels* before running rename-flag"))
  (clsql:update-records
   (tabl 'opinion)
   :av-pairs
   (list (list (colm 'flag) (flag-to-db new)))
   :where (sql-= (colm 'flag) (flag-to-db old)))
  (print "To complete the rename operation, run write-all-warstats. Remember to check the wiki at the source repository and other documentation."))

