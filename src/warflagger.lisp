;;;; warflagger.lisp

(in-package #:warflagger)

(defun extract-opinml-meta-from-html (stream)
  (let ((nodes (lquery:$ (initialize stream) "[property^=opinml:]")))
    (loop for node across nodes
          for attribs = (plump:attributes node)
          collect (cons (gethash "property" attribs) (gethash "content" attribs)))))

(defun file-points-to-opinml-source? (stream)
  (let ((res (extract-opinml-meta-from-html stream)))
    (and (assoc "opinml:opinion" res :test #'equal)
         (< (length (gadgets:assoc-all "opinml:opinion" res :test #'equal)) 2)
         (assoc-cdr "opinml:opinion" res :test #'equal))))

;;FIXME: Bit of a hack. Can we do away with this?
(defun known-translatable-opinurl (url)
  (sequence-starts-with url (strcat *base-url* "opinion-page/")))

(defun translate-opinurl (url)
  (ppcre:regex-replace "opinion-page" url "things/thing/opinion"))

;;FIXME: Don't have an OpinML mime-type. Will want to try a link for OpinML. We might return different
;; things for different request types. Rework opinion urls?
;;FIXME: Will want to check IPFS/whatever. Don't have that yet.
(defun is-location-opinml? (url)
  "Returns T when link is OpinML, returns canonical url when appropriate. Returns nil if url is a root url or is unknown."
  (cond
    ((opinion-exists-p url) t)
    ((known-translatable-opinurl url) (translate-opinurl url))
    ((and (is-cached url) (old-page-available url))
     (if-let ((link (file-points-to-opinml-source? (grab-page url :update nil))))
       link nil))
    (t nil)))

(defun opinion-for-location (url)
  (when-let ((correct-url (is-location-opinml? url)))
    (opinion-from-db-row (opinion-exists-p (if (stringp correct-url) correct-url url)))))

(defun text-server-dispatcher (url)
  (let ((otest (is-location-opinml? url)))
    (cond
      ((not otest) (wf/text-extract:text-server url))
      ((eq otest t) (warflagger:opinion-text-server url))
      ((stringp otest)
       (let ((res (opinion-text-server otest)))
         (setf (gethash :status res) "redirect")
         (setf (gethash :url res) otest)
         res)))))

(defun make-author-url (authid)
  (let ((authdat (get-author-data authid)))
    (cond
      ((assoc :screen-name authdat)
       (strcat *base-url* "u/" (quri:url-encode (assoc-cdr :screen-name authdat))))
      ((assoc :display-name authdat)
       (strcat *base-url* "u/" (quri:url-encode (assoc-cdr :display-name authdat))))
      ((assoc :homepage authdat)
       (strcat *base-url* "author/" (quri:url-encode (assoc-cdr :homepage authdat))))
      ((assoc :email authdat)
       (strcat *base-url* "author/" (quri:url-encode (assoc-cdr :email authdat)))))))

#|
(defun make-opinion-url (userid opinid)
  (declare (ignore userid))
  (strcat *base-url* "things/thing/opinion/" (princ-to-string opinid)))
|#

(defun make-opinion-url (opinion)
  (if-let ((iid (assoc :iid opinion)))
    (strcat *base-url* "o/" (cdr iid))
    (strcat *base-url* "things/thing/opinion/" (princ-to-string (assoc-cdr :id opinion)))))

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
               :warstats (make-warstats-url id :warstats)
               :opinions (make-warstats-url id :opinions))
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

(defun known-flags ()
  (cl-utilities:collecting
    (loop for cat in *flag-category-keys*
          for flags in *flag-types-source*
          do (gadgets:do-window (pair flags :size 2 :step 2)
               (cl-utilities:collect (list cat (car pair)))))))

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

