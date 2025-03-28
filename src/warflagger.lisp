;;;; warflagger.lisp

(in-package #:warflagger)

(defparameter *opinion-store* nil "Allows us to shim opinion-by-id so that we can avoid the database.")

;;FIXME: Don't have an OpinML mime-type. Will want to try a link for OpinML. We might return different
;; things for different request types. Rework opinion urls?
(defun is-location-opinml? (url)
  "Returns T when link is OpinML, returns canonical url when appropriate. Returns nil if url is a root url or is unknown."
  (or (normalize-iid url)
      (and (tt-is-cached url)
           (if-let ((link (url-metadata-points-to-opinml-source? url)))
             link nil))))

;;Complement of get-target-url
(defun iid-or-url (item)
  "Returns an iid if possible, or else an url"
  (or (normalize-iid item)
      (url-p item)))

(defun opinion-for-location (url)
  (when-let ((correct-url (is-location-opinml? url)))
    (opinion-from-db-row (opinion-exists-p (if (stringp correct-url) correct-url url)))))

(defun text-server-dispatcher (url)
  (let ((otest (is-location-opinml? url)))
    (cond
      ((not otest)
       (let ((res (text-server url)))
         (setf (gethash :available res) (wf/ipfs::ipfs-rooturl-exists-p url))
         res))
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
      ((assoc :homepage authdat)
       (strcat *base-url* "author/" (quri:url-encode (assoc-cdr :homepage authdat))))
      ((assoc :email authdat)
       (strcat *base-url* "author/" (quri:url-encode (assoc-cdr :email authdat)))))))

(defun make-opinion-url (opinion)
  (let ((iid (if (iid-p opinion) opinion (assoc-cdr :iid opinion))))
    (strcat *base-url* "o/" iid)))

(defun make-rootid-url (rid)
  (strcat *base-url* "target/" (princ-to-string rid)))

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
  (when thing
    (quri:uri-host (quri:uri thing))))

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
               ;;FIXME: Warstats should come from ipfs. Obsolete
               :warstats nil ;(make-warstats-url id :warstats)
               :opinions nil) ;(make-warstats-url id :opinions))
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
    (if-let ((i (first-match-index (curry #'eq (car flag)) *flag-category-keys*)))
      (getf (nth i *flag-types-source*) (second flag))
      (when (and (eq (car flag) :statements) (eq (second flag) :evidence))
        flag))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Opinion posting central
;;
;; - should this be in another file?
;;   - needs access to all applicable backends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;FIXME: Vote-value will probably go away, at least as an opinion form field.
(defun default-votevalue (flag-pair)
  (destructuring-bind (cat flag) flag-pair
    (if (equal flag :evidence)
        (cond
          ((equal cat :positive) 1)
          ((equal cat :negative) -1)
          (t (error "Invalid evidence flag category")))
        (getf *default-vote* flag 0))))

(defun is-author-initialized (author)
  (get-local-user-id author))

(defun initialize-author (&rest atypes-and-values)
  (apply #'insert-new-author atypes-and-values))

(defun save-opinion (opinion local-author &key post authorid)
  (let* ((authorid (or authorid (get-local-user-id local-author)))
         (author-url (make-author-url authorid))
         (datestamp (clsql:get-time))
         (votevalue (or (assoc-cdr :votevalue opinion)
                         (default-votevalue (assoc-cdr :flag opinion))))
         (opinion (cons (cons :votevalue votevalue) opinion))
         (strop (serialize-opinion opinion :author author-url :created datestamp))
         (iid (iid-from-ipfs-hash (ipfs-data-hash strop)))
         (opinion (cons (cons :iid iid) opinion))
         (opinion (cons (cons :datestamp datestamp) opinion))
         (opinion (cons (cons :url (make-opinion-url opinion)) opinion))
         (opinion (cons (cons :target (iid-or-url (assoc-cdr :target opinion))) opinion))
         (id (insert-opinion opinion authorid))
         (opinion (opinion-by-id id)))
    (when (functionp post)
      (launch-task post opinion))
    opinion))

(defparameter *kernel* nil)
(defparameter *channel* nil)

;;FIXME: Single worker until we know that threads won't tread on each other's work
(defun launch-task (func param)
  (unless *kernel*
    (setf *kernel* (lparallel:make-kernel 1)))
  (let ((lparallel:*kernel* *kernel*))
    (unless *channel*
      (setf *channel* (lparallel:make-channel)))
    (lparallel:submit-task *channel* (lambda () (funcall func param)))))



