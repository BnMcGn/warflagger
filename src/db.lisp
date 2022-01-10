(in-package :warflagger)

;;;
;;; db.lisp
;;;
;;; Tools for accessing stored opinions.
;;;
;;;
(define-condition not-found (error) ())

(defun for-rooturl-mixin (rurl)
  (list :from (list (tabl 'opinion) (tabl 'rooturl))
        :where
        (sql-and (sql-= (colm 'opinion 'rooturl) (colm 'rooturl 'id))
                 (sql-= (colm 'rooturl 'rooturl) (sql-escape rurl)))))

;;Other options: :iid, maybe :url
;;Used for things that return an opinion id. Sometimes we would rather have iids or something else.
(defvar *id-return-type* :id)

(def-query opinion-ids-for-rooturl (rurl)
  (mapcar
   #'car
   (query-marker
    (merge-query
     (select (cond ((eq *id-return-type* :id) (colm 'opinion 'id))
                   ((eq *id-return-type* :iid) (colm 'opinion 'iid))))
     (for-rooturl-mixin rurl)))))

(def-query reference-opinion-ids-for-rooturl (rurl)
  (mapcar
   #'car
   (query-marker
    (merge-query
     (opinion-ids-for-rooturl rurl)
     (list :from (tabl 'reference)
           :where (sql-= (colm 'reference 'opinion) (colm 'opinion 'id)))))))

(defun opinion-tree-for-rooturl (rurl)
  ;;FIXME: fails to check for loops and dead trees.
  (proto:tree-by-parent
   (merge-query
    (opinion-ids-for-rooturl rurl)
    (select (colm 'opinion 'target)
            (colm 'opinion 'url))
    (order-by-mixin (colm 'opinion 'datestamp) :asc))
   #'second
   :root rurl
   :format #'car
   :identity-func #'third))

(defun opinion-tree-for-target (turl)
  (if (rooturl-p turl)
      (opinion-tree-for-rooturl turl)
      (proto:tree-by-parent
       (merge-query
        (opinion-ids-for-rooturl (get-rooturl-for-url turl))
        (select (colm 'opinion 'target)
                (colm 'opinion 'url)))
       #'second
       :root turl
       :format #'car
       :identity-func #'third)))

(defun tree-address (opinid)
  (let* ((opin (opinion-by-id opinid))
         (tree (opinion-tree-for-rooturl (get-rooturl-by-id (assoc-cdr :rootid opin)))))
    (labels ((proc (tree address)
               (let ((branches nil))
                 (dolist (node tree)
                   (when (eq (car node) opinid)
                     (return-from proc (cons opinid address)))
                   (when (< 1 (length node))
                     (push node branches)))
                 (dolist (branch branches)
                   (when-let ((res (proc (cdr branch) (cons (car branch) address))))
                     (return-from proc res))))))
      (nreverse (proc tree nil)))))

(defun target-replies (turl)
  "Return a list of immediate replies to a target. Replies are opinids."
  (mapcar #'car (opinion-tree-for-target turl)))

(defun controversial-p (url)
  (exists
   (unexecuted
     (select (colm 'id) :from (tabl 'opinion)
             :where (sql-and (sql-= (sql-escape url) (colm 'target))
                             (sql-> 0 (colm 'votevalue)))))))

(def-query replies-by-date (&rest rootid/s)
  (query-marker
   (select 'id 'datestamp
           :from (tabl 'opinion)
           :where (in-or-equal (colm 'opinion 'rooturl) rootid/s)
           :order-by (list (list (colm 'opinion 'datestamp) :desc)))))

;;FIXME: This will get too large as site grows. Maybe it should be calc'ed client-side?
;;FIXME: Obsolete? will be storing as static json.
(defparameter *warstat-store* (make-hash-table :test #'equal))

(defun warstats-for-target (target)
  (unless (gethash target *warstat-store*)
    ;;Will place the root warstat in the store.
    (generate-rooturl-warstats (get-rooturl-for-url target)))
  (gethash target *warstat-store*))

(defun opinion-reference (opinion)
  (when-let* ((ref (assoc :reference opinion))
              (ref (and (listp ref) (cdr ref))))
    (and (stringp ref) (not-empty ref) ref)))

;;;
;;; Rooturl utilities
;;;
;;; Rooturl has difficulties:
;;; - A url is a rooturl if it is being discussed by opinml comments, and it
;;;   is not itself an opinml comment on something else.
;;; - Every opinml discussion should have a single rooturl at top of the tree.
;;; - Except that: Implementation of SameThing has not been considered.
;;; -- Might need a sort of virtual rooturl for grouping discussions.
;;; -- Adopted discussions: AlreadyAnswered?
;;; -- Disagreement over SameThing: People may have specific cause to refer to
;;; a shadowed rooturl. Eg, when SameThing is used for two editions of an item.
;;; Most discussion of two editions should be grouped, but occasionally
;;; someone might want to comment on differences of edition.
;;; - We can't tell whether an external url is a root unless we can load it.


(defun rooturl-p (url)
  "Warning: this function does not provide a definitive answer."
  (select (colm 'id) :from (tabl 'rooturl)
          :where (sql-= (colm 'rooturl) (sql-escape url))))

(defun all-rooturls ()
  (get-column (tabl 'rooturl) (colm 'rooturl)))

(defun all-proper-references ()
  (get-column (tabl 'reference) (colm 'reference)))

(defun get-rooturl-for-url (url)
  "Tries to find the url at the root of a tree of comments. The primary value will always be a guess at the url. The second value tells whether get-rooturl is sure of its result. Get-rooturl does not do network lookups. The rooturl must exist in the database as a rooturl."
  (declare (type string url))
  (let ((opinfo (is-location-opinml? url)))
    (if opinfo
        (if (stringp opinfo)
            (get-rooturl-for-url opinfo)
            (let* ((oid (get-target-id-from-url url))
                   (opinion (opinion-by-id oid)))
              (get-rooturl-for-url (assoc-cdr :rooturl opinion))))
        (when (rooturl-p url)
          url))))

(defun get-rooturl-id (rurl)
  (if-let ((rec (get-assoc-by-col (colm 'rooturl 'rooturl) rurl)))
       (assoc-cdr :id rec)
       (error "Root url not found")))

(defun get-rooturl-by-id (rootid)
  (if-let ((rec (get-assoc-by-pkey 'rooturl rootid)))
    (values (assoc-cdr :rooturl rec) (assoc-cdr :rooturl-real rec))
       (error "Rooturl ID not found")))

;;;FIXME: Each opinion should record its rooturl. Needs access to targetted opinion, therefore.
;;; this will save hunting down the rooturl and all of the uncertainty code
(defun rooturl-real-p (id)
  "Means that target chain has been traced back to a non-opinml page, and that
the page text can be found in the cache."
  (nth-value 1 (get-rooturl-by-id id)))

;;;FIXME: What to do if it isn't the real root? Who corrects the rooturl field
;; on all those opinions?
;;; FIXME: Should this check for opinml meta tags?
(defun make-rooturl-real (root-id)
  (let ((url (get-rooturl-by-id root-id)))
    (or (rooturl-real-p root-id)
        (and (is-cached url) (old-page-available url)
             (update-record 'rooturl  root-id `((,(colm :rooturl-real) . t))))
        ;;FIXME: Implement offsite opinml hunting
        t))) ;Why the trailing t? Who receives?

(defun find/store-root-url (rurl)
  (let ((rootid
          (block top
            (multiple-value-bind (val sig) (tryit (get-rooturl-id rurl))
              (when sig (return-from top val)))
            (when-let ((val (get-rooturl-for-url rurl)))
              (return-from top (get-rooturl-id val)))
            (insert-record
             'rooturl
             `((,(colm :rooturl) . ,rurl) (,(colm :rooturl-real) . false))))))
    (make-rooturl-real rootid) ;worth a try: might have been fetched by opinion page.
    rootid))

;;;
;;; Other accessors
;;;

(defun opinion-exists-p (url)
  (when url
    (get-assoc-by-col (colm 'opinion 'url) url)))

(defun get-excerpt-data (eid)
  (ret res (proto:map-tuples
            (compose #'proto:keywordize-foreign (curry #'assoc-cdr :type))
            (curry #'assoc-cdr :value)
            (nth-value 1 (get-assoc-by-col (colm 'excerpt 'opinion) eid)))
    (when-let ((offset (assoc :excerpt-offset res)))
      (setf (cdr offset) (parse-integer (cdr offset))))))

(defun flag-to-lisp (dbflag)
  (mapcar (compose #'make-keyword #'string-upcase #'to-lisp-case)
          (cl-utilities:split-sequence #\  dbflag)))

(defun flag-to-db (lispflag)
  (apply #'format nil "~a ~a"
         (mapcar (compose #'sql-escape #'to-pascal-case #'mkstr) lispflag)))

(defun opinion-from-db-row (row)
  (let ((res (hu:alist->hash row)))
    (hu:with-keys (:id :author :flag :votevalue :target :datestamp :url
                    :comment :reference :authorname :author-id)
        res
      (let ((authdata (get-author-data author)))
        (setf author-id author)
        (setf author (author-identification-from-row authdata))
        (setf authorname (author-representation-from-row authdata)))
      (setf flag (flag-to-lisp flag))
      (setf comment (car (col-from-pkey (colm 'comment 'comment) id)))
      (setf reference (car (col-from-pkey (colm 'reference 'reference) id))))
    (concatenate 'list
                 (get-excerpt-data (gethash :id res))
                 (hu:hash->alist res))))

(defun add-extras-to-opinion (opinion text)
  (when (assoc :excerpt opinion)
    (unless (and text
                 (or (and (stringp text) (not-empty text))
                     (and (tdat-p text) (not-empty (tdat-text text)))))
      (warn "Opinion has excerpt but target text is empty"))
    (let ((textdata (typecase text
                      (tdat text)
                      (string (create-textdata text))))
          (text (typecase text
                  (tdat (tdat-text text))
                  (string text))))
        (multiple-value-bind (index length)
            (find-excerpt-position textdata (assoc-cdr :excerpt opinion)
                                   (or (assoc-cdr :excerpt-offset opinion) 0))
          (if index
            (let ((econtext (excerpt-context text index length)))
             (push (list :text-position index length) opinion)
             (push (cons :leading (getf econtext :leading)) opinion)
              (push (cons :trailing (getf econtext :trailing)) opinion))
            (push (list :text-position nil nil) opinion)))))
  (unless (assoc :tree-address opinion)
    (push (cons :tree-address (tree-address (assoc-cdr :id opinion))) opinion))
  opinion)

(defparameter *opinion-store* nil "Allows us to shim opinion-by-id so that we can avoid the database.")

;;FIXME: we might want to dispatch on string vs. integer here.
(defun opinion-by-id (oid &key extra text)
  (when (and text (not extra))
    (error "Text keyword is not valid without :extra set to T"))
  (let ((opinion (if-let  (res (and *opinion-store* (gethash oid *opinion-store*)))
                   res
                   (opinion-from-db-row
                    (or (if (stringp oid)
                            (get-assoc-by-col (colm 'opinion 'iid) oid)
                            (get-assoc-by-pkey 'opinion oid))
                        (error 'not-found))))))
    ;;FIXME: will go away when db goes
    (when (integerp (assoc-cdr :rooturl opinion))
      (push (cons :rootid (assoc-cdr :rooturl opinion)) opinion)
      (setf (cdr (assoc :rooturl opinion)) (get-rooturl-by-id (assoc-cdr :rootid opinion))))
    (if extra
        (add-extras-to-opinion opinion (or text (get-target-text oid)))
        opinion)))

(def-query get-opinion-peers (o-url)
  (mapcar
   #'car
   (query-marker
    (select (colm 'id) :from 'opinion
                       :where
                       (sql-= (colm 'target)
                              (sql-query (colm 'target)
                                         :from (tabl 'opinion)
                                         :where (sql-= (colm 'url)
                                                       (sql-escape o-url))))))))

(defun opinion-text-server (url)
  (hu:plist->hash
   (if (opinion-exists-p url)
       (let ((res (select (colm 'comment 'comment)
                          :from (list (tabl 'opinion) (tabl 'comment))
                          :where (sql-and (sql-= (colm 'opinion 'url) url)
                                          (sql-= (colm 'opinion 'id)
                                                 (colm 'comment 'opinion))))))
         (if res
             (list :text (caar res)
                   :status "success"
                   :message "")
             (list :text ""
                   :status "success"
                   :message "[No comment text]")))
       (list :text ""
             :status "failure"
             :message "Opinion not found"))))

(defun get-target-url (identifier)
  (if (iid-p identifier)
      (assoc-cdr :url (opinion-by-id identifier))
      identifier))

(defun get-target-id-from-url (url)
  (if-let ((id (rooturl-p url)))
    (values (caar id) :rooturl)
    (when-let ((op (opinion-exists-p url)))
      (values (if (and (assoc :iid op) (assoc-cdr :iid op)) (assoc-cdr :iid op) (assoc-cdr :id op))
              :opinion))))

(defun get-target-id (opinid)
  (get-target-id-from-url (assoc-cdr :target (opinion-by-id opinid))))

(defgeneric get-target-text (opinish))
(defmethod get-target-text ((opinid integer))
  (gethash :text (text-server-dispatcher (assoc-cdr :target (opinion-by-id opinid)))))

(defun make-wf-url-for-url (url)
  (multiple-value-bind (id type) (get-target-id-from-url url)
    (case type
      (:rooturl (make-rootid-url id))
      (:opinion (make-opinion-url (opinion-by-id id)))
      (otherwise
       (if (stringp (is-location-opinml? url))
         url
         (make-missing-rootid-url url))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users and Authors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;; Difficulties:
;;
;; - Users and authors are not always the same thing.
;; - Not all authors will have a login at warflagger.
;; - If a user logs in to warflagger and claims an email or web address as his own,
;; we don't have a way to verify that claim. Therefore we allow a user to possibly
;; lay claim to someone else's authorship.
;; - There is currently a disjunction between OpinML, Warflagger and OpenID
;; user attributes.

(defun get-author-data (aid)
  (declare (type integer aid))
  (proto:map-tuples
   (compose #'proto:keywordize-foreign (curry #'assoc-cdr :type))
   (curry #'assoc-cdr :value)
   (nth-value 1 (get-assoc-by-col (colm 'author 'id) aid))))

;;FIXME: Perhaps in the case of WF user, we shouldn't be looking here for the screen
;; name. That breaks a few assumptions. We don't have userfig here.
(defun author-representation-from-row (data)
  (when-let ((res (assoc-or '(:screen-name :display-name :homepage :email) data)))
    (values (cdr res) (car res))))

(defun get-author-representation (aid)
  "How the author should look on the website."
  (author-representation-from-row (get-author-data aid)))

;;FIXME: doesn't account for local users. Perhaps this should be done at webhax-user level
(defun author-identification-from-row (data)
  (when-let ((res (assoc-or '(:homepage :email) data)))
    (values (cdr res) (car res))))

(defun get-author-identification (aid)
  (author-identification-from-row (get-author-data aid)))

(defun get-local-user-id (wf-user)
  "Maybe should be called get-id-from-local-user"
  (let ((res (select (colm 'author 'id) :from (tabl 'author)
                     :where
                     (sql-and
                      (sql-= (colm 'author 'type) "wf_user")
                      (sql-= (colm 'author 'value) (sql-escape wf-user)))
                     :flatp t)))
    (when (< 1 (length res))
      (error "Integrity Breach in the author table!"))
    (car res)))

(defun get-local-user-from-id (aid)
  (declare (type integer aid))
  (when-let*
      ((data (get-author-data aid))
       (luser (assoc-all :wf-user data)))
    (when (> 1 (length luser))
      (error "Multiple :wf-user fields for id"))
    (car luser)))

;;FIXME: doesn't look like a very reliable way to do things.
(defun author-type (author)
  "For parsing incoming RDF"
  (cond
    ((integerp author) :id)
    ((starts-with "http" author) :homepage)
    ((starts-with "mailto" author) :email)
    (t :string)))

(defun find-author-id (author)
  (let ((author (gadgets:string-unless-number author)))
    (if (integerp author)
        (when (get-assoc-by-col (colm 'author 'id) author)
          author)
        (take-one
         (select (colm 'id) :from 'author
           :where (sql-and (sql-in
                            (colm 'type)
                            ;; Limit the types that the public can search
                            (mapcar #'to-snake-case '(:homepage :email :display-name :screen-name)))
                           (sql-= (colm 'value) author)))))))

(defun insert-new-author (&rest atypes-and-values)
  (let ((aid (next-val "author_id_seq")))
    (do-window ((atype value) atypes-and-values :step 2)
      (insert-records
       :into 'author
       :attributes (list (colm :id) (colm :type) (colm :value))
       :values (list aid (kebab:to-snake-case (mkstr atype)) (sql-escape value))))
    aid))

(def-query author-lister (&key limit offset order-by)
  (mapcar #'car
          (query-marker
           (merge-query
            (select (colm 'id) :from (tabl 'author) :distinct t)
            (order-by-mixin order-by)
            (limit-mixin limit offset)))))

(def-query user-lister (&key limit offset order-by)
  (mapcar #'car
          (query-marker
           (merge-query
            (select (colm 'value) :from (tabl 'author)
                                  :where (sql-= (colm 'type) "wf_user"))
            (order-by-mixin order-by)
            (limit-mixin limit offset)))))

(defun attach-wf-user-to-author (userid authid)
  "Temporary utility to allow a given login to post as a given author. Eventually users maybe should be allowed multiple user names, since it must be assumed that they will create them anyways. For now this is admin only."
  (when (eq userid authid)
    (error "Userid and authid are already equal"))
  (let ((locuse (get-local-user-from-id userid))
        (authdat (get-author-data authid)))
    (when (assoc-cdr :wf-user authdat)
      (error "Author already has a :wf-user"))
    (unless locuse
      (error "No :wf-user found for id"))
    (clsql:update-records
     (tabl 'author)
     :av-pairs
     (list (list (colm 'id) authid))
     :where (sql-and (sql-= (colm 'type) "wf_user")
                     (sql-= (colm 'id) userid)))))

(def-query author-rooturls (authid)
  (mapcar #'car
          (query-marker
           (select (colm 'rooturl 'rooturl)
                   :from (list (tabl 'rooturl) (tabl 'opinion))
                   :where (sql-and (sql-= (colm 'opinion 'rooturl) (colm 'rooturl 'id))
                                   (sql-= (colm 'opinion 'author) authid))
                   :distinct t))))

;;NOTE: Won't find references from comments
(def-query author-references (authid)
  (mapcar #'car
          (query-marker
           (select (colm 'reference 'reference)
                   :from (list (tabl 'reference) (tabl 'opinion))
                   :where (sql-and (sql-= (colm 'reference 'opinion) (colm 'opinion 'id))
                                   (sql-= (colm 'opinion 'author) authid))
                   :distinct t))))

;;A list of replies to a given author
(def-query author-replies (authid)
  (mapcar #'car
          (query-marker
           (clsql:select
            (case *id-return-type*
              (:id (colm :id))
              (:iid (colm :iid))
              (otherwise (error "not implemented")))
            :from (tabl :opinion)
            :where
            (clsql:sql-and
             (clsql:sql-in
              (colm :target)
              (clsql:sql-query
               (colm :opinion :url)
               :from (tabl :opinion)
               :where (clsql:sql-= (colm :author) authid)))
             (clsql:sql-not
              (clsql:sql-= (colm :author) authid)))))))

(def-query author-opinions (authid)
  (mapcar #'car
          (query-marker
           (select
            (case *id-return-type*
              (:id (colm :id))
              (:iid (colm :iid))
              (otherwise (error "not implemented")))
            :from (tabl :opinion)
            :where (sql-= (colm :author) authid)))))


;;FIXME: broken
(def-query author-reply-references (authid)
  (mapcar
   #'car
   (query-marker
    (select (colm 'reference 'reference)
            :from (list (tabl 'reference) (tabl 'opinion))
            :where (in-subquery (colm 'opinion 'id) (author-replies authid))))))

(defun author-urls (author)
  "A (comprehensive, eventually) list of urls that the author has interacted with."
  (let ((*id-return-type* :iid)
        (authid (find-author-id author)))
    (list
     :rooturls (author-rooturls authid)
     ;;FIXME: Want inline references as well. Need IPFS read for that, probably
     :references (author-references authid)
     :replies
     (cl-utilities:collecting
       (dolist (opid (author-replies authid))
         (when-let ((ref (assoc-cdr :reference (opinion-by-id opid))))
           (cl-utilities:collect ref)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Opinion insert and delete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;FIXME: Obsolete. Can probably remove.
(defun save-opinion-from-user (opinion authorid
                               &key (opinurl #'make-opinion-url))
  "Opinions need to be set up with some stuff."
  (unless (listp opinion)
    (error "Opinion needs to be an alist"))
  (let* ((id (next-val "opinion_id_seq"))
         (url (funcall opinurl id)))
    (insert-opinion
     (cons (cons :url url) opinion)
     authorid
     id)
    (values id url)))

(defun insert-opinion (opin authorid &optional id)
  "Stores the opinion, represented as an alist, in the database"
  (hu:with-keys (:target :votevalue :datestamp :url :comment :reference :flag)
      (hu:alist->hash opin)
    (let ((id
           (insert-record
            'opinion
            (cl-utilities:collecting
              ;;FIXME: RootURL should be stored in targetted opin, passed in by client.
                (cl-utilities:collect
                    (cons (colm :rooturl) (find/store-root-url target)))
              (dolist (k '(:votevalue :target :datestamp :url :iid))
                (when-let ((field (assoc k opin)))
                  (cl-utilities:collect (cons (colm (car field)) (sql-escape (cdr field))))))
              (when id
                (cl-utilities:collect (cons (colm :id) id)))
              (cl-utilities:collect (cons (colm :flag) (flag-to-db flag)))
              (cl-utilities:collect (cons (colm :author) authorid))))))
      (when (and (stringp comment) (not-empty comment))
        (insert-records :into 'comment :attributes
                        (list (colm :opinion) (colm :comment))
                        :values (list id comment)))
      (when (and (stringp reference) (not-empty reference))
        (insert-records :into 'reference :attributes
                        (list (colm :opinion) (colm :reference))
                        :values (list id reference)))
      (dolist (k '(:excerpt :excerpt-offset :time-excerpt :excerpt-length))
        (when-let* ((pair (assoc k opin))
                    (val (typecase (cdr pair)
                           (sequence (not-empty (cdr pair)))
                           (integer (and (positive-real-p (cdr pair)) (cdr pair))))))
          (insert-records :into 'excerpt
                          :attributes
                          (list (colm :opinion) (colm :type) (colm :value))
                          :values (list id
                                        (to-snake-case (mkstr k))
                                        val))))
      id)))

;; Opinion will still appear on site until write-all-rootid-warstats is run for the root target.
(defun delete-opinion (oid &key (remove-rooturl t))
  (let* ((oid (if (numberp oid) oid (parse-integer oid)))
         (rooturl (car (col-from-pkey (colm 'opinion 'rooturl) oid))))
    (with-transaction nil
      (delete-records :from 'comment :where (sql-= (colm 'opinion) oid))
      (delete-records :from 'excerpt :where (sql-= (colm 'opinion) oid))
      (delete-records :from 'reference :where (sql-= (colm 'opinion) oid))
      (delete-records :from 'opinion :where (sql-= (colm 'id) oid)))
    (when remove-rooturl
      (unless (get-assoc-by-col (colm 'opinion 'rooturl) rooturl)
       (delete-records :from 'rooturl :where (sql-= (colm 'id) rooturl))))))

(defun opinion-may-exist (opin authorid)
  "For auto-entered opinions, try to give some warning that a double entry is about to happen."
  (let ((target (assoc-cdr :target opin))
        (flag (flag-to-db (assoc-cdr :flag opin))))
    (select 'id
            :from 'opinion
            :where (sql-and
                    (sql-= target (colm 'target))
                    (sql-= flag (colm 'flag))
                    (sql-= authorid (colm 'author))))))



;;;;;;;;;;;;;;
;; Looks stuff
;;;;;;;;;;;;;;

;; Looks table is used to store whether a user has read an item.
;; Columns: firstlook userid rootid opinionid
;; If the opinionid is null, the record refers to a view of the rootid item

(defun get-looks (user rootid)
  (declare (type integer rootid))
  (hu:collecting-hash-table (:mode :replace)
    (loop for (k v) in (select (colm :opinionid) (colm :firstlook)
                               :from (tabl :looks)
                               :where (sql-and (sql-= (colm :wf_user) (sql-escape user))
                                               (sql-= (colm :rootid) rootid)))
          do (hu:collect (or k :root) v))))

(defun set-look (user &key rootid opinionid)
  (let ((rootid (or rootid
                    (assoc-cdr :rooturl
                               (opinion-by-id opinionid)))))
    (unless (integerp rootid)
      (error "Unable to find rootid"))
    (when (emptyp (select (colm :wf_user) :from (tabl :looks)
                          :where
                          (sql-and (sql-= (colm :wf_user) (sql-escape user))
                                   (sql-= (colm :rootid) rootid)
                                   (sql-equal/null (colm :opinionid) opinionid))))
      (insert-records :into 'looks :attributes
        (list (colm :firstlook) (colm :wf_user)
         (colm :rootid) (colm :opinionid))
        :values
        (list (clsql:get-time)
         (sql-escape user) rootid opinionid)))))

(defun get-rooturl-looks (rootid)
  (declare (type integer rootid))
  (select (colm :wf_user) (colm :firstlook)
          :from (tabl :looks)
          :where (sql-and (sql-is (colm :opinionid) nil)
                          (sql-= (colm :rootid) rootid))))

(defun get-opinion-looks (opinid)
  (declare (type integer opinid))
  (select (colm :wf_user) (colm :firstlook)
          :from (tabl :looks)
          :where (sql-= (colm :opinionid) opinid)))

(defun get-target-looks (target-url)
  (multiple-value-bind (id type) (get-target-id-from-url target-url)
    (case type
      (:rooturl (get-rooturl-looks id))
      (:opinion (get-opinion-looks id))
      (otherwise (error "Not a RootURL or an opinion")))))

