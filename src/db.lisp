(in-package :warflagger)

;;;
;;; db.lisp
;;;
;;; Tools for accessing stored opinions.
;;;
;;;


(defun for-rooturl-mixin (rurl)
  (list :from (list (tabl 'opinion) (tabl 'rooturl))
        :where
        (sql-and (sql-= (colm 'opinion 'rooturl) (colm 'rooturl 'id))
                 (sql-= (colm 'rooturl 'rooturl) (sql-escape rurl)))))

(def-query opinion-ids-for-rooturl (rurl)
  (mapcar
   #'car
   (query-marker
    (merge-query
     (select (colm 'opinion 'id))
     (for-rooturl-mixin rurl)))))

(defun opinion-tree-for-rooturl (rurl)
  ;;FIXME: fails to check for loops and dead trees.
  (tree-by-feature
   (merge-query
    (opinion-ids-for-rooturl rurl)
    (select (colm 'opinion 'target)
            (colm 'opinion 'url)))
   #'second
   :root rurl
   :format #'car
   :identity-func #'third))

(defun opinion-tree-for-target (turl)
  (if (rooturl-p turl)
      (opinion-tree-for-rooturl turl)
      (tree-by-feature
       (merge-query
        (opinion-ids-for-rooturl (get-rooturl-for-url turl))
        (select (colm 'opinion 'target)
                (colm 'opinion 'url)))
       #'second
       :root turl
       :format #'car
       :identity-func #'third)))

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
    (generate-rooturl-warstats target)) ;;Will place the root warstat in the store.
  (gethash target *warstat-store*))

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

(defun get-rooturl-for-url (url)
  "Tries to find the url at the root of a tree of comments. The primary value will always be a guess at the url. The second value tells whether get-rooturl is sure of its result. Get-rooturl does not do network lookups."
  (declare (type string url))
  (block top
    (awhen (get-assoc-by-col (colm 'rooturl 'rooturl) url)
      (return-from top (values (assoc-cdr :rooturl it)
                               ;Evidently some bit rot here
                                    ;(ratify:parse-boolean
                                     (assoc-cdr :rooturl-real it))))
    (awhen (select (colm 'rooturl 'rooturl) (colm 'rooturl 'rooturl-real)
                   :from (list (tabl 'rooturl) (tabl 'opinion))
                   :where
                   (sql-and
                    (sql-= (colm 'opinion 'url) (sql-escape url))
                    (sql-= (colm 'opinion 'rooturl) (colm 'rooturl 'id))))
      (return-from top (values (caar it) (second (car it)))))
                                    ;(ratify:parse-boolean (second (car it))))))
    (values nil nil)))

(defun get-rooturl-id (rurl)
  (aif (get-assoc-by-col (colm 'rooturl 'rooturl) rurl)
       (assoc-cdr :id it)
       (error "Root url not found")))

(defun get-rooturl-by-id (rootid)
  (aif (get-assoc-by-pkey 'rooturl rootid)
       (assoc-cdr :rooturl it)
       (error "Rooturl ID not found")))

(defun rooturl-real-p (id)
  "Means that target chain has been traced back to a non-opinml page, and that
the page text can be found in the cache."
  (boolify (assoc-cdr :rooturl-real (get-assoc-by-pkey 'rooturl id))))

;;;FIXME: What to do if it isn't the real root? Who corrects the rooturl field
;; on all those opinions?
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
           (awhen2 (tryit (get-rooturl-id rurl))
             (return-from top it))
           (awhen (get-rooturl-for-url rurl)
             (return-from top (get-rooturl-id it)))
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
  (map-tuples
   (compose #'keywordize-foreign (curry #'assoc-cdr :type))
   (curry #'assoc-cdr :value)
   (nth-value 1 (get-assoc-by-col (colm 'excerpt 'opinion) eid))))

(defun flag-to-lisp (dbflag)
  (mapcar (compose #'make-keyword #'string-upcase #'to-lisp-case)
          (split-sequence #\  dbflag)))

(defun flag-to-db (lispflag)
  (apply #'format nil "~a ~a"
         (mapcar (compose #'sql-escape #'to-pascal-case #'mkstr) lispflag)))

(defun opinion-from-db-row (row)
  (let ((res (alist->hash row)))
    (with-keys (:id :author :flag :votevalue :target :datestamp :url
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
                 (hash->alist res))))

(defun opinion-from-id (oid)
  (opinion-from-db-row (get-assoc-by-pkey 'opinion oid)))

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
       (list :text
             :status "failure"
             :message "Opinion not found"))))

(defun get-target-id-from-url (url)
  (if-let ((id (rooturl-p url)))
    (values (caar id) :rooturl)
    (when-let ((op (opinion-exists-p url)))
      (values (assoc-cdr :id op) :opinion))))

(defun make-wf-url-for-url (url)
  (multiple-value-bind (id type) (get-target-id-from-url url)
    (case type
      (:rooturl (make-rootid-url id))
      (:opinion (make-opinion-url nil id))
      (otherwise (make-missing-rootid-url url)))))

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
  (map-tuples
   (compose #'keywordize-foreign (curry #'assoc-cdr :type))
   (curry #'assoc-cdr :value)
   (nth-value 1 (get-assoc-by-col (colm 'author 'id) aid))))

;;FIXME: Perhaps in the case of WF user, we shouldn't be looking here for the screen
;; name. That breaks a few assumtions. We don't have userfig here.
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
  (assoc-cdr :wf-user (get-author-data aid)))

;;FIXME: doesn't look like a very reliable way to do things.
(defun author-type (author)
  "For parsing incoming RDF"
  (cond
    ((integerp author) :id)
    ((starts-with "http" author) :homepage)
    ((starts-with "mailto" author) :email)
    (t :string)))

(defun find-author-id (author atype)
  "Find the PKey of the author given any type of author identifier. Mostly for RDF input."
  (case atype
    (:id (when (exists (select (colm 'author 'id)
                               :where (sql-= (colm 'author 'id)
                                             (parse-integer author))))
           (parse-integer author)))
    (:string
     (get-local-user-id author))
    ((or :homepage :email)
     (awhen (select (colm 'id) :from 'author
                    :where (sql-and (sql-= (colm 'type)
                                           (sql-escape atype))
                                    (sql-= (colm 'value)
                                           (sql-escape author)))
                    :flatp t)
       (car it)))))

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


(defun save-opinion-from-user (opinion authorid
                               &key (opinurl #'make-opinion-url))
  "Opinions need to be set up with some stuff."
  (unless (listp opinion)
    (error "Opinion needs to be an alist"))
  (let ((id (next-val "opinion_id_seq")))
    (insert-opinion
     (cons (cons :url (funcall opinurl authorid id)) opinion)
     authorid
     id)
    id))

(defun insert-opinion (opin authorid &optional id)
  "Stores the opinion, represented as an alist, in the database"
  (with-keys (:target :votevalue :datestamp :url :comment :reference :flag)
      (alist->hash opin)
    (let ((id
           (insert-record
            'opinion
            (collecting
                (collect
                    (cons (colm :rooturl) (find/store-root-url target)))
              (dolist (k '(:votevalue :target :datestamp :url))
                (when-let ((field (assoc k opin)))
                  (collect (cons (colm (car field)) (sql-escape (cdr field))))))
              (when id
                (collect (cons (colm :id) id)))
              (collect (cons (colm :flag) (flag-to-db flag)))
              (collect (cons (colm :author) authorid))))))
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
                    (val (not-empty (cdr pair))))
          (insert-records :into 'excerpt
                          :attributes
                          (list (colm :opinion) (colm :type) (colm :value))
                          :values (list id
                                        (to-snake-case (mkstr k))
                                        val))))
      id)))

(defun delete-opinion (oid)
  (let* ((oid (if (numberp oid) oid (parse-integer oid)))
         (rooturl (car (col-from-pkey (colm 'opinion 'rooturl) oid))))
    (with-transaction nil
      (delete-records :from 'comment :where (sql-= (colm 'opinion) oid))
      (delete-records :from 'excerpt :where (sql-= (colm 'opinion) oid))
      (delete-records :from 'reference :where (sql-= (colm 'opinion) oid))
      (delete-records :from 'opinion :where (sql-= (colm 'id) oid)))
    (unless (get-assoc-by-col (colm 'opinion 'rooturl) rooturl)
      (delete-records :from 'rooturl :where (sql-= (colm 'id) rooturl)))))

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
  (select (colm :firstlook) (colm :opinionid)
          :from (tabl :looks)
          :where (sql-and (sql-= (colm :wf_user) (sql-escape user))
                          (sql-= (colm :rootid) rootid))))

(defun set-look (user &key rootid opinionid)
  (let ((rootid (or rootid
                    (assoc-cdr :rooturl
                               (opinion-from-id opinionid)))))
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

