(in-package :warflagger)

;;;
;;; db.lisp
;;;
;;; Tools for accessing stored opinions.
;;;
;;;


(def-query opinion-ids-for-rooturl (rurl)
  (mapcar 
   #'car
   (query-marker
    (select (colm 'opinion 'id) :from (list (tabl 'opinion) (tabl 'rooturl))
	    :where 
	    (sql-and (sql-= (colm 'opinion 'rooturl) 
			    (colm 'rooturl 'id))
		     (sql-= (colm 'rooturl 'rooturl)
			    rurl))))))


(defun opinion-tree-for-rooturl (rurl)
  ;FIXME: fails to check for loops and dead trees.
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

(defun controversial-p (url)
  (exists
   (unexecuted
     (select (colm 'id) :from (tabl 'opinion)
	     :where (sql-and (sql-= url (colm 'target))
			     (sql-> 0 (colm 'votevalue)))))))

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
  (exists (select (colm 'id) :from (tabl 'rooturl)
		  :where (sql-= (colm 'rooturl) url))))

(defun all-rooturls ()
  (get-column (tabl 'rooturl) (colm 'rooturl)))

(defun get-rooturl-for-url (url)
  "Tries to find the url at the root of a tree of comments. The primary value will always be a guess at the url. The second value tells whether get-rooturl is sure of its result. Get-rooturl does not do network lookups."
  (block top
    (awhen (get-assoc-by-col (colm 'rooturl 'rooturl) url)
      (return-from top (values (assoc-cdr :rooturl it) 
		      (ratify:parse-boolean (assoc-cdr :rooturl-real it)))))
    (awhen (select (colm 'rooturl 'rooturl) (colm 'rooturl 'rooturl-real)
		   :from (list (tabl 'rooturl) (tabl 'opinion))
		   :where 
		   (sql-and 
		    (sql-= (colm 'opinion 'url) url)
		    (sql-= (colm 'opinion 'rooturl) (colm 'rooturl 'id))))
      (return-from top (values (caar it) 
			       (ratify:parse-boolean (second (car it))))))
    (values nil nil)))

(defun get-rooturl-id (rurl)
  (aif (get-assoc-by-col (colm 'rooturl 'rooturl) rurl)
       (assoc-cdr :id it)
       (error "Root url not found")))

(defun get-rooturl-by-id (id)
  (aif (get-assoc-by-pkey 'rooturl id)
       (assoc-cdr :rooturl it)
       (error "Rooturl ID not found")))

(defun rooturl-real-p (id)
  (boolify (assoc-cdr :rooturl-real (get-assoc-by-pkey 'rooturl id))))

(defun make-rooturl-real (root-id)
  (let ((url (get-rooturl-by-id root-id)))
    (or (rooturl-real-p url)
	(and (is-cached url) (old-page-available url)
	     (update-record 'rooturl  root-id `((:rooturl-real . t))))
	;;FIXME: Implement offsite opinml hunting
	t)))

(defun find/store-root-url (rurl)
  (block top
    (awhen2 (tryit (get-rooturl-id rurl))
      (return-from top it))
    (awhen (get-rooturl-for-url rurl)
      (return-from top (get-rooturl-id it)))
    (insert-record 'rooturl `((:rooturl . ,rurl) (:rooturl-real . nil)))))

;;;
;;; Other accessors
;;;

(defun opinion-exists-p (url)
  (get-assoc-by-col (colm 'opinion 'url) url))

(defun get-author-data (aid)
  (map-tuples 
   (compose #'keywordize-foreign (curry #'assoc-cdr :type))
   (curry #'assoc-cdr :value)
   (nth-value 1 (get-assoc-by-col (colm 'author 'id) aid))))

(defun get-excerpt-data (eid)
  (map-tuples
   (compose #'keywordize-foreign (curry #'assoc-cdr :type))
   (curry #'assoc-cdr :value)
   (nth-value 1 (get-assoc-by-col (colm 'excerpt 'opinion) eid))))

(defun opinion-from-db-row (row)
  (let ((res (alist->hash row)))
    (with-keys (:id :author :flag :votevalue :target :datestamp :url
		    :comment :reference)
	res
      (let ((authdata (get-author-data author)))
	(setf author (cdr (or (assoc :wf-user authdata) 
			      (assoc :homepage authdata)
			      (assoc :email authdata) 
			      (error "Can't find author")))))
      (setf flag
	    (mapcar #'keywordize-foreign (split-sequence #\  flag)))
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
		   (sql-query (colm 'target) :from (tabl 'opinion)
			      :where (sql-= (colm 'url) o-url)))))))


(defun get-local-user-id (user)
  (let ((res (select (colm 'author 'id) :from (tabl 'author)
		     :where (sql-and
			     (sql-= (colm 'author 'type) "wf_user")
			     (sql-= (colm 'author 'value) user)) :flatp t)))
    (when (< 1 (length res))
      (error "Integrity Breach in the author table!"))
    (car res)))
    
(defun author-type (author)
  (cond 
    ((integerp author) :id)
    ((starts-with "http" author) :homepage)
    ((starts-with "mailto" author) :email)
    (t :string)))

(defun get-author-id (author atype)
  (case atype
    (:id (when (exists (select (colm 'author 'id)
				:where (sql-= (colm 'author 'id)
					      (parse-integer author))))
	    (parse-integer author)))
    (:string
     (get-local-user-id author))
    ((or :homepage :email)
     (awhen (select (colm 'id) :from 'author
		    :where (sql-and (sql-= (colm 'type) atype)
				    (sql-= (colm 'value) author)) :flatp t)
       (car it)))))

(defun insert-new-author (atype value)
  (let ((aid (next-val "author_id_seq"))
	(atype (kebab:to-snake-case (mkstr atype))))
    (insert-records :into 'author
		    :attributes '(:id :type :value)
		    :values (list aid atype value))
    aid))

(defun make-user-url (username)
  (strcat *base-url* "u/" username "/"))

(defun make-opinion-url (username opinid)
  (format nil "~a~d" (make-user-url username) opinid))

(defun save-new-opinion (opinion user &key (opinurl #'make-opinion-url)
			 (userurl #'make-user-url))
  (let ((id (next-val "opinion_id_seq")))
    (save-opinion
     (concatenate 'list
		  (collecting 
		    (collect (cons :author (funcall userurl user)))
		    (collect (cons :id id))
		    (collect (cons :url (funcall opinurl user id))))
		  opinion))))

(defun save-opinion (opin)
  (with-keys (:author :target :id :votevalue :datestamp :url :comment
		      :reference :flag) (alist->hash opin)
    (let* ((user (assoc-cdr :author opin))
	   (atype (author-type user))
	   (aid (or (get-author-id user atype)
		    (insert-new-author 
		     (if (eq atype :string) "wf_user" atype) user)))
	   (oid
	    (insert-record 
	     'opinion
	     (collecting
	       (collect 
		   (cons :rooturl (find/store-root-url target)))
	       (dolist (k '(:id :votevalue :target :datestamp :url))
		 (awhen (assoc k opin) (collect it)))
	       (collect (cons :flag 
			      (apply #'format nil "~a ~a" 
				     (mapcar (compose #'to-pascal-case
						      #'mkstr) flag))))
	       (collect (cons :author aid))))))
      (when (and (stringp comment) (string-true comment))
	(insert-records :into 'comment :attributes '(:opinion :comment)
			:values (list oid comment)))
      (when (and (stringp reference) (string-true reference))
	(insert-records :into 'reference :attributes '(:opinion :comment)
			:values (list oid reference)))
      (dolist (k '(:excerpt :excerpt-offset :time-excerpt :excerpt-length))
	(awhen (aand (assoc k opin) (string-true (cdr it)))
	  (insert-records :into 'excerpt :attributes '(:opinion :type :value)
			  :values (list oid (to-snake-case (mkstr k)) it))))
      oid)))
	  
(defun delete-opinion (oid)
  (let ((rooturl (car (col-from-pkey (colm 'opinion 'rooturl) oid))))
    (with-transaction nil
      (delete-records :from 'comment :where (sql-= (colm 'opinion) oid))
      (delete-records :from 'excerpt :where (sql-= (colm 'opinion) oid))
      (delete-records :from 'reference :where (sql-= (colm 'opinion) oid))
      (delete-records :from 'opinion :where (sql-= (colm 'id) oid)))
    (unless (exists (get-assoc-by-col (colm 'opinion 'rooturl) rooturl))
      (delete-records :from 'rooturl :where (sql-= (colm 'id) rooturl)))))

(defun warflagger-user-from-url (url)
  (aref (nth-value 
	 1 (ppcre:scan-to-strings (strcat *base-url* "u/([^/]+)/") url))
	0))



