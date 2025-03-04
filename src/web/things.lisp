(in-package :wf/web)

;;; Definitions for the thing-lister

(define-parts things-parts
  :@javascript-link "/static/javascript/warflagger-bundle.js"
  :@javascript-link *warflagger-js-resources*)

;;(setf html-thing-lister:*html-thing-user-parts* nil)
;;(push #'things-parts html-thing-lister:*html-thing-user-parts*)

(setf thing-lister:*thing-sidebox-width* 18)


;;FIXME: Should all be done in query or else with something like an ordered
;; hash table.
(defun flaggers-for-rooturl (rooturl)
  (declare (string rooturl))
  (ordered-unique
   (mapcar #'car
           (merge-query
            (clsql:select (colm 'opinion 'author))
            (for-rooturl-mixin rooturl)
            (order-by-mixin (colm 'opinion 'datestamp))))
   :test #'eq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New thing-lister stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recent-opinions (&key getcount)
  (let ((query (sql-stuff:unexecuted
                 (merge-query
                  (clsql:select
                   (colm 'id)
                   :from (tabl 'opinion))
                  (recent-mixin 'datestamp "30 days")))))
    (if getcount
        (get-count query)
        (mapcar
         #'car
         (merge-query
          query
          (sql-stuff:limit-mixin *thing-limit* *thing-index*)
          (order-by-mixin (colm 'datestamp) :desc))))))

(setf (ningle:route *app* "/opinions-recent/")
      (cljs-page ((title-part "WF: Recent Opinions"))
        (bind-validated-input
            (&key
             (index :integer))
          (let ((thing-lister:*thing-summary-width* 40))
            (thing-lister:display-thing-block-with-pagers
             (tag-as-opinion #'recent-opinions)
             nil
             #'mount-cljs-thing
             "/opinions-recent/"
             (or index 0))))))

(defun special-side-block (source params display-func main-url
                                  &key (class "featurebox_side")
                                  label (trim *thing-sidebox-width*))
  (html-out
   (:div :class "flex justify-center bg-black"
         (:h3 :class "sm:hidden block"
              (:a :href main-url (str label))))
   (lisp (display-thing-block-in-sidebar
          source params display-func main-url
          :class (gadgets:strcat class " hidden sm:block") :label label :trim trim))))

(defun pagerless-main-block (source params display-func main-url
                                    &key (class "featurebox_side")
                                    label (trim *thing-sidebox-width*))
  (display-thing-block-in-sidebar
   source params display-func main-url
   :class class :label label :trim trim))

(defun target-participants (targid &key getcount)
  (if getcount
      (length (flaggers-for-rooturl (get-rooturl-by-id targid)))
      (thing-slice (flaggers-for-rooturl (get-rooturl-by-id targid)))))

(defun target-participants-sidebar (id)
  (special-side-block
   (tag-as-author #'target-participants)
   (list id)
   #'mount-cljs-thing
   (format nil "/target-participants/~a" id)
   :label "Target: Participants"
   :class (featurebox-side nil)
   :trim *thing-sidebox-width*))

;;FIXME: title could identity subject of list for most thing listers
(setf (ningle:route *app* "/target-participants/*")
      (cljs-page ((title-part "WF: Target: Participants"))
        (bind-validated-input
            ((id :integer)
             &key
             (index :integer))
          (display-thing-block-with-pagers
           (tag-as-author #'target-participants)
           (list id)
           #'mount-cljs-thing
           (format nil "/target-participants/~a" id)
           (or index 0)))))

(defun author-opinions (authid &key getcount)
  (let ((query (sql-stuff:unexecuted
                 (clsql:select (colm 'opinion 'id)
                               :from (tabl 'opinion)
                               :where
                               (clsql:sql-= (colm 'author) (sql-escape authid))))))
   (if getcount
        (get-count query)
        (mapcar
         #'car
         (merge-query
          query
          (sql-stuff:limit-mixin *thing-limit* *thing-index*)
          (list :order-by (list (list (colm 'datestamp) :desc))))))))

(defun author-opinions-sidebar (id)
  (special-side-block
   (tag-as-opinion #'author-opinions '(:hide-author t))
   (list id)
   #'mount-cljs-thing
   (format nil "/author-opinions/~a" id)
   :label "Author: Opinions"
   :class (featurebox-side nil)
   :trim *thing-sidebox-width*))

(setf (ningle:route *app* "/author-opinions/*")
      (cljs-page ((title-part "WF: Author: Opinions"))
        (bind-validated-input
            ((id :integer)
             &key
             (index :integer))
          (display-thing-block-with-pagers
           (tag-as-opinion #'author-opinions)
           (list id)
           #'mount-cljs-thing
           (format nil "/author-opinions/~a" id)
           (or index 0)))))

(defun author-discussions (authid &key getcount)
  (let ((rslt  (ordered-unique (mapcar #'car (clsql:select (colm 'opinion 'rooturl)
                                            :from (tabl 'opinion)
                                            :where
                                            (clsql:sql-= (colm 'author) (sql-escape authid)))))))
   (if getcount
       (length rslt)
       (thing-slice rslt))))

(defun author-discussions-sidebar (id)
  (special-side-block
   (tag-as-rooturl #'author-discussions)
   (list id)
   #'mount-cljs-thing
   (format nil "/author-discussions/~a" id)
   :label "Author: Discussions"
   :class (featurebox-side nil)
   :trim *thing-sidebox-width*))

(setf (ningle:route *app* "/author-discussions/*")
      (cljs-page ((title-part "WF: Author: Discussions"))
        (bind-validated-input
            ((id :integer)
             &key
             (index :integer))
          (display-thing-block-with-pagers
           (tag-as-rooturl #'author-discussions)
           (list id)
           #'mount-cljs-thing
           (format nil "/author-discussions/~a" id)
           (or index 0)))))

(defun user-recently-viewed (user &key getcount)
  (let ((query
          (sql-stuff:unexecuted
            (merge-query
             (clsql:select
              (colm :rootid) (colm :opinionid) :from (tabl :looks)
                                               :where (clsql:sql-= (colm :wf_user) (sql-escape user))
                                               :order-by (colm :firstlook))
             (sql-stuff:recent-mixin 'firstlook "60 days")))))
    (if getcount
        (get-count query)
        (mapcar
         (lambda (row)
           (destructuring-bind (root opin) row
             (list (or opin root) (if opin 'opinion 'target))))
         (merge-query
          query
          (order-by-mixin 'firstlook))))))

(defun %author-replies (authid &key getcount)
  (if getcount
      (get-count (unexecuted (author-replies authid)))
      (mapcar
       #'car
       (merge-query
        (author-replies authid)
        (list :order-by (list (list (colm 'datestamp) :desc)))))))

(setf (ningle:route *app* "/author-replies/*")
      (cljs-page ((title-part "WF: Replies to Author"))
        (bind-validated-input
            ((id :integer)
             &key
             (index :integer))
          (display-thing-block-with-pagers
           (tag-as-opinion #'%author-replies)
           (list id)
           #'mount-cljs-thing
           (format nil "/author-replies/~a" id)
           (or index 0)))))

;;FIXME: should really be including reference opinion...
(defun %author-references (authid &key getcount)
  (if getcount
      (get-count (unexecuted (author-references authid)))
      (cl-utilities:collecting
        (dolist (url (author-references authid))
          (cl-utilities:collect
              (if-let ((opurl (is-location-opinml? url)))
                (let ((opinion (opinion-exists-p opurl)))
                  (hu:hash
                   (:type :opinion)
                   (:key (assoc-cdr :id opinion))
                   (:id (assoc-cdr :iid opinion))))
                (hu:hash
                 (:type :reference)
                 (:key (tryit (get-rooturl-id url)))
                 (:id url))))))))

(setf (ningle:route *app* "/author-references/*")
      (cljs-page ((title-part "WF: Author: References Made"))
        (bind-validated-input
            ((id :integer)
             &key
             (index :integer))
          (display-thing-block-with-pagers
           #'%author-references
           (list id)
           #'mount-cljs-thing
           (format nil "/author-references/~a" id)
           (or index 0)))))

(defun %author-questions (authid &key getcount)
  (let ((query (sql-stuff:unexecuted (author-questions authid))))
    (if getcount
        (get-count query)
        (mapcar
         #'car
         (merge-query
          query
          (sql-stuff:limit-mixin *thing-limit* *thing-index*)
          (list :order-by (list (list (colm 'datestamp) :desc))))))))

(setf (ningle:route *app* "/author-questions/*")
      (cljs-page ((title-part "WF: Author: Questions Asked"))
        (bind-validated-input
            ((id :integer)
             &key
             (index :integer))
          (display-thing-block-with-pagers
           (tag-as-question #'%author-questions)
           (list id)
           #'mount-cljs-thing
           (format nil "/author-questions/~a" id)
           (or index 0)))))

(setf (ningle:route *app* "/thing-source/*")
      (input-function-wrapper
       (lambda ()
         (bind-validated-input
             ((name :string)
              (id :integer)
              &key
              (index :integer)
              (limit :integer))
           (let ((*thing-limit* limit)
                 (*thing-index* index)
                 (source
                   (cdr
                    (assoc name
                           (list
                            (cons "author-open" (tag-as-question #'%author-questions)))
                           :test #'equal))))
             (prin1 (funcall source id) *webhax-output*))))
       ;;Is this right for unicode?
       :content-type "text/plain"))


;; React thing tools

(defun tag-as-author (func)
  (lambda (&rest params)
    (let ((res (apply func params)))
      (if (listp res)
          (mapcar
           (lambda (itm)
             (hu:hash
              (:type :author)
              (:key itm)
              ;;FIXME: what if remote?
              (:id (author-representation-from-row (get-author-data itm)))))
           res)
          res))))

(defun tag-as-opinion (func &optional options)
  (lambda (&rest params)
    (let ((res (apply func params)))
      (if (listp res)
          (mapcar
           (lambda (itm)
             (hu:plist->hash
              (list* :type :opinion
                     :key itm
                     :id (assoc-cdr :iid (opinion-by-id itm))
                     options)))
           res)
          res))))

(defun tag-as-question (func)
  (lambda (&rest params)
    (let ((res (apply func params)))
      (if (listp res)
          (mapcar
           (lambda (itm)
             (hu:hash
              (:type :question)
              (:key itm)
              (:id (assoc-cdr :iid (opinion-by-id itm)))))
           res)
          res))))

(defun tag-as-rooturl (func)
  (lambda (&rest params)
    (let ((res (apply func params)))
      (if (listp res)
          (mapcar
           (lambda (itm)
             (hu:hash
              (:type :rooturl)
              (:key itm)
              (:id (get-rooturl-by-id itm))))
           res)
          res))))

;;FIXME: counter doesn't seem like a healthy idea...
(defparameter *mr-counter* 0)

(defun mount-cljs-thing (items &key url name)
  (let ((thingid (format nil "cljs-thing-~a" (incf *mr-counter*))))
    (mount-cljs-component ("thing-lister" :mount-id thingid :key thingid)
      :things (lisp
               (list* 'list
                      (mapcar #'ps-gadgets:alist->ps-object-code
                              (mapcar #'hu:hash->alist items))))
      :url (lisp url)
      :name (lisp name)
      :trim (lisp *thing-summary-width*))))
