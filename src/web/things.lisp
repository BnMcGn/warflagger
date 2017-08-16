(in-package :wf/web)

;;; Definitions for the thing-lister

(define-parts warflagger-things
  :@javascript
  (titlebar-components)
  :@javascript
  (ps
    (def-component opinion-line
        (if (< (prop trim) 20)
            (psx (:opinion-line-short
                  :... (@ this props)))
            (psx (:opinion-line-long
                  :... (@ this props)))))

    (def-component opinion-line-short
        (psx (:div
              :class "summary_line"
              (:vote-value :key 1 :opinion (prop opinion))
              (:flag-name :key 2 :opinion (prop opinion))
              (:author-long :key 4 :opinion (prop opinion)))))

    (def-component opinion-line-long
        (psx (:div
              :class "summary_line"
              (:vote-value :key 1 :opinion (prop opinion))
              (:flag-name :key 2 :opinion (prop opinion))
              (:date-stamp :key 3 :opinion (prop opinion))
              (:author-long :key 4 :opinion (prop opinion))
                                        ;(:target-short :target)
              (:comment-summary :key 5 :opinion (prop opinion) :trimto 40))))))

(setf html-thing-lister:*html-thing-user-parts* nil)
(push #'warflagger-things html-thing-lister:*html-thing-user-parts*)
(push #'webhax:react-parts html-thing-lister:*html-thing-user-parts*)

(setf html-thing-lister:*thing-summary-sidebar-width* 18)

(defun display-opinion-line (opinion)
  (let ((line-id (gadgets:mkstr (gensym "mount-opinion-"))))
    (webhax-core:html-out-str
      (:div :id line-id)
      (mount-component (opinion-line :mount-id (lisp line-id))
        :opinion (lisp-raw (json:encode-json-alist-to-string opinion))
        :trim (lisp thing-lister:*thing-summary-width*)))))

;;; FIXME: If reasonable, this should move to the rest of the URL stuff in
;; warflagger.lisp
(defun author-thing-link (authid)
  #|
  ;;;FIXME: User link not implemented because we need to be sure that there is a 2 way link between screen-name and author id. Should be in db table somewhere but might not be. So only author URL for now.
  (if-let
      ((uname (assoc :wf-user (get-author-data authid))))
    (make-user-url (userfig:userfig-value-for (cdr uname) 'screen-name))
    (format nil "/author/~a" authid)))
  |#
  (format nil "/author/~a" authid))

(def-thing
    'author
    #'get-author-data
  #'author-representation-from-row
  :lister (list
           #'author-lister
           :sortkeys '(values id)
           :length (lambda (&rest params)
                     (get-count
                      (unexecuted
                        (apply #'author-lister params)))))
  :html-thing-link #'author-thing-link
  :limitable t)

(def-db-thing
    'opinion
    'opinion
  #'display-opinion-line
  :keyfunc (lambda (id)
             (opinion-from-db-row (get-assoc-by-pkey 'opinion id)))
  :sortkeys '(target author datestamp excerpt rooturl))

(def-thing
    'target
    (lambda (rootid)
      ;;FIXME: maybe should check that page is extracted/available
      (let ((url (get-rooturl-by-id rootid)))
        (list
         :id rootid
         :title (grab-title url)
         :text (grab-text url)
         :url url
         :warstats (warstats-for-target url))))
  (lambda (targdata)
    (concatenate 'string
                 (truncate-string (getf targdata :title) :length 15)
                 " - "
                 (truncate-string (getf targdata :text) :length 15)))
  :lister
  (list
   (wrap-with-paging-handler
    (lambda (&key order-by)
      (declare (ignore order-by))
      ;;Only have relevance for now
      (get-ranked-rootids)))
   :sortkeys '(relevance))
  :html-thing-link (lambda (id) (format nil "/target/~a" id)))

(html-thing-lister:def-thing-action 'opinion 'reply
  '(lambda (id) (setf (@ window location href) (+ "/opinion/?opinion-id=" id))))

(def-thing-connector
    'opinion
    'replies
  (lambda (&rest x)
    (mapcar #'car (opinion-tree-for-target
                   (assoc-cdr :url (opinion-from-id (car x))))))
  :other-thing 'opinion)

;;FIXME: Should all be done in query or else with something like an ordered
;; hash table.
(defun flaggers-for-rooturl (rooturl)
  (ordered-unique
   (mapcar #'car
           (merge-query
            (clsql:select (colm 'opinion 'author))
            (for-rooturl-mixin rooturl)
            (order-by-mixin (colm 'opinion 'datestamp))))
   :test #'eq))

(def-thing-connector
    'opinion
    'participants
  (lambda (&rest x)
    (flaggers-for-rooturl
     (get-rooturl-by-id
      (assoc-cdr :rooturl
                 (get-assoc-by-pkey 'opinion (string-unless-number (car x)))))))
  :other-thing 'author)

(def-thing-connector
    'author
    'opinion
  (lambda (&rest id)
    (mapcar #'car
            (clsql:select (colm 'opinion 'id)
                          :from (tabl 'opinion)
                          :where
                          (clsql:sql-= (colm 'author) (sql-escape (car id)))))))



(def-thing-connector
    'author
    'discussions
  (lambda (&rest id)
    (ordered-unique
     (mapcar #'car
             (clsql:select (colm 'opinion 'rooturl)
                           :from (tabl 'opinion)
                           :where
                           (clsql:sql-= (colm 'author) (sql-escape (car id)))))
     :test #'eq))
  :other-thing 'target)

(def-thing-connector
    'target
    'participants
  (lambda (&rest x)
    (flaggers-for-rooturl
     (get-rooturl-by-id (car x))))
  :other-thing 'author)

(def-thing-connector
    'user
    'recently-viewed
  (lambda (&rest x)
    (mapcar
     (lambda (row)
       (destructuring-bind (root opin) row
         (list (or opin root) (if opin 'opinion 'target))))
     (clsql:select
      (colm :rootid) (colm :opinionid) :from (tabl :looks)
      :where (clsql:sql-= (colm :wf_user) (sql-escape (car x)))
      :order-by (colm :firstlook))))
  :other-thing :multiple)

(def-thing-connector
    'user
    'replies
  (lambda (&rest x)
    (mapcar
     #'car
     (clsql:select
      (colm :id) :from (tabl :opinion)
      :where
      (clsql:sql-in
       (colm :target)
       (clsql:sql-query
        (colm :opinion :url) :from (list (tabl :author) (tabl :opinion))
        :where (clsql:sql-and
                (clsql:sql-= (colm :author :id) (colm :opinion :author))
                (clsql:sql-= (colm :author :type) "wf_user")
                (clsql:sql-= (colm :author :value) (sql-escape (car x)))))))))
  :other-thing 'opinion)
