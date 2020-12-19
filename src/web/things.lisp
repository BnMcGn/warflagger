(in-package :wf/web)

;;; Definitions for the thing-lister

(define-parts things-parts
  :@javascript-link "/static/javascript/warflagger-bundle.js"
  :@javascript-link *warflagger-js-resources*)

;; Depends on: titlebar-components
(define-ps-lib warflagger-things ()
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
              (:opinion-icon :key 1 :opinion (prop opinion))
              (:flag-name :key 2 :opinion (prop opinion))
              (:author-long :key 4 :opinion (prop opinion)))))

    (def-component opinion-line-long
        (psx (:div
              :class "summary_line"
              (:opinion-icon :key 1 :opinion (prop opinion))
              (:flag-name :key 2 :opinion (prop opinion))
              (:date-stamp :key 3 :opinion (prop opinion))
              (:author-long :key 4 :opinion (prop opinion))
                                        ;(:target-short :target)
              (:comment-summary :key 5 :opinion (prop opinion) :trimto 30))))))

(setf html-thing-lister:*html-thing-user-parts* nil)
(push #'things-parts html-thing-lister:*html-thing-user-parts*)

(setf html-thing-lister:*thing-summary-sidebar-width* 18)


(defun display-opinion-line (opid)
  (let ((opinion (opinion-by-id opid))
        (line-id (gadgets:mkstr (gensym "mount-opinion-"))))
    (html-out
      (:div :id line-id)
      (mount-component (opinion-line :mount-id (lisp line-id))
        :opinion (lisp (ps-gadgets:as-ps-data opinion))
        :trim (lisp thing-lister:*thing-summary-width*)))))

(defun display-target-line (rootid)
  (let* ((target (target-info-for-line rootid))
         (warstats (getf target :warstats)))
    (html-out
      (:div
       (:a :href (make-rootid-url rootid)
           (str (truncate-string (getf target :title) :length 80)))
       (:span
        :title
        (format nil "~a direct responses, ~a in conversation"
                (getf warstats :replies-immediate) (getf warstats :replies-total))
        (str (format nil "(~a/~a)"
                     (getf warstats :replies-immediate) (getf warstats :replies-total))))
       (str (with-output-to-string (*webhax-output*)
              (display-warstats warstats)))))))

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

(defun opinion-thing-link (opid)
  (format nil "/opinion-page/~a" opid))

(defun target-info-for-line (rootid)
  (let ((url (get-rooturl-by-id rootid)))
    (list
     :id rootid
     ;;FIXME: tryit isn't the safest thing to use
     :title (or (tryit (grab-title url)) "PAGE UNAVAILABLE")
     :text (or (tryit (grab-text url)) "PAGE UNAVAILABLE")
     :url url
     :warstats (warstats-for-target url))))

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
      (quick-page ()
        (bind-validated-input
            (&key
             (index :integer))
          (let ((thing-lister:*thing-summary-width* 40))
            (html-thing-lister:display-things-with-pagers
             #'recent-opinions
             nil
             #'display-opinion-line
             "/opinions-recent/"
             (or index 0))))))

(defun display-author-line (authid)
  (html-out
    (:div
     (:a :href (author-thing-link authid)
         (str (warflagger:author-representation-from-row (get-author-data authid)))))))

(defun target-participants (targid &key getcount)
  (if getcount
      (length (flaggers-for-rooturl (get-rooturl-by-id targid)))
      (thing-slice (flaggers-for-rooturl (get-rooturl-by-id targid)))))

(defun target-participants-sidebar (id)
  (display-things-sidebar
   #'target-participants
   (list id)
   #'display-author-line
   (format nil "/target-participants/~a" id)
   :label "Target: Participants"))

(setf (ningle:route *app* "/target-participants/*")
      (quick-page ()
        (bind-validated-input
            ((id :integer)
             &key
             (index :integer))
          (display-things-with-pagers
           #'target-participants
           (list id)
           #'display-author-line
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
  (display-things-sidebar
   #'author-opinions
   (list id)
   #'display-opinion-line
   (format nil "/author-opinions/~a" id)
   :label "Author: Opinions"))

(setf (ningle:route *app* "/author-opinions/*")
      (quick-page ()
        (bind-validated-input
            ((id :integer)
             &key
             (index :integer))
          (display-things-with-pagers
           #'author-opinions
           (list id)
           #'display-opinion-line
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
  (display-things-sidebar
   #'author-discussions
   (list id)
   #'display-target-line
   (format nil "/author-discussions/~a" id)
   :label "Author: Discussions"))

(setf (ningle:route *app* "/author-discussions/*")
      (quick-page ()
        (bind-validated-input
            ((id :integer)
             &key
             (index :integer))
          (display-things-with-pagers
           #'author-discussions
           (list id)
           #'display-target-line
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

(defun author-replies (authid &key getcount)
  (let ((query
          (unexecuted
            (clsql:select
             (colm :id) :from (tabl :opinion)
                        :where
                        (clsql:sql-in
                         (colm :target)
                         (clsql:sql-query
                          (colm :opinion :url)
                          :from (tabl :opinion)
                          :where (clsql:sql-= (colm :author) authid)))))))
    (if getcount
        (get-count query)
        (merge-query
         query
         (list :order-by (list (list (colm 'datestamp) :desc)))))))

(setf (ningle:route *app* "/author-replies/*")
      (quick-page ()
        (bind-validated-input
            ((id :integer)
             &key
             (index :integer))
          (display-things-with-pagers
           #'author-replies
           (list id)
           #'display-opinion-line
           (format nil "/author-replies/~a" id)
           (or index 0)))))
