
(in-package #:test-warflagger)


(defparameter *target* "http://warflagger.net/static/html/sample.html")
(defparameter *user* "WFTestUser")
(defparameter *refurl* "http://warflagger.net/faq/")
(defparameter *testcomment* "Test")

(defun test-db ()
  (let ((userid (gadgets:assoc-cdr
                 :id
                 (sql-stuff:get-assoc-by-col (sql-stuff:colm 'author 'value) *user*)))
        (rootid (find/store-root-url *target*))
        (opin1
         (list (cons :target *target*)
               (cons :votevalue 1)
               (cons :datestamp (clsql:get-time))
               (cons :comment *testcomment*)
               (cons :reference *refurl*)
               (cons :flag '(:positive :like))
               (cons :excerpt "and")
               (cons :excerpt-offset "3")))
        (opin2
         (list (cons :votevalue -1)
               (cons :datestamp (clsql:get-time))
               (cons :flag '(:negative :disagree))))
        (opin1id nil)
        (opin2id nil)
        (ourl nil))
    (plan 24)

    ;;May need to set up author
    (unless (integerp userid)
      (setf userid (insert-new-author :display-name *user*)))

    (diag "Basic sanity checks:")
    (ok (integerp userid))
    (ok (integerp rootid))
    (ok (rooturl-p *target*))
    (is (warflagger::flag-to-lisp (warflagger::flag-to-db '(:negative :is-wrong)))
        '(:negative :is-wrong))

    (diag "Author functionality:")
    (let* ((authdata (get-author-data userid))
           (wfuser (assoc-cdr :wf-user authdata)))
      ;; FIXME: Why no wf-user in some cases?
      ;;(ok (stringp wfuser))
      (is (author-representation-from-row authdata) *user*)
      ;; FIXME: local users don't have a guaranteed public identification yet.
      ;;(is (author-identification-from-row data))
      ;;(is (get-local-user-from-id (get-local-user-id wfuser)) wfuser)
      )
    ;;Create opinion with excerpt, reference, offset
    (diag "Create opinion:")
    (setf opin1id (save-opinion-from-user opin1 userid))
    (ok (integerp opin1id))
    (let ((saved-opin (opinion-by-id opin1id)))
      (ok saved-opin)

      (setf ourl (assoc-cdr :url saved-opin))

      (ok (opinion-exists-p ourl))
      (is (assoc-cdr :author-id saved-opin) userid)
      (is (assoc-cdr :excerpt saved-opin) (assoc-cdr :excerpt opin1))
      (is (get-rooturl-for-url ourl) *target*)
      (is (get-rooturl-by-id (get-rooturl-id *target*)) *target*)

      (diag "Create child opinion:")
      ;;Create second opinion on first
      (setf opin2id (save-opinion-from-user
                     (cons (cons :target ourl) opin2) userid))

      (is (gethash :text (opinion-text-server ourl)) *testcomment*)
      (is (gethash :text (opinion-text-server
                          (assoc-cdr :url (opinion-by-id opin2id)))) "")
      (is (caar (opinion-tree-for-target ourl)) opin2id))
    (is-values (get-target-id-from-url *target*) (list rootid :rooturl))
    (is-values (get-target-id-from-url ourl) (list opin1id :opinion))

    (diag "Opinion tree checks:")
    (let* ((tree (opinion-tree-for-rooturl *target*))
           (subtree (find-if (lambda (x) (eq (car x) opin1id)) tree)))
      (is (car subtree) opin1id)
      (is (caadr subtree) opin2id))

    (ok (member *target* (all-rooturls) :test #'equal))
    (ok (rooturl-p *target*))
    (ok (null (rooturl-p ourl)))

    (diag "Delete opinions:")
    (delete-opinion opin1id)
    (delete-opinion opin2id)
    (is-error (opinion-by-id opin1id) 'simple-error)
    (is-error (opinion-by-id opin2id) 'simple-error)
    (finalize)
    ))


#|

(defun rooturl-real-p (id)
(defun make-rooturl-real (root-id)

(defun author-identification-from-row (data)
(defun get-author-identification (aid)
(defun author-type (author)
(defun find-author-id (author atype)

(defun get-looks (user rootid)
(defun set-look (user &key rootid opinionid)
(defun get-rooturl-looks (rootid)
(defun get-opinion-looks (opinid)
(defun get-target-looks (target-url)
|#
