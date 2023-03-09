
(in-package #:test-warflagger)


(defparameter *target* "https://warflagger.net/static/html/sample.html")
(defparameter *user* "WFTestUser")
(defparameter *refurl* "https://warflagger.net/faq/")
(defparameter *testcomment* "Test")

(defparameter *userid* nil)
(defparameter *rootid* nil)
(defparameter *opin1* (list (cons :target *target*)
                            (cons :votevalue 1)
                            (cons :datestamp (clsql:get-time))
                            (cons :comment *testcomment*)
                            (cons :reference *refurl*)
                            (cons :flag '(:positive :like))
                            (cons :excerpt "and")
                            (cons :excerpt-offset "3")))
(defparameter *opin2* (list (cons :votevalue -1)
                            (cons :datestamp (clsql:get-time))
                            (cons :flag '(:negative :disagree))))
(defparameter *opin1id* nil)
(defparameter *opin2id* nil)
(defparameter *ourl* nil)

(def-suite test-db
  :description "WF tests for database reading/writing"
  :in wf-tests)

(in-suite test-db)

(defun init-test-db ()
  (setf *rootid* (find/store-root-url *target*))
  (setf *userid* (gadgets:assoc-cdr
                     :id
                     (sql-stuff:get-assoc-by-col (sql-stuff:colm 'author 'value) *user*)))
  (unless (integerp *userid*)
    (setf *userid* (insert-new-author :display-name *user*)))
  (setf *opin1id* nil)
  (setf *opin2id* nil)
  (setf *ourl* nil))

(test sanity-checks "Basic sanity checks"
  (is (integerp *userid*))
  (is (integerp *rootid*))
  (is (rooturl-p *target*))
  (is (warflagger::flag-to-lisp (warflagger::flag-to-db '(:negative :is-wrong)))
      '(:negative :is-wrong)))

(test author-functionality "Author functionality"
  ;; FIXME: Why no wf-user in some cases?
  ;;(ok (stringp wfuser))
  ;; FIXME: local users don't have a guaranteed public identification yet.
  ;;(is (author-identification-from-row data))
  ;;(is (get-local-user-from-id (get-local-user-id wfuser)) wfuser)
  (is (author-representation-from-row (get-author-data *userid*)) *user*))

(test create-opinions "Create opinions"
  (let ((saved-opin (save-opinion *opin1* nil :authorid *userid*)))
    (setf *opin1id* (assoc-cdr :id saved-opin))
    (is (gadgets:not-empty (assoc-cdr :target saved-opin)))

    (setf *ourl* (assoc-cdr :url saved-opin))

    (is (opinion-exists-p *ourl*))
    (is (assoc-cdr :author-id saved-opin) *userid*)
    (is (assoc-cdr :excerpt saved-opin) (assoc-cdr :excerpt *opin1*))
    (is (get-rooturl-for-url *ourl*) *target*)
    (is (get-rooturl-by-id (get-rooturl-id *target*)) *target*)

    ;;Create second opinion on first
    (setf *opin2id* (assoc-cdr :id
                      (save-opinion
                       (cons (cons :target *ourl*) *opin2*) nil :authorid *userid*)))
    (is (gethash :text (opinion-text-server *ourl*)) *testcomment*)
    (is (gethash :text (opinion-text-server (assoc-cdr :url (opinion-by-id *opin2id*)))) "")
    (is (caar (opinion-tree-for-target *ourl*)) *opin2id*))
  (is (eql *rootid* (get-target-id-from-url *target*)))
  (is (eql :rooturl (nth-value 1 (get-target-id-from-url *target*)))))
  ;;FIXME: No longer returns an integer id. Should be an iid
  ;;(is (equal (values-list (get-target-id-from-url *ourl*)) (list *opin1id* :opinion))))

(test opinion-tree "Opinion tree checks"
  (let* ((tree (opinion-tree-for-rooturl *target*))
         (subtree (find-if (lambda (x) (eq (car x) *opin1id*)) tree)))
    (is (car subtree) *opin1id*)
    (is (caadr subtree) *opin2id*))
  (is (member *target* (all-rooturls) :test #'equal))
  (is (rooturl-p *target*))
  (is (null (rooturl-p *ourl*))))

(test delete-opinions "Delete opinions"
  ;(delete-opinion *opin1id*)
  (delete-opinion *opin2id*)
  ;(signals simple-error (opinion-by-id *opin1id*))
  (signals simple-error (opinion-by-id *opin2id*)))

