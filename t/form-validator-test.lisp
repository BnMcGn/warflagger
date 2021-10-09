
(in-package #:test-warflagger)


(defparameter *test-input*
  (hu:plist->alist
   (list :target *target*
         :votevalue "1"
         :comment *testcomment*
         :reference *refurl*
         :flag "Positive: Like"
         :excerpt "and"
         :excerpt-offset "3")))

(def-suite test-form-validator
  :description "Opinion form input validator tests"
  :in wf-tests)

(in-suite test-form-validator)

;;FIXME: Need some negative tests
(test basic-input
  (multiple-value-bind (vals sig)
      (webhax-validate:validate-batch *test-input* wf/web::*opinion-form-specs*)
    (is sig)
    (is (integerp (gethash :votevalue vals))))
  (multiple-value-bind (vals sig)
      (webhax-validate:validate-batch
       (list* (cons :votevalue "test") (cons :target "test")
              (cons :excerpt-offset "1.5") *test-input*)
       wf/web::*opinion-form-specs*)
    (is (null sig))
    (is 3 (length (hash-table-keys vals)))
    (is (key-in-hash? :votevalue vals))
    (is (key-in-hash? :excerpt-offset vals))
    (is (key-in-hash? :target vals))
    (is (null (key-in-hash? :flag vals)))))



