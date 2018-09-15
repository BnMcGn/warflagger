
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

(defun test-form-validator ()
  (multiple-value-bind (vals sig)
      (webhax-validate:validate-batch *test-input* wf/web::*opinion-form-specs*)
    (ok sig)
    (ok (integerp (gethash :votevalue vals))))
  (multiple-value-bind (vals sig)
      (webhax-validate:validate-batch
       (list* (cons :votevalue "test") (cons :target "test")
              (cons :excerpt-offset "1.5") *test-input*)
       wf/web::*opinion-form-specs*)
    (ok (null sig))
    (is 3 (length (hash-table-keys vals)))
    (ok (key-in-hash? :votevalue vals))
    (ok (key-in-hash? :excerpt-offset vals))
    (ok (key-in-hash? :target vals))
    (ok (null (key-in-hash? :flag vals)))))

