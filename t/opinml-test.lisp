(in-package #:test-warflagger)

(defparameter *sample-comment*
  "This is a #test of hashtags.

This is an [URL](http://no.where.com:0016500/things#and?more=things&even=more)asdf)x
 #(bad-directive)
#(more-bad)
#(unknown-directive might have contents)
#(vote-value 0)")

(defparameter *opinion-store* nil)
(defparameter *opinion-tree* nil)
(defparameter *score-script* nil)
(defparameter *subjective* nil)

(defun init-test-opinml ()
  (let* ((opin-path (asdf:system-relative-pathname 'warflagger "t/opinions/"))
         ;;FIXME: Should handle the illegal token better, not just drop the opinion
         (data (handler-bind ((proto:illegal-token #'wf/ipfs:skip-opinion))
                 (wf/ipfs::objective-data-for-dir opin-path)))
         (more-data (warflagger:execute-score-script data)))
    (setf *opinion-store* (getf data :opinion-store))
    (setf *opinion-tree* (getf data :opinion-tree))
    (setf *score-script* (getf data :score-script))
    (setf *subjective* more-data)))

