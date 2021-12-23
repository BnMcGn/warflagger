(in-package #:test-warflagger)

(defparameter *sample-comment*
  "This is a #test of hashtags.

This is an [URL](http://no.where.com:0016500/things#and?more=things&even=more)asdf)x
 #(bad-directive)
#(more-bad)
#(unknown-directive might have contents)
#(vote-value 0)")

(defun init-test-opinml ()
  (let* ((opin-path (asdf:system-relative-pathname 'warflagger "t/opinions/"))
         (data (wf/ipfs::objective-data-for-dir opin-path))
         (more-data (warflagger:execute-score-script data)))
    (values data more-data)))

