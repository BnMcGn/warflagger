
(in-package #:test-warflagger)

(use-package 'wf/text-extract)

(defparameter *tmp-dir* "/tmp/")
(defparameter *testurl*
  (strcat
   "file://"
   (princ-to-string (asdf:system-relative-pathname 'warflagger "t/sample.html"))))

(def-suite test-textract
  :description "Tests of WF text extraction system"
  :in wf-tests)

(in-suite test-textract)

(defun init-test-textract ()
  (initialize-indices))

(test sanity-checks "Basic Sanity Checks"
  ;; Note: these assume a fresh (empty) cache directory
  (is (null (is-cached *testurl*)))
  (is (null (is-fresh *testurl*)))
  (is (null (has-failure *testurl*)))
  (is (null (fresh-failure *testurl*)))
  (is (null (is-pending *testurl*)))
  (is (null (old-page-available *testurl*)))
  (is (= 0 (length (hash-table-keys *byurl*)))))

(test process-page "Process a page"
  (update-page *testurl*)
  (sleep 0.50)
  (print "Printing script errors")
  (print (grab-messages *testurl*))
  (is (is-cached *testurl*))
  (is (string= (grab-title *testurl*) "Sample Web Page"))
  (is (stringp (grab-text *testurl*)))
  (is (sequence-starts-with (grab-text *testurl*) "Sample")))

(test bad-page "Handle bad page"
  (let ((test-url2 (strcat *testurl* "x")))
    (update-page test-url2)
    (sleep 0.50)
    (is (is-cached test-url2))
    (is (has-failure test-url2))
    (is (fresh-failure test-url2))
    (is (null (is-pending test-url2)))))

