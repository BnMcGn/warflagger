
(in-package #:test-warflagger)

(defparameter *sample-file* (asdf:system-relative-pathname 'warflagger "t/sample.html"))
(defparameter *testurl* (strcat "file://" (princ-to-string *sample-file*)))


(def-suite test-textract
  :description "Tests of WF text extraction system"
  :in wf-tests)

(in-suite test-textract)

(defun init-test-textract ()
  nil)

(test core-extractor "Test the extraction process"
  (multiple-value-bind (title text metadata links) (tt-extract *sample-file*)
    (is (string= title "Sample Web Page"))
    (is (stringp text))
    (is (sequence-starts-with text "Sample"))
    (is (identity links))
    (let ((link (some (lambda (l) (eq "CLICK HERE" (getf l :excerpt))) links)))
      (is (identity link))
      (is (string= (getf link :target) "entities.html")))
    (is (null metadata))))

(test nonexistent-page "Handle request for non-extracted page"
  (let ((test-url2 (strcat *testurl* "abc")))
    (signals wf/ipfs:extracted-data-not-found (wf/ipfs:ipfs-extracted-metadata test-url2))
    (is (null (wf/ipfs:extraction-attempted? test-url2)))))

(test bad-page "Handle bad page"
  (let ((test-url2 (strcat *testurl* "x")))
    (unless (wf/ipfs:extraction-attempted? test-url2)
      (tt-update-page-data test-url2))
    (is (not (wf/ipfs:extracted? test-url2)))
    (is (wf/ipfs:extraction-attempted? test-url2))
    (is (getf (wf/ipfs:ipfs-extracted-metadata test-url2) :errors))))

(test extracted-page "Test extracted page storage"
  (unless (wf/ipfs:extracted? *testurl*)
    (tt-update-page-data-from-file *testurl* *sample-file*))
  (is (wf/ipfs:extraction-attempted? *testurl*))
  (is (wf/ipfs:extracted? *testurl*))
  (is (string= (wf/ipfs:ipfs-extracted-title *testurl*) "Sample Web Page"))
  (is (stringp (wf/ipfs:ipfs-extracted-text *testurl*)))
  (is (sequence-starts-with (wf/ipfs:ipfs-extracted-text *testurl*) "Sample")))

#|
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
  (try-awhile (lambda () (old-page-available *testurl*)) :sleep 0.5 :wait 8.0)
  (print "Printing script errors")
  (print (grab-messages *testurl*))
  (is (is-cached *testurl*))
  (is (string= (grab-title *testurl*) "Sample Web Page"))
  (is (stringp (grab-text *testurl*)))
  (is (sequence-starts-with (grab-text *testurl*) "Sample")))

(test bad-page "Handle bad page"
  (let ((test-url2 (strcat *testurl* "x")))
    (update-page test-url2)
    (sleep 2.0)
    (is (is-cached test-url2))
    (is (not (null (has-failure test-url2))))
    (is (not (null (fresh-failure test-url2))))
    (is (null (is-pending test-url2)))))

|#
