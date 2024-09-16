
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
  (multiple-value-bind (title text metadata links) (warflagger::tt-extract-html *sample-file*)
    (is (string= title "Sample Web Page"))
    (is (stringp text))
    (is (sequence-starts-with text "Sample"))
    (is (identity links))
    (let ((link (first-match (lambda (l) (equal "CLICK HERE" (getf l :excerpt))) links)))
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
      (tt-update-page-data test-url2 "text/html"))
    (is (not (wf/ipfs:extracted? test-url2)))
    (is (wf/ipfs:extraction-attempted? test-url2))
    (is (getf (wf/ipfs:ipfs-extracted-metadata test-url2) :errors))))

(test extracted-page "Test extracted page storage"
  (unless (wf/ipfs:extracted? *testurl*)
    (tt-update-page-data *testurl* *sample-file* "text/html"))
  (is (wf/ipfs:extraction-attempted? *testurl*))
  (is (wf/ipfs:extracted? *testurl*))
  (is (string= (wf/ipfs:ipfs-extracted-title *testurl*) "Sample Web Page"))
  (is (stringp (wf/ipfs:ipfs-extracted-text *testurl*)))
  (is (sequence-starts-with (wf/ipfs:ipfs-extracted-text *testurl*) "Sample")))
