
(in-package #:test-warflagger)

(defparameter *target* "https://warflagger.net/static/html/sample.html")
(defparameter *user* "WFTestUser")
(defparameter *refurl* "https://warflagger.net/faq/")
(defparameter *testcomment* "Test")


(def-suite test-services
  :description "Tests for various web services"
  :in wf-tests)

(in-suite test-services)

(test target-seek-server "Target seek server"
  (let ((result (target-seek-server "asdf")))
    (is (gethash :status result) "missing"))
  (let ((result (target-seek-server *target*)))
    (is (gethash :status result) "success")
    (is (sequence-starts-with (gethash :warstats result) "http"))))


#|
- text server? tested already
- looks / look-post
 - will need username
- opinion-text-server
- target-seek-server


|#
