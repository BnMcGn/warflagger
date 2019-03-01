
(in-package #:test-warflagger)

;;; Tests for various web services go here.
;;; Tests can be before or after the service is hosted through the web server.

(defparameter *target* "http://warflagger.net/static/html/sample.html")
(defparameter *user* "WFTestUser")
(defparameter *refurl* "http://warflagger.net/faq/")
(defparameter *testcomment* "Test")

(defun test-services ()

  ;;Target-seek-server
  (let ((result (target-seek-server "asdf")))
    (is (gethash :status result) "missing"))
  (let ((result (target-seek-server *target*)))
    (is (gethash :status result) "success")
    (ok (sequence-starts-with (gethash :warstats result) "http"))))


#|
- text server? tested already
- looks / look-post
 - will need username
- opinion-text-server
- target-seek-server


|#
