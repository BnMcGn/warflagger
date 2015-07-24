
(in-package #:test-warflagger)

(in-suite test-warflagger)

;set up whatever?

(use-package 'wf/text-extract)


;FIXME: need a wf/local-settings package, with tmp location specced.
(defparameter *tmp-dir* "/home/ben/tmp/")
(setf *cache-path* (strcat *tmp-dir* "textract/"))
(defparameter *test-url* 
  "file:///home/ben/quicklisp/local-projects/warflagger/t/sample.html")

(test initialize
  (cl-fad:delete-directory-and-files *cache-path* :if-does-not-exist :ignore)
  (ensure-directories-exist (make-pathname :directory *cache-path*))
  (initialize-indices))

(test (textract-initial :depends-on initialize)
  (is (null (is-cached *test-url*)))
  (is (null (is-fresh *test-url*)))
  (is (null (has-failure *test-url*)))
  (is (null (fresh-failure *test-url*)))
  (is (null (is-pending *test-url*)))
  (is (null (old-page-available *test-url*)))
  (is (= 0 (length (hash-table-keys *byurl*)))))

(test (textract :depends-on initialize)
  (update-page *test-url*)
  (sleep 0.25)
  (is (is-cached *test-url*))
  (is (string= (grab-title *test-url*) "Sample Web Page"))
  (is (streamp (grab-text *test-url*)))
  (is (sequence-starts-with (read-line (grab-text *test-url*)) "Sample")))

(test (textract-nonexistent-url :depends-on initialize)
  (let ((test-url2 (strcat *test-url* "x")))
    (update-page test-url2)
    (sleep 0.25)
    (is (is-cached test-url2))
    (is (has-failure test-url2))
    (is (fresh-failure test-url2))
    (is (null (is-pending test-url2)))))
  


