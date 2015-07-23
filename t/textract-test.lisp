
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

(test (textract-initial :depends-on (and initialize))
  (is (null (is-cached *test-url*)))
  (is (null (is-fresh *test-url*)))
  (is (null (has-failure *test-url*)))
  (is (null (fresh-failure *test-url*)))
  (is (null (is-pending *test-url*)))
  (is (null (old-page-available *test-url*)))
  (is (= 0 (length (hash-table-keys *byurl*)))))

(test (textract :depends-on initialize)
  (update-page *test-url*)
  (sleep 1)
  (is (is-cached *test-url*))
  (print (grab-title *test-url*))
  (print wf/text-extract:*cache-path*)
  (print (wf/text-extract::cache-loc *test-url*)))
  


