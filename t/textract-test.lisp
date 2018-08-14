
(in-package #:test-warflagger)

(use-package 'wf/text-extract)

(defparameter *tmp-dir* "/tmp/")

(defun test-textract ()
  (let ((*cache-path* (strcat *tmp-dir* "textract/"))
        (testurl
         (strcat
          "file://"
          (princ-to-string (asdf:system-relative-pathname 'warflagger "t/sample.html")))))

    (cl-fad:delete-directory-and-files *cache-path* :if-does-not-exist :ignore)
    (ensure-directories-exist (make-pathname :directory *cache-path*))
    (initialize-indices)

    (plan 15)

    (ok (null (is-cached testurl)))
    (ok (null (is-fresh testurl)))
    (ok (null (has-failure testurl)))
    (ok (null (fresh-failure testurl)))
    (ok (null (is-pending testurl)))
    (ok (null (old-page-available testurl)))
    (ok (= 0 (length (hash-table-keys *byurl*))))

    (update-page testurl)
    (delete-file (wf/text-extract::messages-loc testurl))
    (external-program:start "/bin/ls" (list "/")
                            :output (wf/text-extract::messages-loc testurl))
    (sleep 0.50)
    (print (grab-messages testurl))
    (ok (is-cached testurl))
    (ok (string= (grab-title testurl) "Sample Web Page"))
    (ok (stringp (grab-text testurl)))
    (ok (sequence-starts-with (grab-text testurl) "Sample"))

    (let ((test-url2 (strcat testurl "x")))
      (update-page test-url2)
      (sleep 0.50)
      (ok (is-cached test-url2))
      (ok (has-failure test-url2))
      (ok (fresh-failure test-url2))
      (ok (null (is-pending test-url2))))

    (cl-fad:delete-directory-and-files *cache-path* :if-does-not-exist :ignore)))


