
(in-package #:test-warflagger)

;;FIXME: tmpdir should be handled differently on non-travis test runs
(let* ((tmpdir (homedir-relative-pathname "tmp/"))
       (*cache-path* (merge-pathnames "cache/" tmpdir))
       (*warstats-path* (merge-pathnames "warstats/" tmpdir))
       (*static-path* "quicklisp/local-projects/wf-static/")
       ;;FIXME: Need better general way to handle db credentials.
       (*test-db-connect-spec*
         #+ci '("localhost" "test_opinions" "ben" "test_password")
         ;; '( :unix "test_opinions" "ben" "test_password")
         #-ci *test-db-connect-spec*)
       (*db-connect-type* :postgresql-socket3))
  (ensure-directories-exist *cache-path*)
  (ensure-directories-exist *warstats-path*)
  (ensure-directories-exist (merge-pathnames "javascript/" *static-path*))
  (handler-bind
      ((clsql:sql-connection-error
        (lambda (c)
          (declare (ignore c))
          (let ((restart (find-restart 'clsql-sys::use-old)))
            (when restart
              (invoke-restart restart))))))
    (clsql:connect *test-db-connect-spec* :database-type *db-connect-type*))

  (liql:liql)
  (test-textract)
  (test-db)
  (test-form-validator)
  (test-services)
  ;;FIXME: think this through
  ;;(delete-dir *cache-path* :recursive t)
  ;;(delete-dir *warstats-path* :recursive t)
  )
