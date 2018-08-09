
(in-package #:test-warflagger)

;;FIXME: tmpdir should be handled differently on non-travis test runs
(let* ((tmpdir "")
       (*cache-path* (merge-pathnames "cache/" tmpdir))
       (*warstats-path* (merge-pathnames "warstats/" tmpdir))
       ;;FIXME: Need better general way to handle db credentials.
       (*test-db-connect-spec* '( :unix "test_opinions" "ben" "test_password"))
       (*db-connect-type* :postgresql-socket3))
  (ensure-directories-exist *cache-path*)
  (ensure-directories-exist *warstats-path*)
  (clsql:connect *test-db-connect-spec* :database-type *db-connect-type*)

  (test-textract)
  (test-db)
  ;;FIXME: think this through
  ;;(delete-dir *cache-path* :recursive t)
  ;;(delete-dir *warstats-path* :recursive t)
  )
