
(in-package #:test-warflagger)


(with-temporary-directory (:pathname tmpdir)
  (let ((*cache-path* (merge-pathnames "/cache/" tmpdir))
        (*warstats-path* (merge-pathnames "/warstats/" tmpdir))
        (*test-db-connect-spec* '( :unix "test_opinions" "postgres" ""))
        (*db-connect-type* :postgresql-socket3))
   (ensure-directories-exist *cache-path*)
   (ensure-directories-exist *warstats-path*)
   (clsql:connect *test-db-connect-spec* *db-connect-type*)

   (test-textract)
   (test-db)
   ))
