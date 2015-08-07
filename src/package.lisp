;;;; package.lisp

(defpackage #:warflagger
  (:use #:cl #:wf/local-settings #:wf/text-extract #:sql-stuff #:clsql
	#:gadgets #:alexandria #:anaphora #:kebab)
  (:export
   #:opinion-ids-for-rooturl
   #:opinion-tree-for-rooturl
   #:controversial-p
   #:get-rooturl
   #:get-rooturl-id
   #:get-opinion-peers
   #:find/store-root-url
   #:make-rooturl-real
   #:rooturl-real-p
   #:get-rooturl-by-id
   #:get-rooturl-for-url
   #:all-rooturls
   #:rooturl-p
   #:get-author-data
   #:opinion-from-db-row
   #:opinion-from-id
   #:get-local-user-id
   #:get-author-id))

