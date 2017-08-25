;;;; package.lisp

(defpackage #:warflagger
  (:use #:cl #:wf/local-settings #:wf/text-extract #:sql-stuff #:clsql
	#:gadgets #:alexandria #:anaphora #:kebab #:webhax #:cl-postgres)
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
   #:get-author-id
   #:user-lister
   #:get-author-representation
   #:find-excerpt-position
   #:create-textdata
   #:*flag-categories*
   #:*flag-labels*
   #:*vote-ranges*
   #:*flag-types-source*
   #:author-representation-from-row
   #:warstats-for-target
   #:get-ranked-rootids
   #:opinion-tree-for-target
   #:for-rooturl-mixin
   #:insert-new-author
   #:save-opinion-from-user
   #:author-lister
   #:author-identification-from-row
   #:get-author-identification
   #:opinion-exists-p
   #:opinion-text-server
   #:get-looks
   #:set-look
   #:make-user-url
   #:get-local-user-from-id
   #:opinion-effect
   #:*opinion-effect-cache*
   #:generate-rooturl-warstats))

