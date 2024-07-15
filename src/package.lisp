;;;; package.lisp

(defpackage #:warflagger
  (:use #:cl #:wf/local-settings #:warflagger-core #:sql-stuff #:clsql
        #:gadgets #:alexandria #:anaphora #:kebab #:webhax #:cl-postgres #:liql)
  (:shadowing-import-from #:wf/local-settings #:*ssl-key-file*)
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
   #:opinion-by-id
   #:get-local-user-id
   #:find-author-id
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
   #:generate-rooturl-warstats
   #:if-production
   #:when-production
   #:unless-production
   #:delete-opinion
   #:get-target-id-from-url
   #:save-new-references
   #:*flag-colors*
   #:*direction-colors*
   #:make-opinion-url
   #:make-rootid-url
   #:cluster-discussions
   #:*flag-category-keys*
   #:*default-vote*
   #:url-p
   #:bulk-enter-file
   #:target-replies
   #:tree-address
   #:warstats-url-server
   #:target-seek-server
   #:discussion-tree-for-root
   #:order-discussions-by-most-recent-opinion
   #:discussion-roots
   #:get-target-id
   #:question-opinion-p
   #:text-server-dispatcher
   #:opinion-for-location
   #:previous-break
   #:next-break
   #:get-target-text
   #:get-headline-for-url
   #:make-author-url
   #:deserialize-opinion-from-stream
   #:get-ipfs-hash-from-url
   #:*opinion-store*
   #:iid
   #:opinion
   #:opinion-with-iid
   #:opinion-text
   #:extended-opinion-p
   #:extended-opinion
   #:opinion-with-iid-p
   #:opinion-p
   #:iid-p
   #:list-of-type
   #:list-of-type-p
   #:recognized-flag-p
   #:known-flags
   #:make-ballot-box
   #:ballot-box-empty-p
   #:apply-ballot-box-to-warstats!
   #:ballot-box-totals
   #:score-controversy
   #:cast-vote!
   #:merge-ballot-boxes
   #:merge-with-inverted-ballot-boxes
   #:merge-ballot-boxes!
   #:merge-with-inverted-ballot-boxes!
   #:iid-tree-address
   #:iid-tree-address-p
   #:opinion-store
   #:opinion-store-p
   #:iid-opinion-tree
   #:score-script
   #:is-author-initialized
   #:initialize-author
   #:save-opinion
   #:opinion-reference
   #:not-found
   #:execute-score-script
   #:serialize-opinion
   #:uri-domain
   #:reference-opinion-p
   #:js-compatible-utcstamp
   #:with-inverted-case
   #:*side-opinions*
   #:copy-ballot-box
   #:compare-ballot-boxes
   #:rank-ballot-boxes
   #:excerpt-segment-points
   #:flavor-from-warstats
   #:clean-string-for-excerpt
   #:unknown-flag
   #:all-proper-references
   #:*id-return-type*
   #:hashtag
   #:author-rooturls
   #:author-replies
   #:author-references
   #:author-opinions
   #:author-urls
   #:is-location-opinml?
   #:deserialize-warstat
   #:refd-to
   #:discussion-refd-to
   #:vast-majority-p
   #:author-questions
   #:tt-update-page-data
   #:tt-update-page-data-from-file))

(defpackage #:wf/ipfs
  (:use #:cl #:gadgets #:alexandria #:access)
  (:export
   #:target-text
   #:suggest-target-title
   #:target-title
   #:text-comments-tree-p
   #:title-comments-tree-p
   #:text-script
   #:title-script
   #:general-script
   #:participants
   #:flag-core
   #:opinion-references
   #:opinion-can-apply-dircs-to-parent
   #:objective-data-for-opinions
   #:ipfs-write-rooturl-data
   #:unknown-flag
   #:opinion-can-apply-hashtag-to-parent
   #:skip-opinion
   #:has-found-excerpt-p
   #:opinion-is-question
   #:ipfs-write-grouped-data
   #:ipfs-warstats
   #:ipfs-rooturl-path
   #:ipfs-file-exists-p
   #:ipfs-extracted-text
   #:ipfs-write-extracted-text
   #:ipfs-extracted-metadata
   #:ipfs-write-extracted-metadata
   #:ipfs-extracted-title
   #:extracted-data-not-found
   #:extraction-attempted?
   #:extracted?
   #:ipfs-opinion-path
   #:ipfs-have-text-for-rooturl?
   #:ipfs-write-partial-rooturl-data))
