;;;; package.lisp

(defpackage #:wf/web
  (:use #:cl
        #:clack
        #:alexandria
        #:cl-who
        #:anaphora
	      #:gadgets
        #:cl-hash-util
        #:clack-pretend
        #:webhax
        #:warflagger-core
        #:warflagger
        #:wf/local-settings
        #:sql-stuff
        #:thing-lister
        #:parenscript
        #:reacl
        #:ps-gadgets
        #:webhax-user
        #:ps-lib-tool)
  ;;(:shadowing-import-from #:gadgets #:call)
  (:shadowing-import-from #:wf/local-settings #:*cache-path*)
  (:shadowing-import-from #:ps-gadgets #:collecting-set #:strcat
                          #:collecting-string #:collecting #:do-window
                          #:collect #:dotree)
  (:shadowing-import-from #:parenscript #:switch #:call)
  (:shadowing-import-from #:anaphora #:it)
  (:export
   #:dump-static-grouped-suggestions
   #:terminate-thread-on-broken-pipe
   #:web-restart))
