;;;; package.lisp

(defpackage #:wf/web
  (:use #:cl
        #:clack
        #:alexandria
        #:anaphora
	      #:gadgets
        #:cl-hash-util
        #:clack-pretend
        #:webhax
        #:warflagger
        #:wf/local-settings
        #:wf/text-extract
        #:sql-stuff
        #:thing-lister
        #:parenscript
        #:cl-react
        #:ps-gadgets
        )
  ;;(:shadowing-import-from #:gadgets #:call)
  (:shadowing-import-from #:ps-gadgets #:collecting-set #:strcat
                          #:collecting-string #:collecting #:do-window
                          #:collect)
  (:shadowing-import-from #:parenscript #:switch #:call)
  (:shadowing-import-from #:anaphora #:it))
