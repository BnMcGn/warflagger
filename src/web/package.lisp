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
        #:warflagger
        #:wf/local-settings
        #:wf/text-extract
        #:sql-stuff
        #:thing-lister
        #:html-thing-lister
        #:parenscript
        #:cl-react
        #:ps-gadgets
        #:webhax-user
        )
  ;;(:shadowing-import-from #:gadgets #:call)
  (:shadowing-import-from #:ps-gadgets #:collecting-set #:strcat
                          #:collecting-string #:collecting #:do-window
                          #:collect #:dotree)
  (:shadowing-import-from #:parenscript #:switch #:call)
  (:shadowing-import-from #:anaphora #:it))
