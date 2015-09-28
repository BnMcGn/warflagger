;;;; package.lisp

(defpackage #:wf/web
  (:use #:cl
        #:clack
        #:alexandria
        #:anaphora
	      #:gadgets
        #:clack-pretend
        #:webhax
        #:warflagger
        #:wf/local-settings
        #:wf/text-extract
        #:sql-stuff
        #:thing-lister
        ))
