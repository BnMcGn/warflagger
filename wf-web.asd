;;;; wf-web.asd

(asdf:defsystem #:wf-web
  :description "Warflagger web application"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache License, version 2.0"
  :depends-on (#:webhax
               #:clack
	       #:clack-handler-hunchentoot
	       #:lack-session-store-dbi
               #:ningle
               #:clack-pretend
               #:gadgets
               #:sql-stuff
               #:alexandria
               #:cl-who
               #:anaphora
               #:thing-lister
               #:ratify
               #:warflagger-core
               #:warflagger
               #:ps-lib-tool
               #:ps-gadgets
               #:ps-react-gadgets
               #:flaglib
               #:reacl
               #:lack-middleware-mount
               #:clath
               #:claxy
               #:userfig)
  :serial t
  :components ((:module 
		 :src
		 :serial t
		 :components ((:file "web/package")
                              (:file "web/headers")
                              (:file "web/misc")
                              (:file "web/mood")
                              (:file "web/things")
                              (:file "web/opinion-form")
                              (:file "web/account")
                              (:file "web/web")))))

