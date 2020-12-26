;;;; wf-web.asd

(asdf:defsystem #:wf-web
  :description "Warflagger web application"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache License, version 2.0"
  :depends-on (#:webhax
               #:clack
               #:clack-middleware-clsql
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
               #:lack-middleware-mount
               #:clath
               #:userfig)
  :serial t
  :components ((:module 
		 :src
		 :serial t
		 :components ((:file "web/package")
                              (:file "web/mood")
                              (:file "web/titlebar")
                              (:file "web/displayables")
                              (:file "web/target-article")
                              (:file "web/target-thread")
                              (:file "web/target-summary")
                              (:file "web/target")
                              (:file "web/opinion-page")
                              (:file "web/grouped")
                              (:file "web/misc")
                              (:file "web/things")
                              (:file "web/opinion-form")
                              (:file "web/account")
                              ;;(:file "web/rest")
                              ;;(:file "web/radmin")
                              (:file "web/web")))))

