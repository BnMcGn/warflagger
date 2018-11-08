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
               #:html-thing-lister
               #:ratify
               #:warflagger
               #:ps-gadgets
               #:lack-middleware-mount
               #:clath
               #:userfig)
  :serial t
  :components ((:module 
		 :src
		 :serial t
		 :components ((:file "web/package")
                              (:file "web/things")
                              (:file "web/mood")
                              (:file "web/titlebar")
                              (:file "web/target-article")
                              (:file "web/target-thread")
                              (:file "web/target-summary")
                              (:file "web/target")
                              (:file "web/opinion-page")
                              (:file "web/grouped")
                              (:file "web/misc")
                              (:file "web/opinion-form")
                              (:file "web/account")
                              (:file "web/web")))))

