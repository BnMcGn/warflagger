;;;; wf-web.asd

(asdf:defsystem #:wf-web
  :description "Warflagger web application"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Specify license here"
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
               #:userfig
               #:cl-markdown
               #:simple-rgb)
  :serial t
  :components ((:module 
		 :src
		 :serial t
		 :components ((:file "web/package")
                              (:file "web/things")
                              (:file "web/ranks")
                              (:file "web/titlebar")
                              (:file "web/mood")
                              (:file "web/components")
                              (:file "web/misc")
                              (:file "web/opinion-form")
                              (:file "web/account")
                              (:file "web/web")))))

