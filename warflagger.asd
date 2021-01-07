;;;; warflagger.asd

(asdf:defsystem #:warflagger
  :description "The warflagger.net website"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache License, version 2.0"
  :depends-on (#:warflagger-core
               #:webhax
               #:webhax-user
               #:gadgets
               #:sql-stuff
               #:liql
               #:alexandria
               #:cl-who
               #:cl-json
               #:anaphora
               #:kebab
               #:cl-fad
               #:external-program
               #:cl-postgres
               #:ratify
               #:simple-rgb
               #:lquery)
  :serial t
  :components ((:module 
		 :src
		 :serial t
		 :components ((:file "local-settings")
                              (:file "text-extract/textract"
                                     :depends-on ("local-settings"))
                              (:file "package")
                              (:file "warflagger")
                              (:file "db")
                              (:file "bulk-enter")
                              (:file "references")
                              (:file "excerpts")
                              (:file "ranking")
                              (:file "summarizer")))))

