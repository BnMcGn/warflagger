;;;; warflagger.asd

(asdf:defsystem #:warflagger
  :description "The warflagger.net website"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache License, version 2.0"
  :depends-on (#:webhax
               #:webhax-user
               #:gadgets
               #:sql-stuff
               #:liql
               #:alexandria
               #:cl-who
               #:cl-json
               #:anaphora
               #:cl-fad
               #:external-program
               #:cl-postgres
               #:ratify
               #:simple-rgb)
  :serial t
  :components ((:module 
		 :src
		 :serial t
		 :components ((:file "local-settings")
                              (:file "text-extract/textract"
                                     :depends-on ("local-settings"))
                              (:file "package")
                              (:file "constants")
                              (:file "warflagger"
                                     :depends-on ("constants"))
                              (:file "db")
                              (:file "bulk-enter")
                              (:file "references")
                              (:file "excerpts")
                              (:file "ranking")
                              (:file "summarizer")))))

