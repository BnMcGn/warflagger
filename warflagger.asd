;;;; warflagger.asd

(asdf:defsystem #:warflagger
  :description "The warflagger.net website"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache License, version 2.0"
  :depends-on (#:webhax
               #:webhax-user
               #:gadgets
               #:sql-stuff
               #:alexandria
               #:cl-who
               #:anaphora
               #:cl-fad
               #:external-program
               #:cl-postgres
               #:ratify)
  :serial t
  :components ((:module 
		 :src
		 :serial t
		 :components ((:file "local-settings")
                              (:file "text-extract/textract"
                                     :depends-on ("local-settings"))
                              (:file "package")
                              (:file "constants")
                              (:file "warflagger")
                              (:file "db")
                              (:file "excerpts")
                              (:file "ranking")))))

