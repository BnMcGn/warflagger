;;;; warflagger.asd

(asdf:defsystem #:warflagger
  :description "Describe warflagger here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:webhax
               #:gadgets
               #:sql-stuff
               #:alexandria
               #:cl-who
               #:anaphora
               #:cl-fad
               #:external-program
               #:thing-labels
               #:pythonic-string-reader
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
                              (:file "data")
                              (:file "db")
                              (:file "excerpts")
                              (:file "warflagger")))))

