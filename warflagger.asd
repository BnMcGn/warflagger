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
               #:ratify)
  :serial t
  :components ((:module 
		 :src
		 :serial t
		 :components ((:file "text-extract/textract")
                              (:file "local-settings")
                              (:file "package")
                              (:file "data")
                              (:file "db")
                              (:file "warflagger")))))

