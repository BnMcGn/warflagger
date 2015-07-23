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
               #:thing-labels)
  :serial t
  :components ((:file "package")
               (:file "warflagger")
               (:module 
		 :src
		 :serial t
		 :components ((:file "text-extract/textract")))))

