;;;; test-warflagger.asd

(asdf:defsystem #:test-warflagger
  :description "Test suite for warflagger functionality."
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:warflagger
               #:alexandria
               #:gadgets
               #:fiveam) 
  :serial t
  :components ((:module :t
                        :serial t
                        :components ((:file "package")
                                     (:file "textract-test")))))



