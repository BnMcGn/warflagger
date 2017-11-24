;;;; test-warflagger.asd

(asdf:defsystem #:test-warflagger
  :description "Test suite for warflagger functionality."
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache License, version 2.0"
  :depends-on (#:warflagger
               #:alexandria
               #:gadgets
               #:prove) 
  :serial t
  :components ((:module :t
                        :serial t
                        :components ((:file "package")
                                     (:file "textract-test")))))



