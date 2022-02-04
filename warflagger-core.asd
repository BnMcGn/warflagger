;;;; warflagger-core.asd

(asdf:defsystem #:warflagger-core
  :description "Resources for WarFlagger"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache License, version 2.0"
  :depends-on (#:gadgets #:alexandria)
  :serial t
  :components ((:module 
		 :src
		 :serial t
		 :components ((:file "constants")
                              (:file "core"))))) 
