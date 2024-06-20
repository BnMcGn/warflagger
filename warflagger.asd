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
               #:bordeaux-threads
               #:lparallel
               #:cl-who
               #:cl-json
               #:anaphora
               #:kebab
               #:cl-fad
               #:external-program
               #:cl-postgres
               #:ratify
               #:simple-rgb
               #:lquery
               #:cl-ipfs-api2
               #:access
               #:clerk
               #:3bmd
               #:cl-csv
               #:plump-sexp
               #:dexador
               #:osicat
               #:crawly
               #:readability/plump)
  :serial t
  :components ((:module
                :src
                :serial t
                :components ((:file "local-settings")
                             (:file "text-extract/textract"
                              :depends-on ("local-settings"))
                             (:file "package")
                             (:file "warflagger")
                             (:file "objective")
                             (:file "db")
                             (:file "references")
                             (:file "excerpts")
                             (:file "ranking")
                             (:file "opinml")
                             (:file "ballot-box")
                             (:file "author-stats")
                             (:file "score-script")
                             (:file "ipfs")
                             (:file "textract")
                             (:file "summarizer")
                             (:file "grouped")
                             (:file "admin")
                             (:file "slicer"))))
  :in-order-to ((test-op (test-op "test-warflagger"))))

