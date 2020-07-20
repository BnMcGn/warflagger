(in-package :wf/web)

;; This a (currently experimental) comment-style layout of the opinion tree of a
;; root target

(define-ps-lib target-thread ()
  (ps

    (defvar on-screen (@ (require "react-on-screen") default))

    (def-component target-root-thread
        (psx
         (:div :style (create position "relative")
          (:target-title :key "x" :... (@ this props))
          (collecting
              (let ((data (reformat-opinions (prop opinions))))
                (dolist (op (prop tree-addresses))
                  (collect
                      (psx (:on-screen
                            :key (list-last op)
                            ;; :once t ;; Isn't working anyways.
                            (:thread-opinion
                             :opinions (prop opinions)
                             :opinion-store (prop opinion-store)
                             :text (prop text)
                             :styling-data
                             (format-styling-data
                              (copy-merge-all (@ %thisref props)
                                              (create
                                               'opinion-store (@ data 1)
                                               'tree-address op
                                               'opinions (@ data 1))))
                             :tree-address op
                             :warstats (prop warstats)
                             :looks (prop looks)
                             :look-handler (prop look-handler)
                             :reference
                             (getprop (prop references) (list-last op))))))))))))

    ))
