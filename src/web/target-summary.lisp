(in-package :wf/web)

;; Summary tab for root target page

(define-ps-lib target-summary ()
  (ps

    (defun %format-referenced (refs)
      (let ((res (create)))
        (dolist (r refs)
          (setf (getprop res r)
                (make-warstats-url r 'opinion)))
        res))

    (def-component referenced
        (psx
         (:div
          (:h3 :key 1 "Incoming references:")
          (collecting
            (dolist (r (prop referenced))
              (let ((data (getprop (prop inrefs) r)))
                (when data
                  (collect
                      (psx
                       (:opinion-summary
                        :key r
                        :opinion-store (create-from-list (list r (@ data opinion)))
                        ;; We specify opid rather than tree address to prevent opinion-summary from
                        ;; attempting to render the whole icon stack for the opinion. If we decide
                        ;; that we want the whole stack, we will need to load warstats for all of
                        ;; those opins and icons in referenced-loader. A bit heavy...
                        :opid r
                        ;; :tree-address (@ data tree-address)
                        ;;Warstats needs to be an opinid keyed object, but we only have a single warstat
                        :warstats (create-from-list (list (list-last (@ data tree-address))
                                                          (@ data warstats)))
                        :looks (prop looks)
                        :look-handler (prop look-handler)))))))))))

    (def-component referenced-loader
        (psx
         (:json-loader
          :sources (%format-referenced (prop referenced))
          :store-name "inrefs"
          :reducer #'copy-merge-all
          (:referenced :... (@ this props)))))

    (defun %question-answers (treead opinions opstore)
      (collecting
          (dolist (opin (opinion-children treead opinions))
            (when (and (chain (list "evidence" "secondHand" "eyeWitness" "anecdotal")
                              (includes (@ opin 0 flag 1)))
                       (> 1 (@ opin 0 votevalue)))
              (collect (@ opin 0 id))))))

    (def-component question-summary
        (let ((opin (getprop (prop opinion-store) (prop opinion-id))))
          (psx
           (:div
            (:opinion-summary
             :key 1
             :opinion-store (prop opinion-store)
             :tree-address (@ opin tree-address)
             :warstats (prop warstats)
             :looks (prop looks)
             :look-handler (prop look-handler))
            (collecting
                (dolist (ansid (%question-answers (@ opin tree-address) (prop opinions)
                                                  (prop opinion-store)))
                  (collect
                      (psx
                       (:opinion-summary
                        :key ansid
                        :opinion-store (prop opinion-store)
                        :warstats (prop warstats)
                        :looks (prop looks)
                        :look-handler (prop look-handler)
                        :opid ansid)))))))))

    (def-component questions
        (let ((data (filter-opins-question
                     (prop tree-addresses) (prop opinion-store) (prop warstats))))
          (psx
           (:display-if
            :test (not-empty data)
            (:div
             (:h3 :key 2 "Questions and answers:")
             (collecting
                 (dolist (id data)
                   (collect
                       (psx
                        (:question-summary
                         :key id
                         :opinion-id id
                         :opinions (prop opinions)
                         :opinion-store (prop opinion-store)
                         :look-handler (prop look-handler)
                         :looks (prop looks)
                         :warstats (prop warstats)))))))))))

    (def-component high-scores
        (let ((data (filter-opins-score
                     (prop tree-addresses) (prop opinion-store) (prop warstats))))
          (psx
           (:display-if
            :test (not-empty data)
            (:div
             (:h3 :key 2 "High scoring replies:")
             (collecting
                 (dolist (id data)
                   (collect
                       (psx
                        (:opinion-summary
                         :key id
                         :tree-address (getprop (prop opinion-store) id 'tree-address)
                         :opinion-store (prop opinion-store)
                         :warstats (prop warstats)
                         :looks (prop looks)
                         :look-handler (prop look-handler)))))))))))

    (def-component controversial
        (let ((data (filter-opins-controversial
                     (prop tree-addresses) (prop opinion-store) (prop warstats))))
          (psx
           (:display-if
            :test (not-empty data)
            (:div
             (:h3 :key 2 "Controversial replies:")
             (collecting
                 (dolist (id data)
                   (collect
                       (psx
                        (:opinion-summary
                         :key id
                         :tree-address (getprop (prop opinion-store) id 'tree-address)
                         :opinion-store (prop opinion-store)
                         :warstats (prop warstats)
                         :looks (prop looks)
                         :look-handler (prop look-handler)))))))))))

    (def-component references-summary
        (psx (:div
              ;;FIXME: Should the references list be ordered?
              (:h3 :key 1 "References made:")
              (collecting
                  (do-keyvalue (id ref (prop references))
                    (collect
                        (psx
                         (:opinion-summary
                          :key id
                          :tree-address (getprop (prop opinion-store) id 'tree-address)
                          :opinion-store (prop opinion-store)
                          :warstats (prop warstats)
                          :looks (prop looks)
                          :look-handler (prop look-handler)))))))))

    (def-component target-root-summary
        (let ((rwstats (prop warstats root)))
          (psx
           (:div
            :class "target-summary"
            (:target-title :key 1 :... (@ this props))
            (:div
             :key 3
             (:h2 :key 4 "Discussion Statistics")
             (:h3 :key 5 "Score: " (@ rwstats effect))
             (:h3 :key 6 "Controversy: " (@ rwstats controversy))
             ;;FIXME: Shouldn't count refbot/system opinions?
             (:h3 :key 7 "Immediate responses: " (@ rwstats replies-immediate))
             (:h3 :key 8 "Total responses: " (@ rwstats replies-total))
             (:display-if :key 9 :test (@ rwstats referenced)
                          (:referenced-loader :... (@ this props) :referenced (@ rwstats referenced)))
             (:display-if :key 10 :test (not-empty (prop references))
                          (:references-summary :... (@ this props)))
             (:questions :key 11 :... (@ this props))
             (:high-scores :key 12 :... (@ this props))
             (:controversial :key 13 :... (@ this props)))))))

    ))
