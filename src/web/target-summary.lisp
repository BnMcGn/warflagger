(in-package :wf/web)

;; Summary tab for root target page

(define-ps-lib target-summary ()
  (ps

    ;;FIXME: relocate
    (def-component opinion-summary
        (let* ((opinion (@ (prop opinion-store)
                          (if (prop tree-address)
                              (list-last (prop tree-address))
                              (prop opid))))
               (id (strcat "opinion-summary-" (@ opinion id))))
          (psx
           (:div
            :id id
            :... (or (prop styling-data)
                     (format-styling-data (@ this props)))
            :class "opinion-summary"
            :on-mouse-enter (@ this handle-mouse-enter)
            :on-mouse-leave (@ this handle-mouse-leave)
            (if (prop tree-address)
                (psx (:display-tree-address :key 1 :tree-address (prop tree-address)))
                (psx (:vote-value :key 1 :opinion opinion)))
            (:flag-name :key 2 :opinion opinion) " "
            (:date-stamp :key 3 :opinion opinion) " "
            (:author-long :key 4 :opinion opinion) " "
            (:display-warstats2 :key 5)
            (:reply-link :key 6 :url (@ opinion url))
            (:tool-tip :key 7
                       :active (state viewable) :position "bottom"
                       :arrow "right"
                       :group "two"
                       :parent (strcat "#" id)
                       (:opinion-info
                        :... (@ this props)
                        :opinion opinion)))))
      get-initial-state
      (lambda () (create viewable false))
      handle-mouse-enter
      (lambda (e)
        (set-state viewable true))
      handle-mouse-leave
      (lambda (e)
        (set-state viewable false)))

    (defun %format-referenced (refs)
      (let ((res (create)))
        (dolist (r refs)
          (setf (getprop res r)
                (make-warstats-url r 'opinion)))))

    (def-component referenced
        (:div
         (:h3 :key 1 "Incoming references:")
         (dolist (r (prop referenced))
           (let ((data (getprop (prop inrefs) r)))
             (when data
               (psx
                (:opinion-summary
                 :key (unique-id)
                 :opinion-store (create-from-list (list r data))
                 :tree-address (@ data tree-address)
                 :warstats (@ data warstats))))))))

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
             :warstats (prop warstats))
            (collecting
                (dolist (ansid (%question-answers (@ opin tree-address) (prop opinions)
                                                  (prop opinion-store)))
                  (collect
                      (psx
                       (:opinion-summary
                        :key (unique-id)
                        :opinion-store (prop opinion-store)
                        :warstats (prop warstats)
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
                         :key (unique-id)
                         :opinion-id id
                         :opinions (prop opinions)
                         :opinion-store (prop opinion-store)
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
                         :key (unique-id)
                         :tree-address (getprop (prop opinion-store) id 'tree-address)
                         :opinion-store (prop opinion-store)
                         :warstats (prop warstats)))))))))))

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
                         :key (unique-id)
                         :tree-address (getprop (prop opinion-store) id 'tree-address)
                         :opinion-store (prop opinion-store)
                         :warstats (prop warstats)))))))))))

    (def-component references-summary
        (psx (:div
              ;;FIXME: Should the references list be ordered?
              (:h3 :key 1 "References made:")
              (collecting
                  (do-keyvalue (id ref (prop references))
                    (collect
                        (psx
                         (:opinion-summary
                          :key (unique-id)
                          :tree-address (getprop (prop opinion-store) id 'tree-address)
                          :opinion-store (prop opinion-store)
                          :warstats (prop warstats)))))))))

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
                          (:referenced-loader :referenced (@ rwstats referenced)))
             (:display-if :key 10 :test (not-empty (prop references))
                          (:references-summary :... (@ this props)))
             (:questions :key 11 :... (@ this props))
             (:high-scores :key 12 :... (@ this props))
             (:controversial :key 13 :... (@ this props)))))))

    ))
