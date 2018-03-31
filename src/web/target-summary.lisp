(in-package :wf/web)

;; Summary tab for root target page

(defun target-summary ()
  (ps

    (def-component opinion-summary
        (let ((opinion (@ (prop opinion-store)
                          (if (prop tree-address)
                              (list-last (prop tree-address))
                              (prop opid)))))
          (psx
           (:div
            ;;FIXME: do we need to insert styling-data from warstats?
            (if (prop tree-address)
                (psx (:display-tree-address :tree-address (prop tree-address)))
                (psx (:vote-value :opinion opinion)))
            (:flag-name :key 2 :opinion opinion) " "
            (:date-stamp :key 3 :opinion opinion) " "
            (:author-long :key 4 :opinion opinion) " "
            (:display-warstats2 :key 5)
            (:reply-link :key 6 :url (@ opinion url))))))

    (defun %format-referenced (refs)
      (let ((res (create)))
        (dolist (r refs)
          (setf (getprop res r)
                (strcat "/static/warstats"
                        (make-id-path r)
                        (chain r (to-string))
                        "/opinion-data.json")))))

    (def-component referenced
        (:div
         (:h3 "Incoming references:")
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
             (:h3 "Questions and answers:")
             (collecting
                 (dolist (id (filter-opins-question
                              (prop tree-addresses) (prop opinion-store) (prop warstats)))
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
             (:h3 "High scoring replies:")
             (collecting
                 (dolist (id data)
                   (collect
                       (psx
                        (:opinion-summary
                         :tree-address (getprop (prop opinion-store) id 'tree-address)
                         :opinion-store (prop opinion-store)))))))))))

    (def-component controversial
        (let ((data (filter-opins-controversial
                     (prop tree-addresses) (prop opinion-store) (prop warstats))))
          (psx
           (:display-if
            :test (not-empty data)
            (:div
             (:h3 "Controversial replies:")
             (collecting
                 (dolist (id data)
                   (collect
                       (psx
                        (:opinion-summary
                         :key (unique-id)
                         :tree-address (getprop (prop opinion-store) id 'tree-address)
                         :opinion-store (prop opinion-store)))))))))))

    (def-component references-summary
        (psx (:div
              ;;FIXME: Should the references list be ordered?
              (:h3 "References made:")
              (collecting
                  (do-keyvalue (id ref (prop references))
                    (collect
                        (psx
                         (:opinion-summary
                          :key (unique-id)
                          :tree-address (getprop (prop opinion-store) id 'tree-address)
                          :opinion-store (prop opinion-store)))))))))

    (def-component target-root-summary
        (let ((rwstats (prop warstats root)))
          (psx
           (:div
            :... (format-styling-data (@ this props))
            (:headline :key 1
                       :title (prop title)
                       :external-link (prop url))
            (:display-warstats2 :key 2)
            (:div
             :key 3
             (:h2 "Discussion Statistics")
             (:h3 "Score: " (@ rwstats effect))
             (:h3 "Controversy: " (@ rwstats controversy))
             (:h3 "Immediate responses: " (@ rwstats replies-immediate))
             (:h3 "Total responses: " (@ rwstats replies-total))
             (:display-if :test (@ rwstats referenced)
                          (:referenced-loader :key 4 :referenced (@ rwstats referenced)))
             (:display-if :test (not-empty (prop references))
                          (:references-summary :... (@ this props)))
             (:questions :... (@ this props))
             (:high-scores :... (@ this props))
             (:controversial :... (@ this props)))))))


    ))
