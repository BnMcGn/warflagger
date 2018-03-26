(in-package :wf/web)

;; Summary tab for root target page

(defun target-summary ()
  (ps

    (def-component opinion-summary
        (let ((opinion (@ (prop opinions) (list-last (prop tree-address)))))
          (psx
           (:div
            (:vote-value :opinion opinion)
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
                        "/opinion-stats.json")))))

    (def-component referenced
        (:div
         (dolist (r (prop referenced))
           (let ((data (getprop (prop inrefs) r)))
             (when data
               (:opinion-summary
                :key (unique-id)
                :opinions (create-from-list (list r data))
                :tree-address (@ data tree-address)
                :warstats (@ data warstats)))))))

    (def-component referenced-loader
        (psx
         (:json-loader
          :sources (%format-referenced (prop referenced))
          :store-name "inrefs"
          (:referenced :... (@ this props)))))

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
             (:h3 "Immediate responses:" (@ rwstats replies-immediate))
             (:h3 "Total responses:" (@ rwstats replies-total))
             (:h3 "Incoming references:")
             (when (@ rwstats referenced)
               (psx (:referenced-loader :key 4 :referenced (@ rwstats referenced))))
             )
            ))))


    ))