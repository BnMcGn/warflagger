(in-package :wf/web)

;; Summary tab for root target page

(defun target-summary ()
  (ps

    (def-component target-root-summary
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
           (:h3 "Immediate responses:" (prop warstats root replies-immediate))
           (:h3 "Total responses:" (prop warstats root replies-total))
           (:h3 "Incoming references:"))
          )))


    ))
