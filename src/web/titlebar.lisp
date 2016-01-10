(in-package :wf/web)

(defun titlebar-components ()
  (ps

    (def-component flag-name
        (let ((label (prop opinion flag 1)))
          (psx
           (:span (+ (chain label (char-at 0) (to-upper-case))
                     (chain label (slice 1)))))))

    (def-component vote-value
        (let ((vv (prop opinion votevalue)))
          (case vv
            (-1 (psx (:b :style (create background "#ff0000") "-")))
            (0 (psx (:b :style (create background "#aba3a3") "o")))
            (1 (psx (:b :style (create background "#00ff00") "+"))))))

    (defun display-date-nicely (dstamp)
      (let ((past (ago dstamp)))
        (cond
          ((< 0 (chain past (get-weeks)))
           (strcat (chain dstamp (to-date-string))
                   " "
                   (chain dstamp (to-locale-time-string))))
          ((< 0 (chain past (get-days)))
           (strcat (chain past (get-days) (to-string)) " days ago"))
          ((< 0 (chain past (get-hours)))
           (strcat (chain past (get-hours) (to-string))
                   " hours ago"))
          ((< 0 (chain past (get-minutes)))
              (strcat (chain past (get-minutes) (to-string))
                      " minutes ago"))
          (t (strcat (chain past (get-seconds) (to-string))
                     " seconds ago")))))

    (def-component date-stamp
        (psx
         (:span
          (display-date-nicely
           (new (-date (chain -date (parse (prop opinion datestamp)))))))))

    ;;FIXME: lots of improvement here... Avatar? Stats? Faction?
    (def-component author-long
        (psx (:span (prop opinion author))))

    ))
