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
          (psx
           (:b (case vv (-1 "-") (0 "o") (1 "+"))))))

    (def-component date-stamp
        (let ((past (ago (chain -date (parse (prop opinion datestamp))))))
          (psx
           (:span (cond
                    ((< 0 (chain past (get-weeks)))
                     (strcat (chain date-stamp (to-date-string))
                             " "
                             (chain date-stamp (to-locale-time-string))))
                    ((< 0 (chain past (get-days)))
                     (strcat (chain past (get-days) (to-string)) "days ago"))
                    ((< 0 (chain past (get-hours)))
                     (strcat (chain past (get-hours) (to-string))
                             "hours ago"))
                    ((< 0 (chain past (get-minutes))
                        (strcat (chain past (get-minutes) (to-string))
                                "minutes ago")))
                    (t (strcat (chain past (get-seconds) (to-string))
                               "seconds ago")))))))

    ;;FIXME: lots of improvement here... Avatar? Stats? Faction?
    (def-component author-long
        (:span (prop opinion author)))

    ))
