(in-package :wf/web)

(def-ps-lib titlebar-components ()
  (ps

    (def-component flag-name
        (let ((label (prop opinion flag 1)))
          (psx
           (:span (+ (chain label (char-at 0) (to-upper-case))
                     (chain label (slice 1)))))))

    (def-component vote-value
        (psx (:img
              :class "opinion-badge"
              :src (strcat "/static/warstats"
                           (make-id-path (prop opinion id))
                           (chain (prop opinion id) (to-string))
                           "/opinion-badge.svg"))))
    
#|
    (def-component vote-value
        (let ((vv (prop opinion votevalue)))
          (case vv
            (-1 (psx (:b :style (create background "#ff0000") "-")))
            (0 (psx (:b :style (create background "#aba3a3") "o")))
            (1 (psx (:b :style (create background "#00ff00") "+"))))))
|#
    (defun %plurify (value single multiple)
      (if (< 1 value)
          (strcat (chain value (to-string)) multiple)
          (strcat (chain value (to-string)) single)))

    (defun display-date-nicely (dstamp)
      (let ((past (ago dstamp)))
        (cond
          ((< 1 (chain past (get-years)))
           (%plurify (chain past (get-years)) " year ago" " years ago"))
          ((< 0 (chain past (get-months)))
           (%plurify (chain past (get-months)) " month ago" " months ago"))
          ((< 0 (chain past (get-weeks)))
           (%plurify (chain past (get-weeks)) " week ago" " weeks ago"))
          ((< 0 (chain past (get-days)))
           (%plurify (chain past (get-days)) " day ago" " days ago"))
          ((< 0 (chain past (get-hours)))
           (%plurify (chain past (get-hours)) " hour ago" " hours ago"))
          ((< 0 (chain past (get-minutes)))
           (%plurify (chain past (get-minutes)) " minute ago" " minutes ago"))
          (t (%plurify (chain past (get-seconds)) " second ago" " seconds ago")))))

    (def-component date-stamp
        (psx
         (:span
          (display-date-nicely
           (new (-date (chain -date (parse (prop opinion datestamp)))))))))

    ;;FIXME: lots of improvement here... Avatar? Stats? Faction?
    (def-component author-long
        (psx (:span (prop opinion authorname))))

    (def-component reply-link
        (let ((excerpt (prop excerpt))
              (offset (prop offset)))
          (psx
           (:form :action "/opinion/" :method "GET"
                  :style (create :display "inline-block")
                  (:input :type "hidden" :name "target" :key 1
                          :value (prop url))
                  (when excerpt
                    (psx (:input :type "hidden" :name "excerpt" :key 3
                                 :value (encode-u-r-i-component excerpt))))
                  (when offset
                    (psx (:input :type "hidden" :name "excerpt" :key 3
                                 :value (encode-u-r-i-component offset))))
                  (:input :type "submit" :value "Reply" :key 2)))))

    (def-component comment-summary
        (let* ((opin (prop opinion))
               (comment (if (chain opin (has-own-property 'comment))
                            (@ opin comment)
                            nil)))
          (if comment
              (if (> (prop trimto) (@ comment length))
                  (psx (:span comment))
                  (psx (:span (chain comment (slice 0 (prop trimto))) "…")))
              (psx (:span "")))))

    ))
