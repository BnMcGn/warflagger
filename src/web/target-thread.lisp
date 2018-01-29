(in-package :wf/web)

;; This a (currently experimental) comment-style layout of the opinion tree of a
;; root target

(defun target-thread ()
  (ps

    (def-component thread-excerpt
        (psx
         (:div
          (:whatever))))

    (def-component thread-opinion
        (psx
         (:div
          (:vote-value :key 1 :opinion (prop opinion)) " "
          (:flag-name :key 2 :opinion (prop opinion)) " "
          (:date-stamp :key 3 :opinion (prop opinion)) " "
          (:author-long :key 4 :opinion (prop opinion)) " "
          (:reply-link :key 6 :url (prop opinion url))
          (when (prop opinion comment)
            (:div (prop opinion comment))))))

    (defun %reformat-opinions (opins)
      (collecting
          (labels ((proc (tree address)
                     (dolist (branch tree)
                       (let ((newadd (chain address (concat (@ branch 0 id)))))
                         (collect (list (@ branch 0) newadd))
                         (when (< 1 (@ branch length))
                           (proc (chain branch (slice 1)) newadd))))))
            (proc opins (list)))))

    ;;For now, drop in place of target-root-inner
    (def-component target-root-thread
        (psx
         (:div
          (:div "Title:" (prop title))
          (collecting
              (dolist (op (%reformat-opinions (prop opinions)))
                (collect
                    (psx (:thread-opinion :opinion (@ op 0) :tree-address (@ op 1)))))))))

    ))
