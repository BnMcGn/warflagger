(in-package :wf/web)

;; This a (currently experimental) comment-style layout of the opinion tree of a
;; root target

(defun target-thread ()
  (ps

    (def-component thread-excerpt
        (psx
         (:div
          (prop opinion excerpt))))

    (def-component thread-opinion
        (let ((opinion (@ (prop opinions) (list-last (prop tree-address))))
              (text (if (< 1 (prop tree-address length))
                        (or (@ opinion comment) "")
                        (prop text))))
          (psx
           (:div
            (:vote-value :key 1 :opinion opinion) " "
            (:flag-name :key 2 :opinion opinion) " "
            (:date-stamp :key 3 :opinion opinion) " "
            (:author-long :key 4 :opinion opinion) " "
            (:reply-link :key 6 :url (@ opinion url))
            (when (has-excerpt-p opinion)
              (if (has-found-excerpt-p opinion)
                  (psx (:thread-excerpt
                        :opinion opinion
                        :text text))
                  (psx (:thread-excerpt
                        :opinion opinion))))
            (when (@ opinion comment)
              (psx (:div (@ opinion comment))))))))

    (defun %reformat-opinions (opins)
      (let ((opinstore (create)))
        (list
         (collecting
             (labels ((proc (tree address)
                        (dolist (branch tree)
                          (let ((newadd (chain address (concat (@ branch 0 id)))))
                            (setf (@ opinstore (@ branch 0 id)) (@ branch 0))
                            (collect newadd)
                            (when (< 1 (@ branch length))
                              (proc (chain branch (slice 1)) newadd))))))
               (proc opins (list))))
         opinstore)))

    ;;For now, drop in place of target-root-inner
    (def-component target-root-thread
        (psx
         (:div
          (:div "Title:" (prop title))
          (collecting
              (let ((data (%reformat-opinions (prop opinions))))
                (dolist (op (@ data 0))
                  (collect
                      (psx (:thread-opinion
                            :opinions (@ data 1)
                            :text (prop text)
                            :tree-address op)))))))))

    ))
