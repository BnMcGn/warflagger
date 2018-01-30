(in-package :wf/web)

;; This a (currently experimental) comment-style layout of the opinion tree of a
;; root target

(defparameter *excerpt-margin* 100)

(defun target-thread ()
  (ps

    (def-component thread-excerpt
        (if (prop text)
            (let*
                ((tlength (prop text length))
                 (estart (prop opinion text-position 0))
                 (eend (+ (prop opinion text-position 1) estart))
                 (tstart (when (< (lisp *excerpt-margin*) estart)
                           (- estart (lisp *excerpt-margin*))))
                 (tend (when (< (+ eend (lisp *excerpt-margin*)) tlength)
                         (+ eend (lisp *excerpt-margin*))))
                 (leading-context (prop text (slice (or tstart 0) estart)))
                 (excerpt (prop text (slice estart eend)))
                 (trailing-context (prop text (slice eend (or tend tlength)))))
              (psx
               (:div
                (:span leading-context)
                ;;FIXME: quick hack
                (:span :class (flavor (list (list (prop opinion)))) excerpt)
                (:span trailing-context))))
            (psx
             (:div
              "WARNING!"
              (prop opinion excerpt)))))

    (def-component thread-opinion
        (let* ((opinion (@ (prop opinions) (list-last (prop tree-address))))
               (parentid (when (< 1 (prop tree-address length))
                           (@ (prop tree-address) (- (prop tree-address length) 2))))
               (parent (when parentid (@ (prop opinions) parentid)))
               (text (if parent
                         (if (chain parent (has-own-property 'comment))
                             (@ parent comment)
                             "")
                         (prop text))))
          (psx
           (:div
            :class (chain "opinion-thread-depth-"
                          (concat (prop tree-address length (to-string))))
            (:vote-value :key 1 :opinion opinion) " "
            (:flag-name :key 2 :opinion opinion) " "
            (:date-stamp :key 3 :opinion opinion) " "
            (:author-long :key 4 :opinion opinion) " "
            (:reply-link :key 6 :url (@ opinion url))
            (when (has-excerpt-p opinion)
              (if (has-found-excerpt-p opinion)
                  (psx (:thread-excerpt
                        :key 7
                        :opinion opinion
                        :text text))
                  (psx (:thread-excerpt
                        :key 7
                        :opinion opinion))))
            (when (@ opinion comment)
              (psx (:div :key 8 (@ opinion comment))))))))

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
          (:div :key "x"
                "Title:" (prop title))
          (collecting
              (let ((data (%reformat-opinions (prop opinions))))
                (dolist (op (@ data 0))
                  (collect
                      (psx (:thread-opinion
                            :key (unique-id)
                            :opinions (@ data 1)
                            :text (prop text)
                            :tree-address op)))))))))

    ))
