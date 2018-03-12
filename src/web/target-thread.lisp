(in-package :wf/web)

;; This a (currently experimental) comment-style layout of the opinion tree of a
;; root target

(defparameter *excerpt-margin* 50)

(defun target-thread ()
  (ps

    (defun previous-break (text index)
      (let ((res (chain text (substring 0 index) (last-index-of #\linefeed))))
        (when (<= 0 res)
          res)))

    (defun next-break (text index)
      (let ((res (chain text (substring (1+ index)) (index-of #\linefeed))))
        (when (<= 0 res)
          (+ res 1 index))))

    (def-component thread-excerpt
        (if (prop text)
            (let*
                ((tlength (prop text length))
                 (estart (prop opinion text-position 0))
                 (eend (+ (prop opinion text-position 1) estart))
                 (tstart (1+ (previous-break (prop text) estart)))
                 (tend (next-break (prop text) eend))
                 (leading-context (prop text (slice (or tstart 0) estart)))
                 (excerpt (prop text (slice estart eend)))
                 (trailing-context (prop text (slice eend (or tend tlength))))
                 (classes (collecting
                              (collect "thread-excerpt")
                              (when tstart (collect "fade-start"))
                              (when tend (collect "fade-end")))))

              (psx
               (:div
                :!found-excerpt "true"
                :class (chain classes (join " "))
                (:span (rebreak leading-context))
                ;;FIXME: quick hack
                (:span :class (flavor (list (list (prop opinion)))) (rebreak excerpt))
                (:span (rebreak trailing-context)))))
            (psx
             (:div
              :!found-excerpt nil
              (prop opinion excerpt)))))

    (def-component reference
        (psx
         (:div
          :class "reference"
          :... (or (prop styling-data) (create :data-replies-total 0))
          (:headline
           :key 1
           :title (prop headline title)
           :domain (prop reference-domain)
           :url (prop warflagger-link)
           :external-link (when (not (equal (prop reference) (prop warflagger-link)))
                            (prop reference))
          (:display-warstats2)))))

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
            :class (chain "opinion-thread depth-"
                          (concat (prop tree-address length (to-string))))
            :... (prop styling-data)
            (:vote-value :key 1 :opinion opinion) " "
            (:flag-name :key 2 :opinion opinion) " "
            (:date-stamp :key 3 :opinion opinion) " "
            (:author-long :key 4 :opinion opinion) " "
            (:display-warstats2 :key 5)
            (:reply-link :key 6 :url (@ opinion url))
            (:div :key 9 :class "opinion-comment-wrapper"
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
               (psx (:div :key 8 :class "opinion-comment" (@ opinion comment))))
             (when (prop reference)
               (psx (:reference :key 10
                                :... (prop reference)
                                :styling-data
                                (format-reference-styling-data (prop reference))))))))))

    (defun sort-compare-opinions (opa opb)
      ;;FIXME: Special treatment of missing excerpt opinions?
      (let ((posa (and (chain opa (has-own-property 'text-position))
                       (< 0 (@ opa 'text-position length))
                       (@ opa 'text-position 0)))
            (posb (and (chain opb (has-own-property 'text-position))
                       (< 0 (@ opb 'text-position length))
                       (@ opb 'text-position 0))))
        (if posa
            (if posb
                (- posa posb)
                1) ;;Excerptless comes first
            (if posb
                -1 ;; Ditto
                (- (@ opa datestamp) (@ opb datestamp))))))

    (defun %reformat-opinions (opins)
      (let* ((opinstore (create))
             (opins
              (collecting
                  (labels ((proc (tree address)
                             (dolist (branch tree)
                               (let ((newadd (chain address (concat (@ branch 0 id)))))
                                 (setf (@ opinstore (@ branch 0 id)) (@ branch 0))
                                 (collect newadd)
                                 (when (< 1 (@ branch length))
                                   (proc (chain branch (slice 1)) newadd))))))
                    (proc opins (list))))))
        ;;FIXME: can't sort here. They're trees!
        ;; will need to think out sorting.
        ;;(chain opins (sort sort-compare-opinions))
        (list opins opinstore)))

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
                            :styling-data
                            (format-styling-data
                             (copy-merge-all (@ %thisref props)
                                            (create
                                             'opinion-store (@ data 1)
                                             'tree-address op
                                             'opinions (@ data 1))))
                            :tree-address op
                            :reference
                            (getprop (prop references) (list-last op)))))))))))

    ))