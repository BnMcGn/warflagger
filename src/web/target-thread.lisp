(in-package :wf/web)

;; This a (currently experimental) comment-style layout of the opinion tree of a
;; root target

(defparameter *excerpt-margin* 50)

(define-ps-lib target-thread ()
  (ps

    (defvar on-screen (@ (require "react-on-screen") default))

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
                 (tstart (previous-break (prop text) estart))
                 (tend (next-break (prop text) eend))
                 (leading-context (prop text (slice (if tstart (1+ tstart) 0)
                                                    estart)))
                 (excerpt (prop text (slice estart eend)))
                 (trailing-context (prop text (slice eend (or tend tlength))))
                 (classes (collecting
                              (collect "thread-excerpt")
                              (when tstart (collect "fade-start"))
                              (when tend (collect "fade-end")))))

              (psx
               (:div
                :... (create :found-excerpt "true")
                :class (chain classes (join " "))
                (:span :key 1 (rebreak leading-context))
                (:span :key 2 :class (flavor (prop warstats) (prop opinion id))
                       (rebreak excerpt))
                (:span :key 3 (rebreak trailing-context)))))
            (psx
             (:div
              :... (create :found-excerpt nil)
              (prop opinion excerpt)))))

    (def-component reference
        (let ((styling (prop styling-data)))
          (unless styling
            (setf styling (create :data-replies-total 0))
            (format-looks-data styling :root (prop looks)))
          (psx
           (:div
            :class "reference"
            :... styling
            (:headline
             :key 1
             :title (prop headline title)
             :domain (prop reference-domain)
             :url (prop warflagger-link)
             :external-link (when (not (equal (prop reference) (prop warflagger-link)))
                              (prop reference))
             (:display-warstats2))))))

    (def-component question
        (psx
         (:div
          :class "question")))

    (def-component thread-opinion
        (let* ((opinion (@ (prop opinion-store) (list-last (prop tree-address))))
               (parentid (when (< 1 (prop tree-address length))
                           (@ (prop tree-address) (- (prop tree-address length) 2))))
               (parent (when parentid (getprop (prop opinion-store) parentid)))
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
            :on-click (lambda (e)
                        (setf (@ window location)
                              (make-opinionid-url (@ opinion id)))
                        (chain e (stop-propagation)))
            (:opinion-icon :key 1 :opinion opinion
                           :look-handler (prop look-handler) :looks (prop looks)) " "
            (:flag-name :key 2 :opinion opinion) " "
            (:date-stamp :key 3 :opinion opinion) " "
            (:author-long :key 4 :opinion opinion) " "
            (:display-warstats2 :key 5)
            (:reply-link :key 6 :url (@ opinion url)
                         :excerpt (state :reply-excerpt) :offset (state :reply-offset))
            (:div
             :key 9 :class "opinion-comment-wrapper"
             (when (has-excerpt-p opinion)
               (if (has-found-excerpt-p opinion)
                   (psx (:thread-excerpt
                         :key 7
                         :opinion opinion
                         :warstats (prop warstats)
                         :text text))
                   (psx (:thread-excerpt
                         :key 7
                         :opinion opinion
                         :warstats (prop warstats)))))
             (when (@ opinion comment)
               (psx
                (:hilited-text
                 :key 8
                 :text (@ opinion comment)
                 :tree-address (prop tree-address)
                 :focus (prop tree-address)
                 :warstats (prop warstats)
                 :opinion-store (prop opinion-store)
                 :dispatch (@ this dispatch)
                 :hide-popup t
                 :looks (prop looks)
                 :look-handler (prop look-handler))))
             (:div
              :key 12
              :class "opinion-extras"
              (when (prop reference)
                (psx (:reference :key 10
                                 :... (prop reference)
                                 :styling-data
                                 (format-reference-styling-data (prop reference)))))
              (when (prop question)
                (psx (:question :key 11
                                        ;:... (prop question)
                                ))))))))
      get-initial-state
      (lambda ()
        (create))
      dispatch
      (lambda (action)
        (when (eq (@ action type) :selection)
          (set-state :reply-excerpt (@ action excerpt)
                     :reply-offset (@ action offset))))
      component-did-update
      (lambda ()
        (when (prop is-visible)
          (funcall (prop look-handler) (list-last (prop tree-address))))))

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

    (def-component target-root-thread
        (psx
         (:div :style (create position "relative")
          (:target-title :key "x" :... (@ this props))
          (collecting
              (let ((data (%reformat-opinions (prop opinions))))
                (dolist (op (prop tree-addresses))
                  (collect
                      (psx (:on-screen
                            :key (list-last op)
                            ;; :once t ;; Isn't working anyways.
                            (:thread-opinion
                             :opinions (prop opinions)
                             :opinion-store (prop opinion-store)
                             :text (prop text)
                             :styling-data
                             (format-styling-data
                              (copy-merge-all (@ %thisref props)
                                              (create
                                               'opinion-store (@ data 1)
                                               'tree-address op
                                               'opinions (@ data 1))))
                             :tree-address op
                             :warstats (prop warstats)
                             :looks (prop looks)
                             :look-handler (prop look-handler)
                             :reference
                             (getprop (prop references) (list-last op))))))))))))

    ))
