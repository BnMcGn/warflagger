(in-package :wf/web)

(define-ps-lib displayables ()
  (ps

    (setf tool-tip (@ (require "react-portal-tooltip") default))

    ;;FIXME: Overall use of url, external-link, warflagger-link is inconsistent.
    (def-component target-title
        (psx
         (:div
          :... (format-styling-data (@ this props))
          (:h3
           :class (strcat (flavor-from-own-warstats (prop warstats root))
                          "-old target-title")
           (or (prop intro-text) "Target Page: ")
           (:headline :key 1
                      :title (prop title)
                      :url (prop warflagger-link)
                      :external-link (prop url))
           (:display-warstats2 :key 2)
           (prop children)
           (unless (prop hide-reply)
             (psx (:reply-link
                   :key 3
                   :url (prop url)
                   :excerpt (prop reply-excerpt)
                   :offset (prop reply-offset))))
           (when (prop show-count)
             (psx (:reply-count :key 4 :warstats (prop warstats root))))))))

    (defun popup-side (position)
      (if position
          (if (< (* (chain -math (abs (@ position right))) 2)
                 (chain -math (abs (@ position left))))
              "right"
              "left")
          "left"))

    (defun %find-parent-hilited (element)
      (if element
          (if (equal "hilited" (@ element class-name))
              element
              (%find-parent-hilited (@ element parent-element)))))

    (defun is-selection-in-single-hilited-text? (selection)
      (let ((parent1 (%find-parent-hilited (@ selection anchor-node))))
        (and (not (@ selection is-collapsed))
             parent1
             (eq parent1 (%find-parent-hilited (@ selection focus-node))))))

    (def-component hilited-segment
        (psx
         (:span
          :id (prop id)
          :style (create font-weight :bold position :relative)
          :class (apply #'flavor (prop warstats) (prop excerpt-opinions))
          :on-mouse-enter (@ this handle-mouse-enter)
          :on-mouse-leave (@ this handle-mouse-leave)
          :ref
          (lambda (el)
            (when el
              (let ((res (position-difference
                          (chain document (get-element-by-id (prop hilited-text-id)))
                          el)))
                (unless (chain _ (is-equal (state position) res))
                  (set-state position res)))))
          (rebreak (prop text))
          (:span :class "segment-count" :key (unique-id)
                 (let ((count (%get-replies-count (prop excerpt-opinions) (@ this props))))
                   (if (< 1 count) count "")))
          (:tool-tip :key "a1"
                     :style (popup-style)
                     :active (and (state viewable) (not (prop hide-popup)))
                     :position "bottom"
                     :group "one"
                     :arrow (popup-side (state position))
                     :parent (strcat "#" (prop id))
                     (:sub-opinion-list :... (@ this props)))))
      get-initial-state
      (lambda () (create viewable false position nil))
      handle-mouse-enter
      (lambda (e)
        (when (prop dispatch)
          (funcall (prop dispatch)
                   (create :type 'set-indicated
                           :data
                           (immediate-children-ids (prop id-of-text) (prop opinion-store)))))
        (set-state viewable true))
      handle-mouse-leave
      (lambda (e)
        (when (prop dispatch)
          (funcall (prop dispatch)
                   (create :type 'clear-indicated
                           :data
                           (immediate-children-ids (prop id-of-text) (prop opinion-store)))))
        (set-state viewable false)))

    (def-component plain-segment
        (psx
         (:span :style (create font-weight :normal)
                (rebreak (prop text)))))

    (def-component parent-segment
        (let ((focussed (focus-parent-p (@ this props))))
          (psx
           (:span :style (create font-weight :bold position :relative)
                  :class (if focussed "parent-active" "parent-inactive")
                  :key (unique-id)
                  (rebreak (prop text))
                  (when (and nil ;;Disable for  now
                             focussed
                             (eql (prop last-char-pos)
                                  (+ (@ focussed 0 text-position 0)
                                     (@ focussed 0 text-position 1))))
                    ;;FIXME: obsolete
                    (psx (:opinion :opinions focussed
                                   :key (unique-id)
                                   :focus (prop focus)
                                   :focusfunc (prop focusfunc)
                                   :looks (prop looks)
                                   :look-handler (prop look-handler)
                                   :tree-address (prop tree-address))))))))

    (defun %make-segments (text props)
      (collecting
          (let* ((current-id (when (@ props tree-address)
                               (list-last (@ props tree-address))))
                 (opstore (@ props opinion-store))
                 (opins (immediate-children-ids current-id opstore))
                 (segpoints (excerpt-segment-points
                             (collecting
                                 (dolist (id opins)
                                   (let ((opin (getprop opstore id)))
                                     (when (has-found-excerpt-p opin)
                                       (collect opin)))))
                             (length text))))
            (do-window ((start end) segpoints)
              (let* ((id (strcat "lvl-"
                          (chain props
                                 tree-address length (to-string))
                          "-pos-"
                          (chain end (to-string))))
                     (common-data
                      (create 'excerpt-opinions
                              (remove-if-not
                               (lambda (id)
                                 (let ((opin (getprop opstore id)))
                                   (and (has-found-excerpt-p opin)
                                        (%overlap-p
                                         start (1- end)
                                         (@ opin 'text-position 0)
                                         (+ (@ opin 'text-position 0)
                                            (@ opin 'text-position 1) (- 1))))))
                               opins)
                              :key id
                              :id id
                              :text (chain text (slice start end))
                              :id-of-text current-id
                              :focusfunc (@ props focusfunc)
                              :looks (@ props looks)
                              :warstats (@ props warstats)
                              :references (@ props references)
                              'opinion-store (@ props opinion-store)
                              'hilited-text-id (@ props hilited-text-id)
                              'root-target-url (@ props root-target-url)
                              'look-handler (@ props look-handler)
                              'hide-popup (@ props hide-popup)
                              :dispatch (@ props dispatch)
                              tree-address (@ props tree-address))))
              (cond ((< (@ common-data 'excerpt-opinions length) 1)
                       (collect
                           (psx (:plain-segment
                                 :... common-data))))
                      ((focus-p props)
                       (collect
                           (psx (:hilited-segment
                                 :... common-data))))
                      (t
                       (collect
                           (psx (:parent-segment
                                 :... common-data
                                 :focus (@ props focus)
                                 :last-char-pos end))))))))))

    (def-component hilited-text
        (psx
         (:div
          :class
          (if (focus-p (@ this props)) "hilited" "hilited-parent")
          :key (state id)
          :id (state id)
          :on-click (lambda (e) (chain e (stop-propagation)))
          :on-mouse-up (@ this selection-change)
          :on-key-press (@ this selection-change)
          (when (prop text)
            ;;Stray whitespace can confuse location of reply to excerpt, hence the trim
            (%make-segments (chain (prop text) (trim))
                            (set-copy (@ this props)
                                      'hilited-text-id
                                      (state id))))))
      get-initial-state
      (lambda ()
        (create :id (strcat "hilited-text-" (unique-id))))
      selection-change
      (lambda (ev)
        (when (prop dispatch)
          (if (is-selection-in-single-hilited-text? (chain rangy (get-selection)))
            (let* ((textel (chain document (get-element-by-id (state id))))
                   (range (chain rangy (get-selection) (get-range-at 0)
                                 (to-character-range textel)))
                   (excerpt (get-location-excerpt (create-textdata (chain (prop text) (trim)))
                                                  (@ range start) (@ range end))))
              (funcall
               (prop dispatch)
               (create :type :selection
                       :range range
                       :excerpt (getprop excerpt 0)
                       :offset (getprop excerpt 1))))
            (funcall
             (prop dispatch)
             (create :type :selection
                     :range false :excerpt "" :offset 0))))))

    (defun %get-replies-count (opinion-ids props)
      (let ((total 0))
        (dolist (op opinion-ids)
          (incf total))
          ;(incf total (@ props warstats op replies-total)))
        total))

    (def-component sub-opinion-list
        (psx
         (:div
          :key 1 :class "sub-opinion-list"
          (:a :key 2 :class "action"
              :href (excerpt-reply-link (if (not-empty (prop tree-address))
                                    (@ (getprop (prop opinion-store)
                                                (list-last (prop tree-address)))
                                       url)
                                    (prop root-target-url))
                                (prop text))
              "Reply to the excerpt")
          (if (< 1 (@ (prop excerpt-opinions) length))
              (collecting
                  (dolist (itm (prop excerpt-opinions))
                    (collect
                        (let ((opin (getprop (prop opinion-store) itm)))
                          (psx
                           (:opinion-summary
                            :key (unique-id)
                            :... (@ this props)
                            :tree-address (@ opin tree-address)
                            :opid itm))))))
              (let ((opin (getprop (prop opinion-store) (prop excerpt-opinions 0))))
                (psx
                 (:opinion-info
                  :key 3
                  :... (@ this props)
                  :tree-address (@ opin tree-address)
                  :opinion opin)))))))

;;; Opinion-info is used in popups and sub-opinion-list. Not meant to be a complete view of opinion.
    (def-component opinion-info
        (let* ((opinion (prop opinion))
               (refdat (when (and (prop references)
                                  (chain (prop references)
                                         (has-own-property (prop opinion id))))
                         (getprop (prop references) (prop opinion id)))))
          (psx
           (:div
            :style (create :display "grid" grid-template-columns "30px auto"
                           margin-left "0px" background-color "white")
            :... (format-styling-data (@ this props))
            :on-click (lambda ()
                        (setf (@ window location) (make-opinionid-url (@ opinion id))))
            (:opinion-icon :key 1 :opinion opinion)
            (:div
             :key 2
             :style (create :display "grid"
                            align-items "center"
                            grid-template-columns "auto auto auto auto auto"
                            grid-template-rows "1.8em"
                            column-gap "0.5em"
                            :border "solid 3px black")
             (:flag-name :key 2 :opinion opinion)
             (:date-stamp :key 3 :opinion opinion)
             (:author-long :key 4 :opinion opinion)
             (:display-warstats2 :key 5)
             (:div :key 6 :class "opinion-comment-wrapper"
                   :style (create grid-column-start 1 grid-column-end 6)
                   (when (@ opinion comment)
                     (psx (:div :key 8 :class "opinion-comment" (rebreak (@ opinion comment)))))
                   (:div
                    :key 12
                    :class "opinion-extras"
                    (when refdat
                      (psx (:reference
                            :key 10
                            :... refdat
                            :minify t
                            :styling-data
                            (format-reference-styling-data refdat)))))))))))

    (def-component excerptless-opinions
        (let* ((ta-len (when (prop tree-address) (@ (prop tree-address) length)))
               (idlist
                (if (not-empty (prop tree-address))
                    (remove-if-not
                     (lambda (x)
                       (and (< ta-len (@ x length))
                            (eql (getprop (prop tree-address) (1- ta-len))
                                 (getprop x (1- ta-len)))))
                     (prop tree-addresses))
                    (remove-if (lambda (x) (< 1 (@ x length))) (prop tree-addresses))))
               (idlist (mapcar #'list-last idlist)))
          (psx
           (:div
            :class "excerptless"
            :style (create 'margin-top "2em")
            (when (< 0 (@ idlist length))
              (psx (:h3 :key 1 "Replies:")))
            (:div
             :key 2
             :style (create :position :relative :left (or (prop comment-left) "0px"))
             (collecting
               (dolist (opid idlist)
                 (let ((opin (getprop (prop opinion-store) opid)))
                   (unless (has-excerpt-p opin)
                     (collect
                         (psx
                          (:on-screen
                           :key opid
                           (:thread-opinion
                            :... (@ this props)
                            :tree-address (@ opin tree-address)
                            :styling-data (format-styling-data
                                           (set-copy (@ this props)
                                                     'tree-address (@ opin tree-address)))
                            :reference (getprop (prop references) opid))))))))))))))

;;; Opinion-summary is used to display opinions in one line situations. It may be displayed with
;;; tree address icons. 
    (def-component opinion-summary
        (let* ((opinion (@ (prop opinion-store)
                          (if (prop opid)
                              (prop opid)
                              (list-last (prop tree-address)))))
               ;; FIXME: Can have multiple copies of opinion on same page. Shouldn't use id!
               (id (strcat "opinion-summary-" (@ opinion id))))
          (psx
           (:div
            :id id
            :... (or (prop styling-data)
                     (format-styling-data (set-copy (@ this props) :opinion opinion)))
            :class "opinion-summary"
            (if (prop opid)
                (psx (:opinion-icon :key 1 :opinion opinion
                                   :look-handler (prop look-handler)))
                (psx (:display-tree-address :key 1 :tree-address (prop tree-address)
                                            :opinion-store (prop opinion-store)
                                            :warstats (prop warstats)
                                            :looks (prop looks)
                                            :look-handler (prop look-handler))))
            (:flag-name :key 2 :opinion opinion) " "
            (:date-stamp :key 3 :opinion opinion) " "
            (:author-long :key 4 :opinion opinion) " "
            (:display-warstats2 :key 5)
            (:reply-link :key 6 :url (@ opinion url)))))
      component-did-catch
      (lambda (err err-info)
        (say "Something missing in opinion-summary")
        (say (@ this props))
        (say err)
        (say err-info)))


    (defun previous-break (text index)
      (let ((res (chain text (substring 0 index) (last-index-of #\linefeed))))
        (when (<= 0 res)
          res)))

    (defun next-break (text index)
      (let ((res (chain text (substring (1+ index)) (index-of #\linefeed))))
        (when (<= 0 res)
          (+ res 1 index))))

    ;;;Used to show the context of an excerpt, especially when the full text is not being displayed.
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
          (when (prop minify)
            (setf (@ styling :data-reference-minify) t))
          (psx
           (:div
            :class "reference"
            :... styling
            (:headline
             :key 1
             :title (prop headline title)
             :domain (prop reference-domain)
             :url (prop warflagger-link)
             :external-link (unless (prop minify)
                              (when (not (equal (prop reference) (prop warflagger-link)))
                                (prop reference)))
             (:display-warstats2))))))

    (def-component question
        (psx
         (:div
          :class "question"
          ;;FIXME: would be nice if this could happen in comment summary
          :style (create 'white-space "nowrap" overflow "hidden" 'text-overflow "ellipsis")
          :... (prop styling-data)
          (:span
           (if (prop opinion)
               (psx (:a :href (make-opinionid-url (prop opinion id))
                        :key 1
                        (:comment-summary :... (@ this props))))
               (psx (:comment-summary :... (@ this props))))))))

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


    ))

