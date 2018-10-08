(in-package :wf/web)

(defparameter *grandchild-shift* 15)

(defun target-article ()
  (ps

    (setf tool-tip (@ (require "react-portal-tooltip") default))

    (defun focus-p (props?)
      (let ((tad (@ props? tree-address))
            (foc (@ props? focus)))
        (when (and tad foc (eq (@ tad length) (@ foc length)))
          (loop for x in tad
             for y in foc
             unless (equal x y) do (return-from focus-p nil)
             finally (return-from focus-p t)))))

    (defun focus-parent-p (props?)
      (let ((tad (@ props? tree-address))
            (foc (@ props? focus)))
        (when (and tad foc (< (@ tad length) (@ foc length)))
          (loop for x in tad
             for y in foc
             unless (equal x y) do (return-from focus-parent-p nil))
          (dolist (op (@ props? opinions))
            (when (eq (@ op 0 id) (@ foc (@ tad length)))
              (return-from focus-parent-p op))))))

    (defun popup-side (position)
      (if position
          (if (< (* (chain -math (abs (@ position right))) 2)
                 (chain -math (abs (@ position left))))
              "right"
              "left")
          "left"))

    (defun immediate-children-ids (id opinstore)
      (if id
          (let ((len (@ (getprop opinstore id) 'tree-address length)))
            (collecting
                (dolist (itm (all-descendant-ids id opinstore))
                  (when (eq (1+ len) (@ (getprop opinstore itm) 'tree-address length))
                    (collect itm)))))
          (collecting
              (do-keyvalue (k opin opinstore)
                (when (eq 1 (@ opin 'tree-address length))
                  (collect k))))))

    (defun all-descendant-ids (id opinstore)
      (if id
          (let* ((trad (@ (getprop opinstore id) 'tree-address))
                 (len (@ trad length)))
            (collecting
                (do-keyvalue (k opin opinstore)
                  (when (< len (@ opin 'tree-address length))
                    (when (array-equal trad (chain (@ opin 'tree-address) (slice 0 len)))
                      (collect k))))))
          (chain -object (keys opinstore))))

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
                  (when (and focussed
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
                             (mapcar (lambda (id) (getprop opstore id)) opins)
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
          :on-mouse-up (@ this selection-change)
          :on-key-press (@ this selection-change)
          (when (prop text)
            (%make-segments (prop text)
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
                   (excerpt (get-location-excerpt (create-textdata (prop text))
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

    (defun excerpt-reply-link (url excerpt)
      (let ((exstr (when excerpt (strcat "&excerpt=" (encode-u-r-i-component excerpt)))))
        (strcat
         "/opinion/?target="
         (encode-u-r-i-component url)
         exstr)))

    (def-component sub-opinion-list
        (psx
         (:div
          :key 1 :class "sub-opinion-list"
          (:a :key 2 :class "action"
              :href (excerpt-reply-link (if (not-empty (prop tree-address))
                                    (@ (getprop (prop opinion-store)
                                                (last-list (prop tree-address)))
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
              (psx
               (:opinion-info
                :key 3
                :... (@ this props)
                :opinion (getprop (prop opinion-store) (prop excerpt-opinions 0))))))))

    ;;FIXME: Needs different look, more long form.
    (def-component opinion-info
        (let* ((opinion (prop opinion))
               (reference (when (and (prop references)
                                     (chain (prop references)
                                            (has-own-property (prop opinion id))))
                            (getprop (prop references) (prop opinion id)))))
          (psx
           (:div
            :... (format-styling-data (@ this props))
            :on-click (lambda ()
                        (setf (@ window location) (@ opinion url)))
            (:vote-value :key 1 :opinion opinion) " "
            (:flag-name :key 2 :opinion opinion) " "
            (:date-stamp :key 3 :opinion opinion) " "
            (:author-long :key 4 :opinion opinion) " "
            (:display-warstats2 :key 5)
            (:div :key 9 :class "opinion-comment-wrapper"
                  (when (@ opinion comment)
                    (psx (:div :key 8 :class "opinion-comment" (@ opinion comment))))
                  (:div
                   :key 12
                   :class "opinion-extras"
                   (when reference
                     (psx (:reference
                           :key 10
                           :... (prop reference)
                          :styling-data
                          (format-reference-styling-data reference))))))))))

    (defun %get-excerptless-opinions (opins)
      (collecting
          (dolist (o opins)
            (unless (and (@ o 0 excerpt)
                         (< 0 (@ o 0 excerpt length)))
              (collect o)))))

    (def-component excerptless-opinions
        (psx
         (:div
          :style (create 'margin-top "2em")
          (:h3 :key 1 "General Opinions:")
          (collecting
              (dolist (op (prop tree-addresses))
                (when (> 2 (@ op length)) ;opinion is on root article
                  (let* ((opid (elt op 0))
                         (opin (getprop (prop opinion-store) opid)))
                    (unless (has-excerpt-p opin)
                      (collect
                          (psx
                           (:thread-opinion
                            :key (unique-id)
                            :... (@ this props)
                            :tree-address op
                            :styling-data (format-styling-data
                                           (copy-merge-all (@ %thisref props)
                                                           (create
                                                            'tree-address op)))
                            :reference (getprop (prop references) opid))))))))))))

    (def-component target-root-article
        (psx
         (:div
          :style (create position :absolute width "80%" 'margin-bottom "20em")
          :on-click (@ this handle-click)
          (:target-title
           :key 0
           :... (@ this props)
           :reply-excerpt (state :reply-excerpt)
           :reply-offset (state :reply-offset)
           " ")
          (:h3
           :style (create 'font-style "italic" :background "lightgrey")
           :key 2
           "Text from article at " (url-domain (prop url)))
          (:hilited-text
           :key 3
           :text (prop text)
           :opinions (prop opinions)
           :focus (state focus)
           :focusfunc (@ this focus-func)
           :root-target-url (prop url)
           :tree-address (list)
           :warstats (prop warstats)
           :opinion-store (prop opinion-store)
           :dispatch (@ this dispatch)
           :looks (prop looks)
           :look-handler (prop look-handler))
          (:excerptless-opinions
           :key 4
           :... (@ this props))))
      get-default-state
      (lambda ()
        (create 'reply-excerpt "" 'reply-offset nil))
      handle-click
      (lambda () (set-state focus (list)))
      focus-func
      (lambda (new-focus) (set-state focus new-focus))
      get-initial-state
      (lambda () (create focus (prop focus)))
      dispatch
      (lambda (action)
        (when (eq (@ action type) :selection)
          (set-state :reply-excerpt (@ action excerpt)
                     :reply-offset (@ action offset)))))

    ))

