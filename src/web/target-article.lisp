(in-package :wf/web)

(defparameter *grandchild-shift* 15)

(defun target-article ()
  (ps

    (chain js-plumb (ready (lambda ())))

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

    (def-component hilited-segment
        (psx
         (:span
          :style (create font-weight :bold)
          :class (flavor (prop opinions))
          :on-click (@ this handle-click)
          (rebreak (prop text))
          (:span :style (create position :relative) :key (unique-id)
                 :id (prop id)
                 (%make-opin-popups (if (and (not (prop not-viewable))
                                             (state viewable))
                                        (prop opinions)
                                        (list))
                                    (@ this props)))))
      get-initial-state
      (lambda () (create viewable false))
      handle-click
      (lambda (e)
        (unless (prop not-viewable)
          (set-state viewable (not (state viewable)))
          (chain e (stop-propagation))))
      display-plumbs
      (lambda ()
        (when (state viewable)
          (display-popup-plumbs
           this
           (prop id)
           (chain document (get-element-by-id (prop id))
                  parent-element parent-element parent-element)
           (prop opinions))))
      component-did-mount
      (lambda () (chain this (display-plumbs)))
      component-did-update
      (lambda () (chain this (display-plumbs)))
      component-will-unmount
      (lambda () (delete-plumbs this)))

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
                    (psx (:opinion :opinions focussed
                                   :key (unique-id)
                                   :focus (prop focus)
                                   :focusfunc (prop focusfunc)
                                   :looks (prop looks)
                                   :look-handler (prop look-handler)
                                   :tree-address (prop tree-address))))))))

    (defun %make-segments (text opins props)
      (collecting
          (let ((segpoints (excerpt-segment-points
                            (collecting (dolist (op opins)
                                          (collect (@ op 0))))
                            (length text))))
            (do-window ((start end) segpoints)
              (let ((common-data
                     (create :opinions
                             (remove-if-not
                              (lambda (itm)
                                (%overlap-p
                                 start (1- end)
                                 (@ itm 0 'text-position 0)
                                 (+ (@ itm 0 'text-position 0)
                                    (@ itm 0 'text-position 1) (- 1))))
                              opins)
                             :key (unique-id)
                             :id (strcat "lvl-"
                                         (chain props
                                                tree-address length (to-string))
                                         "-pos-"
                                         (chain end (to-string)))
                             :text (chain text (slice start end))
                             :focusfunc (@ props focusfunc)
                             :looks (@ props looks)
                             'look-handler (@ props look-handler)
                             tree-address (@ props tree-address))))
                (cond ((< (@ common-data :opinions length) 1)
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
          :key (unique-id)
          (when (prop text)
            (%make-segments (prop text)
                            (remove-if-not
                             ;;FIXME: do what with broken excerpts?
                             ;; right now they just disappear?
                             (lambda (x) (has-found-excerpt-p (@ x 0)))
                             (prop opinions))
                            (@ this props))))))

    (defun %make-opin-popups (opinions props)
      (loop for op in opinions
         for (x y) in (opinion-fan (length opinions))
         collect
           (psx (:mini-opinion :... props
                               :opinion (@ op 0)
                               :opinions (chain op (slice 1))
                               :top (+ y (lisp *unit-string*))
                               :left (+ x (lisp *unit-string*))
                               :key (unique-id)))))

    (def-component mini-opinion
        (psx
         (:div
          :id (strcat "opinid-" (prop opinion id (to-string)))
          :class (strcat "opinion " (flavor (prop opinions)))
          :on-click (@ this handle-click)
          :style (create position :absolute
                         top (prop top) left (prop left)
                         box-shadow "4px 4px 20px -2px #000"
                         ;;FIXME: Inline-table works for now. What does it mean?
                         display "inline-table")
          (:vote-value :opinion (prop opinion) :key (unique-id))
          (:flag-name :key (unique-id) :opinion (prop opinion))))
      handle-click
      (lambda (e)
        (funcall (prop focusfunc)
                 (chain (prop tree-address)
                        (concat (list (prop opinion id)))))
        (chain e (stop-propagation)))
      component-did-mount
      (lambda ()
        (unless ;;Leave unread if has comment text or replies
            (or (prop opinion comment)
                (< 0 (prop opinions length)))
          (funcall (prop look-handler) (prop opinion id)))))

    (defun %make-knob-id (treeaddress)
      (collecting-string
        (collect "knob")
        (dolist (ta treeaddress)
          (collect "-")
          (collect (chain ta (to-string))))))

    (defun %get-excerptless-opinions (opins)
      (collecting
          (dolist (o opins)
            (unless (and (@ o 0 excerpt)
                         (< 0 (@ o 0 excerpt length)))
              (collect o)))))

    ;;FIXME: this is getting too large & complicated
    (def-component general-opinion-knobdule
        (let ((opins (%get-excerptless-opinions (prop opinions))))
          (delete-plumbs this)
          (psx (:span :on-click (@ this handle-click)
                      :style (create position :relative)
                      :id (%make-knob-id (prop tree-address))
                      (:knobdule-display :opinions opins :key "x")
                      (let ((focussed (focus-parent-p (@ this props))))
                        (if (and focussed
                                 (not (and (@ focussed 0 excerpt)
                                           (< 0 (@ focussed 0 excerpt length)))))
                            (psx (:opinion :opinions focussed
                                           :key (unique-id)
                                           :focus (prop focus)
                                           :focusfunc (prop focusfunc)
                                           :tree-address (prop tree-address)
                                           :looks (prop looks)
                                           :look-handler
                                           (prop look-handler)))
                            (when (focus-p (@ this props))
                              (psx (:span :style (create position :absolute)
                                          :key 1
                                          (%make-opin-popups
                                           (if (and (not (prop not-viewable))
                                                    (state viewable))
                                               opins
                                               (list))
                                           (@ this props))))))))))
      get-initial-state
      (lambda () (create viewable false))
      handle-click
      (lambda (e)
        (unless (prop not-viewable)
          (set-state viewable (not (state viewable)))
          (chain e (stop-propagation))))
      display-plumbs
      (lambda ()
        (let ((id (%make-knob-id (prop tree-address))))
          (when (state viewable)
            (display-popup-plumbs
             this
             id
             ;;FIXME: Looks a little shaky
             (chain document (get-element-by-id id)
                    parent-element parent-element parent-element)
             (%get-excerptless-opinions (prop opinions))))))
      component-will-receive-props
      (lambda (_)
        (set-state viewable nil))
      component-did-mount
      (lambda () (chain this (display-plumbs)))
      component-did-update
      (lambda () (chain this (display-plumbs))))

    (def-component knobdule-display
        (let* ((immed (chain (prop opinions) length))
               (all
                (labels ((counter (things)
                           (let ((ln (chain things length)))
                             (if (= 1 ln)
                                 1
                                 (chain (mapcar #'counter things)
                                        (reduce (lambda (a b) (+ a b))
                                                0))))))
                  (counter (prop opinions))))
               (allstr (if (< immed all)
                           (strcat "/" (chain all (to-string)))
                           "")))
          (psx (:span :class "knobdule"
                      (strcat (chain immed (to-string)) allstr)))))

    ;;FIXME: The reference should include much more information: title, link to target
    ;; page, warstats...
    (def-component reference-line
        (psx
         (:div
          (:a :href (prop reference) "Reference"))))

    (def-component opinion
        (let* ((op (@ (prop opinions) 0))
               (tree-ad (chain (prop tree-address)
                               (concat (list (@ op id))))))
          (psx
           (:div
            :ref
            (lambda (el)
              (when el
                (let ((offset
                       (+ (lisp *grandchild-shift*)
                          (chain (position-difference
                                  (chain el parent-element parent-element)
                                  (chain el parent-element)) left))))
                  (setf (chain el style left)
                        (chain offset (to-string) (concat "px"))))))
            :on-click (@ this handle-click)
            :class "opinion"
            :key (unique-id)
            :style (create position :absolute
                           top "1em"
                           left (prop horizontal-position))
            (:div :key (unique-id)
                  :class (flavor (prop opinions (slice 1)))
                  :style (create padding "1em")
                  (:vote-value :key 1 :opinion op) " "
                  (:flag-name :key 2 :opinion op) " "
                  (:date-stamp :key 3 :opinion op) " "
                  (:author-long :key 4 :opinion op) " "
                  (:reply-link :key 6 :url (@ op url))
                  (:general-opinion-knobdule
                   :key 5
                   :... (@ this props)
                   :opinions (prop opinions (slice 1))
                   :tree-address tree-ad))
            (:hilited-text
             :key (unique-id)
             :... (@ this props)
             :text (@ op comment)
             :opinions (prop opinions (slice 1))
             :tree-address tree-ad)
            (when (@ op reference)
              (psx (:reference-line :key "x1" :reference (@ op reference)))))))
      handle-click
      (lambda (e)
        (chain e (stop-propagation)))
      component-did-mount
      (lambda ()
        (funcall (prop look-handler) (@ (prop opinions) 0 id))))

    (def-component target-root-article
        (psx
         (:div
          :style (create position :absolute width "80%")
          :on-click (@ this handle-click)
          (:target-title
           :key 0
           :... (@ this props)
           " "
           (:general-opinion-knobdule
            :key 1
            :... (@ this props)
            :focus (state focus)
            :opinions (prop opinions)
            :tree-address (list)
            :focusfunc (@ this focus-func)))
          (:hilited-text
           :key 2
           :text (prop text)
           :opinions (prop opinions)
           :focus (state focus)
           :focusfunc (@ this focus-func)
           :tree-address (list)
           :looks (prop looks)
           :look-handler (prop look-handler))))
      handle-click
      (lambda () (set-state focus (list)))
      focus-func
      (lambda (new-focus) (set-state focus new-focus))
      get-initial-state
      (lambda () (create focus (prop focus))))

    ))
