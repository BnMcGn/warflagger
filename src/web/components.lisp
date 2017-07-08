(in-package :wf/web)

(defparameter *grandchild-shift* 15)

(define-parts target-components
  :@css-link "/static/css/target.css"
  :@javascript #'distribute-ranks
  :@javascript #'titlebar-components
  :@javascript-link "/static/javascript/jsPlumb-2.0.5.js"
  :@javascript
  (lambda ()
    (ps (chain js-plumb (ready (lambda ())))))
  :@javascript
  (lambda ()
    (ps

      (let ((counter 0))
        (defun unique-id ()
          (incf counter)))

      (defun rebreak (text)
        (chain (collecting
                   (dolist (string (chain text (split (lisp-raw "\"\\n\""))))
                     (collect string)
                     (collect (psx (:br :key (unique-id))))))
               (slice 0 -1)))

      (defun opinion-p (itm)
        (and (not (atom itm))
             (chain itm (has-own-property "votevalue"))))

      (defun %overlap-p (start1 end1 start2 end2)
        (not (or (> start1 end2) (> start2 end1))))

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

      ;;Find all the indices where excerpts start or stop.
      (defun excerpt-segment-points (opset end)
        (chain
         (collecting-set
             (dolist (itm opset)
               (collect (@ itm text-position 0))
               (collect (+ (@ itm text-position 0) (@ itm text-position 1))))
           (collect 0)
           (collect (1+ end)))
         (sort (lambda (a b) (- a b)))))

      (def-component hilited-segment
          (progn
            (when (or (prop not-viewable) (not (state viewable)))
              (delete-plumbs this))
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
                                            ([]))
                                        (@ this props))))))
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
                    parent-element parent-element)
             (prop opinions))))
        component-did-mount
        (lambda () (chain this (display-plumbs)))
        component-did-update
        (lambda () (chain this (display-plumbs))))

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

      (def-component
          hilited-text
          (psx
           (:div
            :class
            (if (focus-p (@ this props)) "hilited" "hilited-parent")
            :key (unique-id)
            (when (prop text)
              (%make-segments (prop text)
                              (remove-if-not
                               (lambda (x)
                                 (chain (@ x 0) (has-own-property :excerpt)))
                               (prop opinions))
                              (@ this props))))))

      (defun %make-opin-popups (opinions props)
                                        ;(setf opinions (mock-opinions 30))
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
                           box-shadow "4px 4px 20px -2px #000")
            (:vote-value :opinion (prop opinion) :key (unique-id))
            (:flag-name :key (unique-id) :opinion (prop opinion))))
        handle-click
        (lambda (e)
          (funcall (prop focusfunc)
                   (chain (prop tree-address)
                          (concat (list (prop opinion id)))))
          (chain e (stop-propagation))))

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

      (def-component general-opinion-knobdule
          (progn
            (delete-plumbs this)
            (let ((opins (%get-excerptless-opinions (prop opinions))))
              (psx (:span :on-click (@ this handle-click)
                          :id (%make-knob-id (prop tree-address))
                          (when (not-empty opins) " X")
                          (:span :style (create position :relative) :key 1
                                 (%make-opin-popups
                                  (if (and (not (prop not-viewable))
                                           (state viewable))
                                      opins ([]))
                                  (@ this props)))))))
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
               (chain document (get-element-by-id id) parent-element)
               (%get-excerptless-opinions (prop opinions))))))
        component-did-mount
        (lambda () (chain this (display-plumbs)))
        component-did-update
        (lambda () (chain this (display-plumbs))))


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
               :tree-address tree-ad))))
        handle-click
        (lambda (e)
          (chain e (stop-propagation))))

      (def-component target-root
          (psx
           (:div
            :on-click (@ this handle-click)
            (:div :key 1
                  :class (flavor (prop opinions))
                  (:h3
                   "Target Page:"
                   (:a :href (prop url) :key 1 (prop title))
                   (:general-opinion-knobdule
                    :key 2
                    :... (@ this props)
                    :opinions (prop opinions)
                    :tree-address (list)
                    :focusfunc (@ this focus-func))
                   (:reply-link :url (prop url))))
            (:hilited-text
             :key 2
             :text (prop text)
             :opinions (prop opinions)
             :focus (state focus)
             :focusfunc (@ this focus-func)
             :tree-address (list))))
        handle-click
        (lambda () (set-state focus (list)))
        focus-func
        (lambda (new-focus) (set-state focus new-focus))
        get-initial-state
        (lambda () (create focus (prop focus))))


      )))

(defpsmacro mock-opinions (quantity)
  `(list
    ,@(gadgets:collecting
        (dotimes (i quantity)
          (gadgets:collect
              `(list (create :flag (list "Negative"
                                         ,(whichever "Spam" "Inflammatory" "Disagree" "Dislike" "Obscene" "Disturbing" "AlreadyAnswered" "LogicalFallacy" "AdHominem" "FromAuthority" "NeedsReference" "RaiseQuestion"))

                             :votevalue ,(whichever (- 1) 0 1))))))))
