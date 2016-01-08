(in-package :wf/web)

(defparameter *grandchild-shift* 15)

(define-parts target-components
  (add-part :@css "/static/css/target.css")
  (add-part :@javascript #'distribute-ranks)
  (add-part
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

       (defun %overlap-p (start1 end1 start2 end2)
         (not (or (> start1 end2) (> start2 end1))))

       (defun focus-p (props?)
         (let ((tad (@ props? tree-address))
               (foc (@ props? focus)))
           (when (eq (@ tad length) (@ foc length))
             (loop for x in tad
                   for y in foc
                   unless (equal x y) do (return-from focus-p nil)
                     finally (return-from focus-p t)))))

       (defun focus-parent-p (props?)
         (let ((tad (@ props? tree-address))
               (foc (@ props? focus)))
           (when (< (@ tad length) (@ foc length))
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
                (collect (+ (@ itm text-position 0) (@ itm text-position 1) 1)))
            (collect 0)
            (collect end))
          (sort (lambda (a b) (- a b)))))

       (def-component hilited-segment
           (psx
            (:span
             :style (create font-weight :bold)
             :class (flavor (prop opinions))
             :on-click (@ this handle-click)
             (rebreak (prop text))
             (:span :style (create position :relative) :key (unique-id)
                    (%make-opin-popups (if (and (not (prop not-viewable))
                                                (state viewable))
                                           (prop opinions)
                                           ([]))
                                       (@ this props)))))
         get-initial-state
         (lambda () (create viewable false))
         handle-click
         (lambda (e)
           (unless (prop not-viewable)
                       (set-state viewable (not (state viewable)))
                       (chain e (stop-propagation)))))

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
                                        (@ focussed 0 text-position 1) 1)))
                       (psx (:opinion :opinions focussed
                                      :key (unique-id)
                                      :focus (prop focus)
                                      :tree-address
                                      (chain
                                       (prop tree-address)
                                       (concat
                                        (list (@ focussed 0 id)))))))))))

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
                                           (@ itm 0 'text-position 1))))
                                     opins)
                               :key (unique-id)
                               :text (chain text (slice start end))
                               :focus-func (@ props focus-func)
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

       (def-component flag-display
           (psx
            (:span (prop flag 1))))

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
           (let ((vv (prop opinion votevalue)))
             (psx
              (:div
               :class (strcat "opinion " (flavor (prop opinions)))
               :on-click (@ this handle-click)
               :style (create position :absolute top (prop top) left (prop left))
               (:b :key (unique-id)
                  (case vv
                     (-1 "-") (0 "o") (1 "+")))
               (:flag-display :key (unique-id) :flag (prop opinion flag)))))
         handle-click
         (lambda (e)
           (funcall (prop :focus-func)
                    (chain (prop tree-address)
                           (concat (list (prop opinion id)))))
           (chain e (stop-propagation))))

       (def-component opinion
           (let ((op (@ (prop opinions) 0)))
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
               (:div :key (unique-id) "title placeholder")
               (:hilited-text
                :key (unique-id)
                :... (@ this props)
                :text (@ op comment)
                :opinions (prop opinions (slice 1))
                :tree-address (chain (prop tree-address)
                                     (concat (list (@ op id))))))))
         handle-click
         (lambda (e)
           (chain e (stop-propagation))))

       (def-component target-root
           (psx
            (:div
             :on-click (@ this handle-click)
             (:div :key (unique-id) "title placeholder")
             (:hilited-text
              :key (unique-id)
              :text (prop text)
              :opinions (prop opinions)
              :focus (state focus)
              :focus-func (@ this focus-func)
              :tree-address (list))))
         handle-click
         (lambda () (set-state focus (list)))
         focus-func
         (lambda (new-focus) (set-state focus new-focus))
         get-initial-state
         (lambda () (create focus (prop focus))))

       ))))

(defpsmacro mock-opinions (quantity)
  `(list
    ,@(collecting
        (dotimes (i quantity)
          (collect
              `(list (create :flag (list "Negative"
                                    ,(whichever "Spam" "Inflammatory" "Disagree" "Dislike" "Obscene" "Disturbing" "AlreadyAnswered" "LogicalFallacy" "AdHominem" "FromAuthority" "NeedsReference" "RaiseQuestion"))

                        :votevalue ,(whichever (- 1) 0 1))))))))
