(in-package :wf/web)

(defparameter *preferred-space* 1.5)
(defparameter *minimum-space* 1)
(defparameter *unit-string* "em")
(defparameter *opin-box-height* 20)
(defparameter *opin-box-width* 10)
(defparameter *placement-randomness* 0.45)
(defparameter *rank-overlap* 2)
(defparameter *sway-factor* (- 4))


(define-parts target-components
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
             :style (create font-weight :bold background-color :blue)
             :on-click (@ this handle-click)
             (rebreak (prop text))
             (:span :style (create position :relative)
                    (%make-opin-popups (if (and (not (prop not-viewable))
                                                (state viewable))
                                           (prop opinions)
                                           ([]))
                                       (@ this props)))))
         get-initial-state
         (lambda () (create viewable false))
         handle-click
         (lambda (e) (unless (prop not-viewable)
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
             (%make-segments (prop text)
                             (remove-if-not
                              (lambda (x)
                                (chain (@ x 0) (has-own-property :excerpt)))
                              (prop opinions))
                             (@ this props)))))

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
                                   :top (+ y (lisp *unit-string*))
                                   :left (+ x (lisp *unit-string*))
                                   :key (unique-id)))))

       (def-component
           mini-opinion
           (let ((vv (prop opinion votevalue)))
             (psx
              (:div
               :class "opinion"
               :style (create position :absolute top (prop top) left (prop left)
                              background :tan)
               (:b (case vv
                     (-1 "-") (0 "o") (1 "+")))
               (:flag-display :flag (prop opinion flag))))))

       (def-component opinion
           (let ((op (@ (prop opinions) 0)))
             (psx
              (:div
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
                                     (concat (list (@ op id)))))))))

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
         (lambda (new-focus) (set-state new-focus))
         get-initial-state
         (lambda () (create focus (prop focus))))

       (defun %make-sway-func (weight)
         ;;For now, just a parabola
         (lambda (i)
           (1- (expt (- i 0.5) 2))))

       (defun distribute-ranks-evenly (number
                                       &optional (rankmax
                                                  (/ (lisp *opin-box-height*)
                                                     (lisp *minimum-space*))))
         (let* ((div (chain -math (floor (/ number rankmax))))
                (mod (rem number rankmax))
                (ranks (+ div (if (< 0 mod) 1 0)))
                (ranksize (chain -math (floor (/ number ranks))))
                (longranks (rem number ranks)))
           (collecting
             (dotimes (i (- ranks longranks))
               (collect ranksize))
             (dotimes (i longranks)
               (collect (1+ ranksize))))))

       (defun spread-rank (number)
         (let* ((space (if (< (lisp *opin-box-width*)
                              (* number (lisp *preferred-space*)))
                           (lisp *preferred-space*)
                           (lisp *minimum-space*)))
                (endpoint (/ (* number space) 2)))
           (range (- endpoint) endpoint space)))

       (defun random-wiggle (x y)
         (destructuring-bind (a b)
             (chain (list (chain -math (random))
                          (chain -math (random)))
                    (sort (lambda (a b) (- a b))))
           (list
            (+ x
               (* b (lisp *placement-randomness*)
                  (chain -math (cos (* 2 (@ -math -p-i) (/ a b))))))
            (+ y
               (* b (lisp *placement-randomness*)
                  (chain -math (sin (* 2 (@ -math -p-i) (/ a b)))))))))

       (defun opinion-fan (item-count)
         (let ((xpos (lambda (y &optional (sway (%make-sway-func 0.5)))
                       (* (lisp *sway-factor*)
                          (funcall sway (+ 0.5 (/ y (lisp *opin-box-height*)))))))
               (rankcount 1))
           (collecting
             (dolist (ranklen (distribute-ranks-evenly item-count))
               (dolist (itempos (spread-rank ranklen))
                 (collect
                     (random-wiggle (+ (funcall xpos itempos)
                                       (* rankcount (lisp *rank-overlap*)))
                                    itempos)))
               (incf rankcount)))))

       ))))

(defpsmacro mock-opinions (quantity)
  `(list
    ,@(collecting
        (dotimes (i quantity)
          (collect
              `(list (create :flag (list "Negative"
                                    ,(whichever "Spam" "Inflammatory" "Disagree" "Dislike" "Obscene" "Disturbing" "AlreadyAnswered" "LogicalFallacy" "AdHominem" "FromAuthority" "NeedsReference" "RaiseQuestion"))

                        :votevalue ,(whichever (- 1) 0 1))))))))
