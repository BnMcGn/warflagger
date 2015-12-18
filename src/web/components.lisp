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

       (defun focus-p (focus-spec)
         (and focus-spec (< 0 (@ focus-spec length))))

       (defun focus-parent-p (opins focus-spec)
         (dolist (o opins)
           (when (eql (@ o 0 id) (@ focus-spec 0))
             (return o))))

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

       (define-react-class hilited-segment
           (psx
            (:span
             :style (create font-weight :bold)
             :on-click (@ this handle-click)
             (rebreak (prop text))
             (:span :style (create position :relative)
                    (%make-opin-popups (if (and (not (prop not-viewable))
                                                (state viewable))
                                           (prop opinions)
                                           ([]))))))
         get-initial-state
         (lambda () (create viewable false))
         handle-click
         (lambda () (unless (prop not-viewable)
                      (set-state viewable (not (state viewable))))))

       (define-react-class plain-segment
           (psx
            (:span :style (create font-weight :normal)
                   (rebreak (prop text)))))

       (define-react-class parent-segment
           (let ((focussed (focus-parent-p (prop opinions) (prop focus))))
             (psx
                 (:span :style (create font-weight :bold position :relative)
                        :class (if focussed "parent-active" "parent-inactive")
                        (rebreak (prop text))
                        (when (and focussed
                                   (eql (prop last-char-pos)
                                        (+ (@ focussed 0 text-position 0)
                                           (@ focussed 0 text-position 1) 1)))
                          (psx (:opinion :opinions focussed
                                         :focus (prop focus))))))))

       (defun %make-segments (text opins focus)
         (collecting
           (let ((segpoints (excerpt-segment-points
                             (collecting (dolist (op opins) (collect (@ op 0))))
                             (length text))))
             (do-window ((start end) segpoints)
               (let ((current-opins (remove-if-not
                                     (lambda (itm)
                                       (%overlap-p start (1- end)
                                                   (@ itm 0 'text-position 0)
                                                   (+ (@ itm 0 'text-position 0)
                                                      (@ itm 0 'text-position 1))))
                                     opins))
                     (current-text (chain text (slice start end))))
                 (cond ((< (@ current-opins length) 1)
                        (collect
                            (psx (:plain-segment
                                  :key (unique-id)
                                  :text current-text))))
                       ((focus-p focus)
                        (collect
                            (psx (:parent-segment
                                  :key (unique-id)
                                  :text current-text
                                  :opinions current-opins
                                  :focus focus
                                  :last-char-pos end))))
                       (t
                        (collect
                            (psx (:hilited-segment
                                  :key (unique-id)
                                  :text (chain text (slice start end))
                                  :opinions current-opins))))))))))

       (define-react-class
           hilited-text
           (psx
            (:div
             :class
             (if (focus-p (prop focus)) "hilited" "hilited-parent")
             (%make-segments (prop text)
                             (remove-if-not
                              (lambda (x)
                                (chain (@ x 0) (has-own-property :excerpt)))
                              (prop opinions))
                              (prop focus)))))

       (define-react-class flag-display
           (psx
            (:span (prop flag 1))))

       (defun %make-opin-popups (opinions)
         ;(setf opinions (mock-opinions 30))
         (loop for op in opinions
               for (x y) in (opinion-fan (length opinions))
               collect
               (psx (:mini-opinion :opinion (@ op 0)
                                   :top (+ y (lisp *unit-string*))
                                   :left (+ x (lisp *unit-string*))
                                   :key (unique-id)))))

       (define-react-class
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

       (define-react-class opinion
           (let ((op (@ (prop opinions) 0)))
             (psx
              (:div
               :class "opinion"
               :style (create position :absolute
                              top "2em"
                              left (prop horizontal-position))
               (:div "title placeholder")
               (:hilited-text
                :text (@ op comment)
                :opinions (chain (prop opinions) (slice 1))
                :focus (and (focus-p (prop focus))
                            (chain (prop focus) (slice 1))))))))

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
