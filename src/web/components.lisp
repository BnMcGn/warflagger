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

       (defun %overlap-p (start1 end1 start2 end2)
         (not (or (> start1 end2) (> start2 end1))))

       (defun %make-segments (text opins)
         (collecting
           (let ((segpoints (excerpt-segment-points
                             (collecting (dolist (op opins) (collect (@ op 0))))
                             (length text))))
             (do-window ((start end) segpoints)
               (collect
                   (psx (:hilited-segment
                         :key (unique-id)
                         :text (chain text (slice start end))
                         :opinions (remove-if-not
                                    (lambda (itm)
                                      (%overlap-p start (1- end)
                                                  (@ itm 0 'text-position 0)
                                                  (+ (@ itm 0 'text-position 0)
                                                     (@ itm 0 'text-position 1))))
                                    opins))))))))

       (define-react-class hilited-segment
           (if (> (prop opinions length) 0)
               (psx
                (:span
                 :style (create font-weight :bold)
                 (rebreak (prop text))
                 (:span :style (create position :relative)
                        (%make-opin-popups (prop opinions)))))
               (psx
                (:span :style (create font-weight :normal)
                       (rebreak (prop text))))))

       (define-react-class
           hilited-text
           (psx
            (:div (%make-segments (@ this props text)
                                  (remove-if-not
                                   (lambda (x)
                                     (chain (@ x 0) (has-own-property :excerpt)))
                                   (@ this props opinions))))))

       (define-react-class flag-display
           (psx
            (:span (prop flag 1))))

       (defun %make-opin-popups (opinions)
         (setf opinions (mock-opinions 30))
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

       (defun %make-sway-func (weight)
         ;;For now, just a parabola
         (lambda (i)
           (1- (expt (- i 0.5) 2))))

       (defun distribute-ranks-evenly (number
                                       &optional (rankmax
                                                  (/ (lisp *opin-box-height*)
                                                     (lisp *minimum-space*))))
         (let* ((div (chain -math (floor (/ number rankmax))))
                (mod (% number rankmax))
                (ranks (+ div (if (< 0 mod) 1 0)))
                (ranksize (chain -math (floor (/ number ranks))))
                (longranks (% number ranks)))
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
