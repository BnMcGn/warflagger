(in-package :wf/web)

(defparameter *preferred-space* 1.5)
(defparameter *minimum-space* 1)
(defparameter *unit-string* "em")
(defparameter *opin-box-height* 20)
(defparameter *opin-box-width* 10)
(defparameter *placement-randomness* 0.45)
(defparameter *rank-overlap* 2)
(defparameter *sway-factor* (- 4))

(defun distribute-ranks ()
  (ps

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

    ))
