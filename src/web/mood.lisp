(in-package :wf/web)

(defparameter *intensity-thresholds*
  '((0 . 2) (5 . 4) (20 . 6) (50 . 8) (100 . 16)))

(define-parts mood-lib
  (add-part
   :@javascript
   (lambda ()
     (ps

;;;FIXME: Implementation of flavor is naive and slow.
;;; Should account for different values placed on opinions.
;;; Should use cached results from database
       
       (defun calculate-flavor (opins)
         (let ((pos nil)
               (neg nil)
               (neut nil))
           (dolist (o opins)
             (threeway (@ o 0 votevalue)
                       (setf neg t) (setf neut t) (setf pos t)))
           (if pos
               (if neg "contested" "positive")
               (if neg "negative" "neutral"))))

       (defun calculate-freshness (opins)
         (let* ((hours (lisp *js-hour*))
                (days (* 2 (lisp *js-day*)))
                (now (chain -date (now)))
                (newest))
           (mapleaves
            (lambda (op)
              (let* ((dt (@ op datestamp))
                     (dt (if (eq (typeof dt) "string") (new (-date dt)) dt)))
                (if newest
                    (when (< newest dt)
                      (setf newest dt))
                    (setf newest dt))))
            opins)
           (cond ((< newest (- now days)) "old")
                 ((< newest (- now hours)) "recent")
                 (t "new"))))

       (defun flavor (opins)
         (chain (calculate-flavor opins)
                (concat "-" (calculate-freshness opins))))

       (defun calculate-intensity (opins)
         ;;For now, just count
         (let ((counter 0))
           (mapleaves (lambda (op) (incf counter))
                      opins)
           counter :test #'opinion-p))

;;; Difference between flavor and direction: Flavor is the sum of the effect of
;;; a group of opinions on a target/excerpt. One positive + one negative =
;;; contested. Direction is the votevalue of a single opinion, with indication
;;; if there is disagreement over it. Positive opin + negative child =
;;; contested. Negative opin + positive child = (more strongly) negative.

;;; So, for example, a high intensity contested opinion likely indicates a
;;; lively debate, but a high intensity positive or negative opinion may
;;; represent an obvious truism.

       (defun calculate-direction (opins)
         (if (member (calculate-flavor (chain opins (slice 1)))
                     (list "contested" "negative"))
             "contested"
             (threeway (@ opins 0 votevalue) "negative" "neutral" "positive")))

       (defun stroke-intensity (opins)
         (create line-width
                 (let ((q (calculate-intensity opins)))
                   (do-keyvalue (k v (lisp (alist->ps-object-code
                                            *intensity-thresholds*)))
                     (when (< k q)
                       (return v))))
                 stroke-style
                 (case (calculate-direction opins)
                   ("negative" "rgba(256,0,0,0.75)")
                   ("neutral" "rgba(171,163,163,0.75)")
                   ("positive" "rgba(0,256,0,0.75)")
                   ("contested" "rgba(256,136,0,0.75)"))))
       ))))
