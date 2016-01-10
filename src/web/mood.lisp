(in-package :wf/web)

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
             (let ((vv (@ o 0 votevalue)))
               (case vv
                 (-1 (setf neg t))
                 (0 (setf neut t))
                 (1 (setf pos t)))))
           (if pos
               (if neg "contested" "positive")
               (if neg "negative" "neutral"))))

       (defun calculate-freshness (opins)
         (let* ((hours *js-hour*)
                (days (* 2 *js-day*))
                (now (chain -date (now)))
                (newest))
           (mapleaves
            (lambda (op)
              (let* ((dt (@ op datestamp))
                     (dt (if (eq (typeof dt) "string") (new (-date dt)) dt)))
                (if newest
                    (when (< newest dt)
                      (setf newest dt))
                    (setf newest dt)))))
           (cond ((< newest (- now days)) "old")
                 ((< newest (- now hours)) "recent")
                 (t "new"))))

       (defun flavor (opins)
         (chain (calculate-flavor opins)
                (concat "-" (calculate-freshness opins))))

       ))))
