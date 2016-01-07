(in-package :wf/web)

(define-parts mood-lib
  (add-part
   :@javascript
   (lambda ()
     (ps
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
         (let* ((hours (* 60 60 1000))
                (days (* 48 hours))
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
