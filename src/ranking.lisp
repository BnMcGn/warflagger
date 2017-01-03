(in-package :warflagger)

;;;
;;; ranking.lisp
;;;
;;; Various ranking algorithms and stuff
;;;
;;;

;;Opinion tree for rooturl

(defparameter *age-limit* (encode-time-delta 0 0 0 30))

(defun ratio-of-recentness-factor (ratio)
  "An arbitrary formula to reduce the hotness of a large, older discussion
that is winding down."
  (- 1 (* 0.01 (expt 1.55 (* 100 (- ratio 0.9))))))

(defun calculate-age-ranking (dates)
  ;;FIXME: Should ensure no future dates
  (let* ((total (length dates))
         (utime (get-universal-time))
         (recent
          (collecting
              (dolist (d dates)
                (let ((diff (+ *age-limit*
                               (- (clsql-helper:clsql-date/times->utime d)
                                  utime))))
                  (when (< 0 diff)
                    (collect diff))))))
         (rec-fact
          (ratio-of-recentness-factor (if (zerop (length recent))
                                          0
                                          (/ total (length recent))))))
    (* rec-fact
       (apply #'+ recent))))

(defun number-of-posts-factor (number)
  (or (cdr (first-match (lambda (x)
                          (> (car x) number))
                        '((1 . 0) (2 . 1) (10 . 4) (100 . 6) (200 . 8))))
      10))

(defun calculate-simple-ranking (rootid)
  ;;Can we do anything if rooturl hasn't been traced yet? Should do it?
  (if (rooturl-real-p rootid)
      (let* (;;(rooturl (get-rooturl-by-id rootid))
             (flat-opins
              (select (colm 'id) (colm 'datestamp)
                      :from (tabl 'opinion)
                      :where (sql-= (colm 'rooturl) rootid)
                      :order-by (colm 'datestamp)))
             ;;(opin-tree (opinion-tree-for-rooturl rooturl))
             (quantity-factor (number-of-posts-factor (length flat-opins)))
             (freshness-factor
              (calculate-age-ranking (mapcar #'second flat-opins))))
        (+ quantity-factor freshness-factor))
      0))

(defun get-ranked-rootids ()
  (let* ((ids (get-column 'rooturl 'id))
         (scores (mapcar #'calculate-simple-ranking ids)))
    (mapcar #'car
            (sort (remove-if-not (lambda (x) (< 0 (cdr x))) (pairlis ids scores))
                  #'> :key #'cdr))))

