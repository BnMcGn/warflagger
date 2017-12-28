(in-package :wf/web)

(defparameter *intensity-thresholds*
  '((0 . 2) (5 . 4) (20 . 6) (50 . 8) (100 . 16)))

(defparameter *direction-colors*
  (list :negative "rgba(256,0,0,0.75)"
        :neutral "rgba(171,163,163,0.75)"
        :positive "rgba(0,256,0,0.75)"
        :contested "rgba(256,136,0,0.75)"))

(define-parts mood-lib
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
           opins
           :test #'opinion-p)
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
                  (block found
                    (do-keyvalue (k v (lisp (alist->ps-object-code
                                             *intensity-thresholds*)))
                      (when (< k q)
                        (return-from found v)))))
                stroke-style
                (case (calculate-direction opins)
                  ("negative" (lisp (getf *direction-colors* :negative)))
                  ("neutral" (lisp (getf *direction-colors* :neutral)))
                  ("positive" (lisp (getf *direction-colors* :positive)))
                  ("contested" (lisp (getf *direction-colors* :negative))))))

      (defun delete-plumbs (base-obj)
        (when (chain base-obj (has-own-property '%plumb-instance))
          (chain base-obj %plumb-instance (delete-every-endpoint))))

      (defun display-popup-plumbs (base-obj target-id container opinions)
        (let ((plinst (chain js-plumb (get-instance))))
          ;;Clean up old plumbs
          (delete-plumbs base-obj)
          (setf (@ base-obj %plumb-instance) plinst)
          (chain plinst (set-container container))
          (chain plinst
                 (batch
                  (lambda ()
                    (dolist (op opinions)
                      (chain plinst
                             (connect
                              (create
                               source
                               (strcat "opinid-"
                                       (chain op 0 id (to-string)))
                               target target-id
                               anchors (list "Left" "Left")
                               paint-style (stroke-intensity op)
                               connector "Straight"
                               endpoint "Blank")))))))))

      )))

(defun flag-color-page (&rest params)
  (declare (ignore params))
  (html-out-str
    (:html
     (:head
      (:title "Flag colors"))
     (:body)
     (:div
      (mapcan-by-2
       (lambda (tag col)
         (html-out
           (:div (:span
                  :style (format nil "background-color: ~a" col)
                  "____")
                 (str tag))))
       *flag-colors*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Status indicator for targets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *indicator-names* (list :x-supported "thumbs_up_sign"
                                      :x-dissed "thumbs_down_sign"
                                      :x-right "check_mark"
                                      :x-wrong "ballot_x"
                                      :x-problematic "heavy_exclamation_mark_symbol"
                                      :x-unverified "black_question_mark_ornament"))

(defparameter *warstat-text*
  (list :x-supported "Has approval"
        :x-dissed "Has disapproval"
        :x-right "Has supporting evidence"
        :x-wrong "Has contradicting evidence"
        :x-problematic "Has conflict"
        :x-unverified "Has unresolved questions"))

(defun draw-indicator (category direction text)
  (html-out
    (:span :style (format nil "background-color: ~a; margin-left: 1px; margin-right: 1px;"
                          (getf *direction-colors* direction))
           (:img :src (format nil "/static/img/~a.svg"
                              (getf *indicator-names* category))
                 :title text
                 :style "height: .75em; :background-color: black;"))))

(defun display-warstats (wstats)
  (when (<= 1 (car (getf wstats :x-supported)))
    (draw-indicator :x-supported :positive (getf *warstat-text* :x-supported)))
  (when (<= 1 (car (getf wstats :x-dissed)))
    (draw-indicator :x-dissed :negative (getf *warstat-text* :x-dissed)))
  (when (<= 1 (car (getf wstats :x-right)))
    (draw-indicator :x-right :positive (getf *warstat-text* :x-right)))
  (when (<= 1 (car (getf wstats :x-wrong)))
    (draw-indicator :x-wrong :negative (getf *warstat-text* :x-wrong)))
  (when (or (<= 1 (car (getf wstats :x-problematic)))
            (<= 1 (second (getf wstats :x-supported)))
            (<= 1 (second (getf wstats :x-dissed)))
            (<= 1 (second (getf wstats :x-right)))
            (<= 1 (second (getf wstats :x-wrong)))
            (<= 1 (second (getf wstats :x-unverified))))
    (draw-indicator :x-problematic :contested (getf *warstat-text* :x-problematic)))
  (when (<= 1 (car (getf wstats :x-unverified)))
    (draw-indicator :x-unverified :contested (getf *warstat-text* :x-unverified))))
