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

;;;;;;;;;;;;;;;;;;;;;;;
; SVG badge generator
;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *flag-colors*
  (list :spam "#f00"
        :inflammatory "#ff8137"
        :disagree "#ff8137"
        :dislike "#ff8137"
        :language-warning "#ffe843"
        :disturbing "#f00"
        :already-answered "#ffe843"
        :logical-fallacy "#ff8137"
        :needs-reference "#6ffee4"
        :raise-question "#6ffee4"
        :out-of-bounds "#f00"
        :funny "#1cff00"
        :agree "#1cff00"
        :like "#1cff00"
        :interesting "#6ce9d2"
        :eye-witness "#6ffee4"
        :am-qualified "#6ffee4"
        :second-hand "#6ffee4"
        :anecdotal "#6ffee4"
        :evidence "#6ffee4"
        :disclosure "#9a12c6"
        :redundant "#ffe843"
        :out-of-date "#ffe843"
        :retraction "#560272"
        :correction "#ffe843"
        :incorrect-flag "#ffe843"
        :flag-abuse "#f00"
        :offtopic "#ffe843"
        :arcane "#ffe843"
        :same-thing "#ffe843"))

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

(defparameter *ring-cx* "21")
(defparameter *ring-cy* "21")
(defparameter *ring-r* "15.91549430918954")

(defmethod convert-tag-to-string-list ((tag (eql :radialgradient))
                                       attr-list body body-fn)
  (nconc (cons "<radialGradient"
               (convert-attributes attr-list))
         (list ">")
         (funcall body-fn body)
         (list "</radialGradient>")))

#|
;; A more generic solution to the case thing
(defmethod convert-tag-to-string-list :around ((tag t) attr-list body body-fn)
  (if (find-if #'lower-case-p (symbol-name tag))
      (nconc (list* "<"
                    (symbol-name tag)
                    (convert-attributes attr-list))
             (list ">")
             (funcall body-fn body)
             (list (format nil "</~a>" (symbol-name tag))))
      (call-next-method)))
|#

(defun draw-gradients (color)
  (html-out
    (:defs
        (:radialGradient :id "grad1" :cy *ring-cy* :fx "10%" :fy "50%" :r "8"
          (:stop :offset "0%" :stop-color color)
          (:stop
           :offset "100%"
           :stop-color (simple-rgb:xmlify-rgb
                        (simple-rgb:darken-rgb
                         (simple-rgb::parse color))))))))

(defun draw-ring (hole-func content-func)
  (html-out
    (:svg
     :xmlns "http://www.w3.org/2000/svg"
     :width "100" :height "100" :|viewBox| "0 0 42% 42%" :class "donut"
     (funcall hole-func)
     (:circle :class "donut-ring" :cx *ring-cx* :cy *ring-cy* :r *ring-r*
              :fill "none" :stroke "#d2d3d4" :stroke-width "3")
     (funcall content-func))))

(defun draw-segment (color offset length width)
  (html-out
    (:circle :class "donut-segment" :cx *ring-cx* :cy *ring-cy* :r *ring-r*
             :fill "none" :stroke color
             :stroke-dasharray
             (format nil "~a ~a" (float length) (float (- 100 length)))
             :stroke-dashoffset (princ-to-string (float (+ 25 offset)))
             :stroke-width (princ-to-string (float width)))))

(defun draw-hole (color)
  (html-out
    (draw-gradients color)
    (:circle :class "donut-hole" :cx *ring-cx* :cy *ring-cy* :r *ring-r*
             :fill "url(#grad1)")))

(defun intervals (num)
  (unless (zerop num)
    (let ((len (/ 100 num)))
      (gadgets:collecting
          (dotimes (i num)
            (gadgets:collect (cons (* i len) len)))))))

(defun calculate-direction-from-axis (opinion warstats)
  (if
   (< (getf warstats :effect) (getf warstats :controversy))
   :contested
   (three-way (assoc-cdr :votevalue opinion) :negative :neutral :positive)))

(defun draw-opinion-badge (opinion reply-tree warstats)
  (let* ((this-stats (gethash (assoc-cdr :id opinion) warstats))
         (reply-count (getf this-stats :replies-immediate))
         (intervals (intervals reply-count))
         (reply-ids (mapcar #'car reply-tree))
         (max-effect (when reply-ids
                       (apply #'max (mapcar (lambda (id)
                                              (getf (gethash id warstats)
                                                    :effect))
                                            reply-ids))))
         (min-effect (when reply-ids
                       (apply #'min (mapcar (lambda (id)
                                              (getf (gethash id warstats)
                                                    :effect))
                                            reply-ids))))
         (max-controv (when reply-ids
                        (apply #'max (mapcar (lambda (id)
                                               (getf (gethash id warstats)
                                                     :controversy))
                                             reply-ids))))
         (min-controv (when reply-ids
                        (apply #'min (mapcar (lambda (id)
                                               (getf (gethash id warstats)
                                                     :controversy))
                                             reply-ids)))))
    (labels ((scale-effect (num)
               (if (< min-effect max-effect)
                   (as-in-range 2 10
                                (relative-to-range min-effect max-effect num))
                   4))
             (scale-controv (num)
               (if (< min-controv max-controv)
                   (as-in-range 2 10
                                (relative-to-range min-controv max-controv num))
                   4)))
      (draw-ring
       (lambda ()
         (draw-hole
          (getf *flag-colors* (second (assoc-cdr :flag opinion)) "#fff")))
       (lambda ()
         (loop
            for (offset . length) in intervals
            for id in reply-ids
            for opinion = (opinion-from-id id)
            for warstat = (gethash id warstats)
            for direction = (calculate-direction-from-axis opinion warstat)
            do (draw-segment (getf *direction-colors* direction)
                             offset length
                             (if (eq direction :contested)
                                 (scale-controv (getf warstat :controversy))
                                 (scale-effect (getf warstat :effect))))))))))

(defun create-badges-for-rootid (rootid storage-dir)
  (let* ((rooturl (get-rooturl-by-id rootid))
         (main-tree (opinion-tree-for-rooturl rooturl))
         (warstats (generate-rooturl-warstats rooturl :tree main-tree)))
    (labels ((proc (tree)
               (dolist (node tree)
                 (let ((fname (merge-pathnames
                               storage-dir
                               (format nil "~a.svg" (car node)))))
                   (when (probe-file fname)
                     (delete-file fname))
                   (write-html-file
                       fname
                     (draw-opinion-badge (opinion-from-id (car node))
                                         (cdr node)
                                         warstats)))
                 (when (cdr node)
                   (proc (cdr node))))))
      (proc main-tree))))
