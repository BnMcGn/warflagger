(in-package :warflagger)



;;;;;;;;;;;;;;;;;;;;;;;
; SVG badge generator
;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *flag-colors*
  (list :spam "#f00"
        :inflammatory "#ff8100"
        :disagree "#ff8100"
        :dislike "#ff8100"
        :language-warning "#ffe843"
        :disturbing "#f00"
        :already-answered "#ffe843"
        :logical-fallacy "#ff8100"
        :needs-evidence "#4f41c8"
        :raise-question "#4f41c8"
        :out-of-bounds "#f00"
        :funny "#1cff00"
        :agree "#1cff00"
        :like "#1cff00"
        :interesting "#00adff"
        :eye-witness "#00adff"
        :am-qualified "#00adff"
        :second-hand "#00adff"
        :anecdotal "#00adff"
        :evidence "#00adff"
        :disclosure "#9a12c6"
        :redundant "#ffe843"
        :out-of-date "#ffe843"
        :retraction "#9a12c6"
        :correction "#ffe843"
        :incorrect-flag "#ffe843"
        :flag-abuse "#f00"
        :offtopic "#ffe843"
        :arcane "#ffe843"
        :same-thing "#ffe843"))


(defparameter *ring-cx* "21")
(defparameter *ring-cy* "21")
(defparameter *ring-r* "15.91549430918954")

;; https://stackoverflow.com/questions/46010186/mixed-case-tag-names-in-cl-who/
;; has a more generic solution


(eval-always
  (defmethod convert-tag-to-string-list ((tag (eql :radialgradient))
                                         attr-list body body-fn)
    (nconc (cons "<radialGradient"
                 (let ((*downcase-tokens-p* nil))
                   (convert-attributes (recasify-attributes attr-list))))
           (list ">")
           (funcall body-fn body)
           (list "</radialGradient>")))

  (defmethod convert-tag-to-string-list ((tag (eql :svg)) attr-list body body-fn)
    (nconc (cons "<svg"
                 (let ((*downcase-tokens-p* nil))
                   (convert-attributes (recasify-attributes attr-list))))
           (list ">")
           (funcall body-fn body)
           (list "</svg>")))

  (defun recasify-attributes (attr-list)
    "Get around cl-who trying to upcase everything or nothing."
    (mapcar
     (lambda (attr)
       (cons
        (if (find-if #'lower-case-p (symbol-name (car attr)))
            (string (car attr))
            (string-downcase (string (car attr))))
        (cdr attr)))
     attr-list)))

(defun draw-gradients (color)
  (html-out
    (:defs
        (:radialGradient
         :id "grad1"
         :fx "50%" :fy "30%" :r "1"
         (:stop :offset "0%" :stop-color color)
         (:stop
          :offset "100%"
          :stop-color (simple-rgb:xmlify-rgb
                       (simple-rgb:darken-rgb
                        (simple-rgb::parse color) :alpha 0.75))))
        (:radialGradient
         :id "grad2" :cx "21" :cy "10.5"
         :fx "21" :fy "10.5" :r "5.08"
         :|gradientTransform| "matrix(1.66,0.04,-0.03,1.25, -15.43,-0.099)"
         :|gradientUnits| "userSpaceOnUse"
         (:stop :offset "0%" :stop-color "#fff" :stop-opacity "1")
         (:stop :offset "100%" :stop-color "#fff" :stop-opacity "0")))))

(defun draw-ring (hole-func content-func)
  (html-out
    (:svg
     :xmlns "http://www.w3.org/2000/svg"
     :width "25" :height "25" :|viewBox| "0 0 42 42" :class "donut"
     (funcall hole-func)
     (:circle :class "donut-ring" :cx *ring-cx* :cy *ring-cy* :r *ring-r*
              :fill "none" :stroke "#525354" :stroke-width "2")
     (funcall content-func))))

(defun draw-segment (color offset length width)
  (html-out
    (:circle :class "donut-segment" :cx *ring-cx* :cy *ring-cy* :r *ring-r*
             :fill "none" :stroke color
             :stroke-dasharray
             (format nil "~a, ~a" (float length) (float (- 100 length)))
             :stroke-dashoffset (princ-to-string (float (+ 25 (- 100 offset))))
             :stroke-width (princ-to-string (float width)))))

(defun draw-hole (color)
  (html-out
    (draw-gradients color)
    (:circle :class "donut-hole" :cx *ring-cx* :cy *ring-cy* :r *ring-r*
             :fill "url(#grad1)")
    ;;spectral highlight
    (:ellipse :fill "url(#grad2)" :style "opacity:0.8;" :fill-opacity "1"
              :cx "19.5" :cy "14.0" :rx "8.68" :ry "8.36")))

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
    ;;FIXME: These values are only calculated on opins that have replies. Some
    ;; Opins have more effect even if they don't have replies. Effect calculation
    ;; mechanism may need some rethinking.
    (labels ((scale-effect (num)
               (if (< min-effect max-effect)
                   (as-in-range 3 15
                                (relative-to-range min-effect max-effect num))
                   4))
             (scale-controv (num)
               (if (< min-controv max-controv)
                   (as-in-range 3 15
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

;;FIXME: eventually might not want a flat directory space for these
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

(defun redraw-all-badges (&optional (storage-dir wf/local-settings:*targinfo-path*))
  (dolist (id (sql-stuff:get-column 'rooturl 'id))
    (create-badges-for-rootid id storage-dir)))

