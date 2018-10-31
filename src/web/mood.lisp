(in-package :wf/web)

(defparameter *intensity-thresholds*
  '((0 . 2) (5 . 4) (20 . 6) (50 . 8) (100 . 16)))

(define-ps-lib mood-lib ()
  (ps

    (defun freshness-from-warstats (&rest warstats-coll)
      (let* ((hours (lisp *js-hour*))
             (days (* 2 (lisp *js-day*)))
             (now (chain -date (now)))
             (newest (max
                      (mapcar (lambda (warstats) (@ warstats tree-freshness))
                              warstats-coll))))
        (cond ((< newest (- now days)) "old")
              ((< newest (- now hours)) "recent")
              (t "new"))))

    (defun flavor-from-warstats (&rest warstats-coll)
      "Creates a flavor descriptor from a collation of all the warstats passed in. Needs to handle multiple warstats collections because it is used for excerpts which may represent multiple opinions."
        ;;; controv: The opinions are themselves contested
        ;;; positive: Relatively uncontested postive opinions
        ;;; negative: Relatively uncontested negative opinions
        ;;; We return contested if there are significant values in both positive and negative
        ;;; We return contested if controv is significant
        ;;; Neutral if none of the three are significant.
      (labels ((significant (n1 n2)
                 "is n1 significant related to n2?"
                 (when (< 0 n1)
                   (if (>= n1 n2)
                       t
                       (if (< 0.7 (relative-to-range 0 n2 n1))
                           t
                           nil)))))
        (let ((controv 0)
              (positive 0)
              (negative 0))
          (dolist (warstats warstats-coll)
            (cond
              ((significant (@ warstats controversy) (@ warstats effect))
               (incf controv (@ warstats controversy)))
              ((eql "pro" (@ warstats direction))
               (incf positive (@ warstats effect)))
              ((eql "con" (@ warstats direction))
               (incf negative (@ warstats effect)))))
          (let ((top (max controv positive negative)))
            (cond
              ((> 0.5 top) "neutral")
              ((significant controv top) "contested")
              ((eql top positive)
               (if (significant negative positive)
                   "contested"
                   "positive"))
              ((eql top negative)
               (if (significant positive negative)
                   "contested"
                   "negative")))))))

    ;;FIXME: Crude, doesn't account for target type, doesn't represent problematic
    (defun flavor-from-own-warstats (warstats)
      "Used to give a basic display of the status of a target. Uses own warstats. "
      (let* ((effect (@ warstats effect))
             (controv (@ warstats controversy))
             (pos (+ (@ warstats x-right 0) (@ warstats x-supported 0)))
             (neg (+ (@ warstats x-wrong 0) (@ warstats x-dissed 0)))
             (irrel (@ warstats x-irrelevant 0))
             (unver (@ warstats x-unverified 0))
             (probl (@ warstats x-problematic 0))
             (diff (relative-to-range 0 effect controv)))
        (if (< 0 effect)
            (if (> diff 0.7)
                "contested"
                "positive")
            (if (< 0 neg)
                "negative"
                (if (< 0 probl)
                    "contested"
                    "neutral")))))

    (defun flavor (warstats &rest ids)
      (let ((wcoll (mapcar (lambda (id) (getprop warstats id)) ids)))
        (chain (apply #'flavor-from-warstats wcoll)
               (concat "-" (freshness-from-warstats wcoll)))))

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

;;; This section is for setting up data attributes in the opinion display, so that moods
;;; can be rendered by css


    (defun magnitude (item &optional (keyfunc (lambda (x) x)))
      "Because CSS doesn't support greater than/ less than."
      (let ((val (keyfunc item)))
        (cond
          ((< 200 val) 4)
          ((< 50 val) 3)
          ((< 10 val) 2)
          ((< 3 val) 1)
          (t 0))))

    (defun format-warstats-data (stor stats)
      (let ((keylist
             (create controversy :data-controversy effect :data-effect
                     reference-controversy-extra :data-reference-controversy-extra
                     reference-controversy-main :data-reference-controversy-main
                     reference-effect-extra :data-reference-effect-extra
                     reference-effect-main :data-reference-effect-main  replies-immediate
                     :data-replies-immediate replies-total :data-replies-total))
            (xlist
             (create x-dissed :data-x-dissed  x-irrelevant :data-x-irrelevant
                     x-problematic :data-x-problematic  x-right :data-x-right
                     x-supported :data-x-supported  x-unverified :data-x-unverified
                     x-wrong :data-x-wrong)))
        (do-keyvalue (k v keylist)
          (setf (getprop stor v) (getprop stats k))
          (setf (getprop stor (strcat v "-m")) (magnitude (getprop stats k))))
        (do-keyvalue (k v xlist)
          (let ((val (getprop stats k)))
            (setf (getprop stor (strcat v "-effect")) (@ val 0))
            (setf (getprop stor (strcat v "-effect-m")) (magnitude (@ val 0)))
            (setf (getprop stor (strcat v "-controversy")) (@ val 1))
            (setf (getprop stor (strcat v "-controversy-m")) (magnitude (@ val 1)))))
        (let ((referenced (if (@ stats :referenced)
                              (@ stats :referenced length)
                              0)))
          (setf (@ stor :data-referenced) referenced)
          (setf (@ stor :data-referenced-m) (magnitude referenced)))))

    (defun format-opinion-data (stor opinion)
      (setf (@ stor :data-flag) (@ opinion flag 1))
      (setf (@ stor :data-flag-category) (@ opinion flag 0))
      (setf (@ stor :data-votevalue) (@ opinion votevalue)))
    ;;Add datestamp?

    (defun format-looks-data (stor id looks username)
      (when username
        (setf (@ stor :data-looks-available) "true")
        (when (chain looks (has-own-property id))
          (setf (@ stor :data-looked) "true"))))

    (defun format-reference-data (stor reference)
      (when reference
        (setf (@ stor :data-is-reference) t)
        (when (@ reference :refbot)
          (setf (@ stor :data-refbot) t))))

    (defun format-depth-data (stor tree-address display-depth)
      (when (or tree-address display-depth)
        (setf (@ stor :data-display-depth)
              (if display-depth
                  display-depth
                  (@ tree-address length)))))

    (defun format-styling-data (props)
      ;;Wants: opinion-store, warstats
      ;;Optional: tree-address (if opinion), references (if reference)
      (let ((res (create))
            (opid (when (chain props (has-own-property 'tree-address))
                    (list-last (@ props tree-address)))))
        (format-warstats-data
         res (if opid (getprop (@ props warstats) opid) (@ props warstats root)))
        (format-looks-data res opid (@ props looks) (@ props username) )
        (when opid
          (format-opinion-data res (getprop (@ props opinion-store) opid))
          (when (@ props references)
            (format-reference-data res (getprop (@ props references) opid))))
        (format-depth-data res (@ props tree-address) (@ props display-depth))
        (if (@ props extra-styling)
            (copy-merge-all res (@ props extra-styling))
            res)))

    (defun format-reference-styling-data (refdata)
      (let ((stor {}))
        (when (@ refdata 'warstats-src-url)
          (format-warstats-data stor (@ refdata :warstats)))))


    ))

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
