(in-package :wf/web)

(defparameter *intensity-thresholds*
  '((0 . 2) (5 . 4) (20 . 6) (50 . 8) (100 . 16)))

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

;;FIXME: is any of this in use?

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
