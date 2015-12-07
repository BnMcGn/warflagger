(in-package :wf/web)

(define-parts target-components
  (add-part
   :@javascript
   (lambda ()
     (ps

       (let ((counter 0))
         (defun unique-id ()
           (incf counter)))

       (defun rebreak (text)
         (chain (collecting
                  (dolist (string (chain text (split (lisp-raw "\"\\n\""))))
                    (collect string)
                    (collect (psx (:br :key (unique-id))))))
                (slice 0 -1)))

       ;;Find all the indices where excerpts start or stop.
       (defun excerpt-segment-points (opset end)
         (chain
          (collecting-set
              (dolist (itm opset)
                (collect (@ itm text-position 0))
                (collect (+ (@ itm text-position 0) (@ itm text-position 1) 1)))
            (collect 0)
            (collect end))
          (sort (lambda (a b) (- a b)))))

       (defun %overlap-p (start1 end1 start2 end2)
         (not (or (> start1 end2) (> start2 end1))))

       (defun %make-segments (text opins)
         (collecting
           (let ((segpoints (excerpt-segment-points
                             (collecting (dolist (op opins) (collect (@ op 0))))
                             (length text))))
             (do-window ((start end) segpoints)
               (collect
                   (psx (:hilited-segment
                         :key (unique-id)
                         :text (chain text (slice start end))
                         :opinions (remove-if-not
                                    (lambda (itm)
                                      (%overlap-p start (1- end)
                                                  (@ itm 0 'text-position 0)
                                                  (+ (@ itm 0 'text-position 0)
                                                     (@ itm 0 'text-position 1))))
                                    opins))))))))

       (define-react-class hilited-segment
           (if (> (prop opinions length) 0)
               (psx
                (:span (:span :style (create font-weight :bold)
                              (rebreak (prop text)))
                       (:span (%make-opin-popups (prop opinions)))))
               (psx
                (:span :style (create font-weight :bold)
                       (rebreak (prop text))))))

       (define-react-class
           hilited-text
           (psx
            (:div (%make-segments (@ this props text)
                                  (remove-if-not
                                   (lambda (x)
                                     (chain (@ x 0) (has-own-property :excerpt)))
                                   (@ this props opinions))))))

       (define-react-class flag-display
           (psx
            (:span (prop flag 1))))

       (defun %make-opin-popups (opinions)
         (collecting
           (dolist (op opinions)
             (collect
                 (psx (:mini-opinion :opinion op :top 0 :left 0 :key (unique-id)))))))

       (define-react-class
           mini-opinion
           (psx
            (:div
             :class "opinion"
             :style (create position :absolute top (prop top) left (prop left))
             (:b (case (prop opinion votevalue)
                   (-1 "-") (0 "o") (1 "+")))
             (:flag-display (create flag (prop opinion flag))))))


       ))))
