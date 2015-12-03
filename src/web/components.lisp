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
           (psx
            (:span :style (if (> (@ this props opinions length) 0)
                              (create font-weight :bold)
                              (create font-weight :normal))
                   (rebreak (@ this props text)))))

       (define-react-class
           hilited-text
           (psx
            (:div (%make-segments (@ this props text)
                                  (remove-if-not
                                   (lambda (x)
                                     (chain (@ x 0) (has-own-property :excerpt)))
                                   (@ this props opinions))))))


       ))))
