(in-package :warflagger)

;;;Excerpt match spec:
;;;Because whitespace can vary so much around tags, it seems like it will
;;;be best to match excerpts in a target page without reference to whitespace.
;;;Therefore, an excerpt like "any time " will match "anytime" "any timed "
;;;"company timesheet" etc. Software that creates OpinML should therefore
;;;check target pages and compensate for false matches using the offset field.

(defun %is-whitespaceless-match (text pattern &optional (offset 0))
  (labels ((wht (itm) (member itm *whitespace-characters*)))
    (let ((tindex offset)
          (pindex 0)
          (tlen (length text))
          (plen (length pattern)))
      (loop do
        (cond
          ((>= pindex plen) (return (- tindex offset)))
          ((>= tindex tlen) (return nil))
          ((wht (elt text tindex)) (incf tindex))
          ((wht (elt pattern pindex)) (incf pindex))
          ((eq (elt text tindex) (elt pattern pindex))
           (incf tindex)
           (incf pindex))
          (t (return nil)))))))

(defun find-excerpt-matches (text excerpt)
  (nreverse
   (apply #'pairlis
          (multiple-value-list
           (with-collectors (offsets< lengths<)
             (dotimes (i (length text))
               (when (eq (elt excerpt 0) (elt text i))
                 (awhen (%is-whitespaceless-match text excerpt i)
                        (offsets< i)
                        (lengths< it)))))))))

(defparameter *excerpt-pre-context* 50)
(defparameter *excerpt-post-context* 50)

(defun excerpt-context (text start length &key (pre *excerpt-pre-context*)
                                            (post *excerpt-post-context*))
  (list
   (subseq text (if (< start pre) 0 (- start pre)) start)
   (subseq text start (+ start length))
   (subseq text (+ start length) (max (length text) (+ start length post)))))


