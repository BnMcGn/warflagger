(in-package :warflagger)

;;;
;;; author-stats.lisp
;;;
;;; Ratings objective and subjective on authors
;;;
;;;

(defparameter *author-stats* nil)


;;FIXME: These are just initial sketches. Remember to remove if they don't get used.

(defun author-blocked-p (author)
  (declare (ignore author)) nil)

;; This function is funny...
(defun author-ignored-p (author)
  (declare (ignore author)) (or (author-blocked-p author) nil))

(defun author-unknown-p (author)
  (and (not (author-ignored-p author)) (not (author-reasonable-p author))))

(defun author-reasonable-p (author)
  (or (author-trusted-p author) t))

(defun author-trusted-p (author)
  (declare (ignore author)) nil)

(defun author-average-reference-quality (author)
  "If an author has provided high quality refs in the past, we assume new refs are good. This will only work if we count previously unrated refs. Else the author can stuff known good refs to build a rating."
  (declare (ignore author))
  nil)

(defun author-vote-value (author)
  ;;FIXME!!!
  (if (author-ignored-p author) 0 1))

;;FIXME: rethink
;; - Ref votes should have more punch than regular. Old system did 2x on vote.
;; - We should incorporate the value of the reference if it is available. It isn't yet.
;; - Use average-reference-quality if not.
;; - Reference quality can't be used for unreasonable users. Use single regular vote for unknown auths.
(defun author-reference-vote-value (author reference)
  (if (author-reasonable-p author)
      (* *reference-vote-multiplier*
         (if-let  ((val (reference-transferrable-value reference)))
           val
           (or (author-average-reference-quality author) (author-vote-value author))))
      (author-vote-value author)))

;;FIXME: Stub. Needs to move somewhere else. Needs to be thought out.
(defun reference-transferrable-value (reference)
  (declare (ignore reference))
  nil)

(defparameter *reference-vote-multiplier* 2)
