(in-package :warflagger)


;;;
;;; ballot-box.lisp
;;;
;;; General tools for calculating the outcome of a vote
;;;
;;;


(defun ballot-box-p (item)
  (and (hash-table-p item)
       (every (rcurry #'gadgets:key-in-hash? item) '(:right :wrong :up :down))))

(deftype ballot-box () `(satisfies ballot-box-p))

(defun make-ballot-box ()
  (let ((res (make-hash-table)))
    (setf (gethash :right res) (make-hash-table :test #'equal))
    (setf (gethash :wrong res) (make-hash-table :test #'equal))
    (setf (gethash :up res) (make-hash-table :test #'equal))
    (setf (gethash :down res) (make-hash-table :test #'equal))
    res))

(defun cast-vote! (balbox direction author iid &optional reference)
  (hu:collecting-hash-table (:existing (gethash direction balbox) :mode :append)
    (hu:collect iid (if reference (list author reference) (list author)))))

(defun merge-ballot-boxes (&rest boxes)
  (let ((res (make-ballot-box)))
    (dolist (box boxes)
      (dolist (dir '(:right :wrong :up :down))
        (do-hash-table (iid (author &optional reference) (gethash dir box))
          (cast-vote! res dir author iid reference))))
    res))

(defun remove-extra-votes (balbox)
  "Rules:
 - An author can't have more than one positive and one negative vote, except maybe in the right/wrong
   category.
 - Right/wrong votes take precedence over up/down.
 - Right/wrong votes are created by an author providing references. Extra votes can be gained under the   following conditions:
   - Author has an average-reference-quality rating
 - If an author makes multiple references and we need to narrow down to one, we already don't know if we
   can trust the author, so we just use a single vote as the value. This won't be done in this function."
  (let ((res (make-ballot-box))
        (present (make-hash-table :test #'equal)))
    (hu:collecting-hash-table (:existing present :mode :replace)
      (do-hash-table (iid (author &optional reference) (gethash :right balbox))
        (if (author-reasonable-p author)
            (cast-vote! res :right author iid reference)
            (unless (gethash author present)
              (cast-vote! res :right author iid reference)
              (hu:collect author t))))
      (do-hash-table (iid (author &optional reference) (gethash :up balbox))
        (unless (gethash author present)
          (cast-vote! res :up author iid reference)
          (hu:collect author t))))
    (setf present (make-hash-table :test #'equal))
    (hu:collecting-hash-table (:existing present :mode :replace)
      (do-hash-table (iid (author &optional reference) (gethash :wrong balbox))
        (if (author-reasonable-p author)
            (cast-vote! res :wrong author iid reference)
            (unless (gethash author present)
              (cast-vote! res :wrong author iid reference)
              (hu:collect author t))))
      (do-hash-table (iid (author &optional reference) (gethash :down balbox))
        (unless (gethash author present)
          (cast-vote! res :down author iid reference)
          (hu:collect author t))))
    res))

(defun ballot-box-scores (balbox)
  "Remove-extra-votes should have been run on the input already."
  (let ((right
          (apply #'+
                 (cl-utilities:collecting
                   (do-hash-table ))
                 (maphash
                  (lambda (iid vote)
                    (uthor-reference-vote-value author ())))))))
  (values
   (+
    (apply #'
     (maphash )))
   ))


