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
    (setf (gethash :right res) nil)
    (setf (gethash :wrong res) nil)
    (setf (gethash :up res) nil)
    (setf (gethash :down res) nil)
    res))

(defun cast-vote! (balbox direction iid author &optional reference)
  (push `(,iid ,author ,@(when reference (list reference)) (gethash direction balbox))))

(defun merge-ballot-boxes (&rest boxes)
  (let ((res (make-ballot-box)))
    (dolist (box boxes)
      (dolist (dir '(:right :wrong :up :down))
        (dolist (vote (gethash dir box))
          (apply #'cast-vote res dir vote))))
    res))

(defun remove-extra-votes (balbox)
  "Rules:
 - An author can't have more than one positive and one negative vote, except maybe in the right/wrong
   category.
 - Right/wrong votes take precedence over up/down.
 - Right/wrong votes without a reference are ignored (for now. Not sure about this one)
 - Right/wrong votes are created by an author providing references. Extra votes can be gained under the   following conditions:
   - Author has an average-reference-quality rating
 - If an author makes multiple references and we need to narrow down to one, we already don't know if we
   can trust the author, so we just use a single vote as the value. This won't be done in this function.
 - There can be multiple unique ref votes per iid."
  (let ((res (make-ballot-box))
        (present (make-hash-table :test #'equal))
        (refcheck (make-hash-table :test #'equal)))
    (hu:collecting-hash-table (:existing present :mode :replace)
      (loop for (iid author . reference) in (gethash :right balbox)
            do (when reference
                 (if (author-reasonable-p author)
                     (unless (gethash (cons author (car reference)) refcheck)
                       (cast-vote! res :right iid author reference)
                       (hu:collect author t)
                       (setf (gethash (cons author (car reference)) refcheck) t))
                     (unless (gethash author present)
                       (cast-vote! res :right iid author (car reference))
                       (hu:collect author t)))))
      (loop for (iid author . nil) in (gethash :up balbox)
            do (unless (gethash author present)
                 (cast-vote! res :up iid author)
                 (hu:collect author t))))
    (setf present (make-hash-table :test #'equal))
    (hu:collecting-hash-table (:existing present :mode :replace)
      (loop for (iid author . reference) in (gethash :wrong balbox)
            do (when reference
                 (if (author-reasonable-p author)
                     (unless (gethash (cons author (car reference)) refcheck)
                       (cast-vote! res :wrong iid author reference)
                       (hu:collect author t)
                       (setf (gethash (cons author (car reference)) refcheck) t))
                     (unless (gethash author present)
                       (cast-vote! res :wrong iid author (car reference))
                       (hu:collect author t)))))
      (loop for (iid author . nil) in (gethash :down balbox)
            do (unless (gethash author present)
                 (cast-vote! res :down iid author)
                 (hu:collect author t))))
    res))

(defun ballot-box-totals (balbox)
  "Remove-extra-votes should have been run on the input already."
  (let
      ((right
         (loop for (nil author . reference) in (gethash :right balbox)
               sum (author-reference-vote-value author (car reference))))
       (up
         (loop for (nil author . nil) in (gethash :up balbox)
               sum (author-vote-value author)))
       (wrong
         (loop for (nil author . reference) in (gethash :wrong balbox)
               sum (author-reference-vote-value author (car reference))))
       (down
         (loop for (nil author . nil) in (gethash :down balbox)
               sum (author-vote-value author))))
    (values (+ right up) (+ wrong down))))

(defun score-vast-majority-p (pos neg)
  (unless (>= 0 pos)
    (>= (/ 1 10) (/ neg pos))))
