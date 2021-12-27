(in-package :warflagger)


;;;
;;; ballot-box.lisp
;;;
;;; General tools for calculating the outcome of a vote
;;;
;;;

;;; Typedefs

(defun ballot-box-p (item)
  (and (hash-table-p item)
       (every (rcurry #'gadgets:key-in-hash? item) '(:right :wrong :up :down))))

(deftype ballot-box () `(satisfies ballot-box-p))

(defun vote-direction-p (item)
  (member item '(:up :down :right :wrong)))

(deftype vote-direction () `(satisfies vote-direction-p))

(declaim (ftype (function () ballot-box) make-ballot-box))
(defun make-ballot-box ()
  (let ((res (make-hash-table)))
    (setf (gethash :right res) nil)
    (setf (gethash :wrong res) nil)
    (setf (gethash :up res) nil)
    (setf (gethash :down res) nil)
    (setf (gethash 'cache res) nil)
    res))

(declaim (ftype (function (ballot-box) ballot-box) copy-ballot-box))
(defun copy-ballot-box (old)
  "Make a shallow copy of a ballot box"
  (let ((new (make-ballot-box)))
    (dolist (k '(:up :down :right :wrong))
      (setf (gethash k new) (gethash k old)))
    new))

(declaim (ftype (function (ballot-box vote-direction iid uri &optional uri) t) cast-vote!))
(defun cast-vote! (balbox direction iid author &optional reference)
  (setf (gethash 'cache balbox) nil)
  (push `(,iid ,author ,@(when reference (list reference))) (gethash direction balbox)))

(declaim (ftype (function (&rest (list-of-type 'ballot-box)) ballot-box)
                merge-ballot-boxes merge-ballot-boxes!
                merge-with-inverted-ballot-boxes merge-with-inverted-ballot-boxes!))

(defun merge-ballot-boxes (&rest boxes)
  (apply #'merge-ballot-boxes! (make-ballot-box) boxes))

(defun merge-ballot-boxes! (&rest boxes)
  (let ((res (car boxes)))
    (dolist (box (cdr boxes))
      (dolist (dir '(:right :wrong :up :down))
        (dolist (vote (gethash dir box))
          (apply #'cast-vote! res dir vote))))
    res))

(defun merge-with-inverted-ballot-boxes (&rest boxes)
  (apply #'merge-with-inverted-ballot-boxes!
         (merge-ballot-boxes! (make-ballot-box) (car boxes))
         (cdr boxes)))

(defun merge-with-inverted-ballot-boxes! (&rest boxes)
  (let ((res (car boxes)))
    (dolist (box (cdr boxes))
      (loop for dir in '(:right :wrong :up :down)
            for swap in '(:wrong :right :down :up)
            do (dolist (vote (gethash dir box))
                 (apply #'cast-vote! res swap vote))))
    res))

(declaim (ftype (function (ballot-box) boolean) ballot-box-empty-p))
(defun ballot-box-empty-p (balbox)
  (not (some (lambda (cat) (not-empty (gethash cat balbox)))
             '(:right :up :wrong :down))))

(declaim (ftype (function (ballot-box) ballot-box) remove-extra-votes print-ballot-box))
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
            do (if (author-reasonable-p author)
                   ;;FIXME: null reference shouldn't count if there are other refs
                   (unless (gethash (cons author (car reference)) refcheck)
                     (cast-vote! res :right iid author (car reference))
                     (hu:collect author t)
                     (setf (gethash (cons author (car reference)) refcheck) t))
                   (unless (gethash author present)
                     (cast-vote! res :right iid author (car reference))
                     (hu:collect author t))))
      (loop for (iid author . nil) in (gethash :up balbox)
            do (unless (gethash author present)
                 (cast-vote! res :up iid author)
                 (hu:collect author t))))
    (setf present (make-hash-table :test #'equal))
    (hu:collecting-hash-table (:existing present :mode :replace)
      (loop for (iid author . reference) in (gethash :wrong balbox)
            do (if (author-reasonable-p author)
                   (unless (gethash (cons author (car reference)) refcheck)
                     (cast-vote! res :wrong iid author (car reference))
                     (hu:collect author t)
                     (setf (gethash (cons author (car reference)) refcheck) t))
                   (unless (gethash author present)
                     (cast-vote! res :wrong iid author (car reference))
                     (hu:collect author t))))
      (loop for (iid author . nil) in (gethash :down balbox)
            do (unless (gethash author present)
                 (cast-vote! res :down iid author)
                 (hu:collect author t))))
    res))

(defun %ballot-box-totals (balbox)
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
    (values right up wrong down)))

(defun ballot-box-totals (balbox)
  (if-let ((totals (gethash 'cache balbox)))
    (apply #'values totals)
    (let ((totals (multiple-value-list (%ballot-box-totals (remove-extra-votes balbox)))))
      (setf (gethash 'cache balbox) totals)
      (apply #'values totals))))

(defun print-ballot-box (bb)
  (hu:with-keys
      (:right :up :wrong :down) bb
    (format t ":right ~a~%" right)
    (format t ":up ~a~%" up)
    (format t ":wrong ~a~%" wrong)
    (format t ":down ~a~%" down)
    (format t "~{~a~^ ~}~%" (multiple-value-list (ballot-box-totals bb))))
  bb)

(defun score-vast-majority-p (pos neg)
  (when (>= 1 pos)
    (>= (/ 1 10) (/ neg pos))))

(defun score-controversy (pos neg)
  (let* ((score (- pos neg))
         (effect (max 0 score))
         (balance (min pos neg)))
    ;;FIXME: Should this have a multiplier? Should it be a ratio?
    (values effect balance score)))

(defun apply-ballot-box-to-warstats! (balbox warstats)
  (multiple-value-bind (right up wrong down) (ballot-box-totals balbox)
    (setf (gethash :x-right warstats) right)
    (setf (gethash :x-up warstats) up)
    (setf (gethash :x-wrong warstats) wrong)
    (setf (gethash :x-down warstats) down)
    (multiple-value-bind (effect controv) (score-controversy (+ right up) (+ wrong down))
      (setf (gethash :effect warstats) effect)
      (setf (gethash :controversy warstats) controv))))

;; Comparison is a bit tricky!
;; - Need to be careful that users can't sneak in extra votes
;; - What about comparing a box that is high on both axes to one that is low on both?
;; - Can we use merge-with-inverted to do a good comparison? Might solve the sneak vote problem.
(defun compare-ballot-boxes (bbox1 bbox2)
  (multiple-value-bind (right up wrong down)
      (ballot-box-totals (merge-with-inverted-ballot-boxes bbox1 bbox2))
    (nth-value 2 (score-controversy (+ right up) (+ wrong down)))))

(defun rank-ballot-boxes (boxes &key (keys (range (length boxes))))
  "Order the ballot boxes by rank."
  (nreverse
   (mapcar #'car
           (sort (pairlis keys boxes) #'compare-ballot-boxes :key #'cdr))))
