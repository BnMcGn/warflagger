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

(def-list-of ballot-box)

(defun vote-direction-p (item)
  (member item '(:up :down :right :wrong)))

(deftype vote-direction () `(satisfies vote-direction-p))

(declaim (ftype (function (&optional iid uri) ballot-box) make-ballot-box))
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

(declaim (ftype (function (ballot-box vote-direction iid uri &key &allow-other-keys) t) cast-vote!))
(defun cast-vote! (balbox direction iid author &key reference reversible &allow-other-keys)
  (setf (gethash 'cache balbox) nil)
  (push `(:iid ,iid :author ,author
               ,@(when reference (list :reference reference))
               ,@(when reversible (list :reversible reversible)))
        (gethash direction balbox)))

(defun apply-vote! (balbox direction vote)
  (destructuring-bind (&key iid author &allow-other-keys) vote
    ;;Extra keys will be ignored
    (apply #'cast-vote! balbox direction iid author vote)))

(defun up-voted (balbox iid author)
  (let ((newbox (copy-ballot-box balbox)))
    (cast-vote! newbox :up iid author)
    newbox))

(declaim (ftype (function (&rest list-of-ballot-box) ballot-box)
                merge-ballot-boxes merge-ballot-boxes!
                merge-with-inverted-ballot-boxes merge-with-inverted-ballot-boxes!))

(defun merge-ballot-boxes (&rest boxes)
  (apply #'merge-ballot-boxes! (make-ballot-box) boxes))

(defun merge-ballot-boxes! (&rest boxes)
  (let ((res (car boxes)))
    (dolist (box (cdr boxes))
      (dolist (dir '(:right :wrong :up :down))
        (dolist (vote (gethash dir box))
          (apply-vote! res dir vote))))
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
                 (apply-vote! res swap vote))))
    res))

(declaim (ftype (function (ballot-box) boolean) ballot-box-empty-p))
(defun ballot-box-empty-p (balbox)
  (not (or
        (some (lambda (cat) (not-empty (gethash cat balbox)))
              '(:right :up :wrong :down))
        (gethash :author balbox))))

(defun vote-countable-p (vote direction &key author-negative author-positive refcheck)
  "Rules:
 - An author can't have more than one positive and one negative vote, except maybe in the right/wrong category.
 - Tracking info is passed in the author-negative and author-positive hash tables
 - Refcheck is a per reference tracking hash
 - Right/wrong votes take precedence over up/down.
 - Right/wrong votes without a reference are ignored (for now. Not sure about this one)
 - Right/wrong votes are created by an author providing references. Extra votes can be gained under the   following conditions:
   - Author has an average-reference-quality rating
 - If an author makes multiple references and we need to narrow down to one, we already don't know if we
   can trust the author, so we just use a single vote as the value. This won't be done in this function.
 - There can be multiple unique ref votes per iid."
  (destructuring-bind (&key author reference reversible &allow-other-keys) vote
    (case direction
      (:up (unless (gethash author author-positive)
             (values :up (author-vote-value author))))
      (:down (unless (gethash author author-negative)
               (values :down (author-vote-value author))))
      ;; For a known reasonable author and an existing reference, we can add a vote for
      ;; each reference. Otherwise only one vote per author.
      (:right
       (if reversible
           (let* ((val (author-reference-vote-value author reference))
                  (dir (if (negative-real-p val) :wrong :right))
                  (authcheck (if (negative-real-p val) author-negative author-positive)))
             (unless (if (and (author-reasonable-p author) reference)
                         (gethash (cons author reference) refcheck)
                         (gethash author authcheck))
               (values dir
                       (abs val)
                       (when (and (author-reasonable-p author) reference)
                         (cons author reference)))))
           (unless (if (and (author-reasonable-p author) reference)
                       (gethash (cons author reference) refcheck)
                       (gethash author author-positive))
             (values :right
                     (author-reference-vote-value author reference)
                     (when (and (author-reasonable-p author) reference)
                       (cons author reference))))))
      (:wrong
       (if reversible
           (let* ((val (author-reference-vote-value author reference))
                  (dir (if (negative-real-p val) :right :wrong))
                  (authcheck (if (negative-real-p val) author-positive author-negative)))
             (unless (if (and (author-reasonable-p author) reference)
                         (gethash (cons author reference) refcheck)
                         (gethash author authcheck))
               (values dir
                       (abs val)
                       (when (and (author-reasonable-p author) reference)
                         (cons author reference)))))
           (unless (if (and (author-reasonable-p author) reference)
                       (gethash (cons author reference) refcheck)
                       (gethash author author-negative))
             (values :wrong
                     (author-reference-vote-value author reference)
                     (when (and (author-reasonable-p author) reference)
                       (cons author reference)))))))))

(defun ballot-box-totals (balbox)
  (let* ((author-negative (make-hash-table :test #'equal))
         (author-positive (make-hash-table :test #'equal))
         (refcheck (make-hash-table :test #'equal))
         (res
           (hu:collecting-hash-table
               (:mode :sum)
             (dolist (category '(:right :wrong :up :down))
               (dolist (vote (gethash category balbox))
                 (multiple-value-bind (outdir val refkey)
                     (vote-countable-p vote category
                                       :author-positive author-positive
                                       :author-negative author-negative
                                       :refcheck refcheck)
                   (when outdir
                     (when refkey (setf (gethash refkey refcheck) t))
                     (case outdir
                       (:right (setf (gethash (getf vote :author) author-positive) t))
                       (:up (setf (gethash (getf vote :author) author-positive) t))
                       (:wrong (setf (gethash (getf vote :author) author-negative) t))
                       (:down (setf (gethash (getf vote :author) author-negative) t)))
                     (hu:collect outdir val))))))))
    (hu:with-keys (:right :up :wrong :down) res
        (values right up wrong down))))

(defun tally-ballot-box (balbox category)
  (let* ((author-negative (make-hash-table :test #'equal))
         (author-positive (make-hash-table :test #'equal))
         (refcheck (make-hash-table :test #'equal))
         (res
           (hu:collecting-hash-table
               (:mode :append)
             (dolist (category '(:right :wrong :up :down))
               (dolist (vote (gethash category balbox))
                 (multiple-value-bind (outdir val refkey)
                     (vote-countable-p vote category
                                       :author-positive author-positive
                                       :author-negative author-negative
                                       :refcheck refcheck)
                   (declare (ignore val))
                   (when outdir
                     (when refkey (setf (gethash refkey refcheck) t))
                     (case outdir
                       (:right (setf (gethash (getf vote :author) author-positive) t))
                       (:up (setf (gethash (getf vote :author) author-positive) t))
                       (:wrong (setf (gethash (getf vote :author) author-negative) t))
                       (:down (setf (gethash (getf vote :author) author-negative) t)))
                     (hu:collect outdir (getf vote :iid)))))))))
    (gethash category res)))

(defun print-ballot-box (bb)
  (hu:with-keys
   (:right :up :wrong :down) bb
    (format t ":right ~a~%" right)
    (format t ":up ~a~%" up)
    (format t ":wrong ~a~%" wrong)
    (format t ":down ~a~%" down)
    (format t "~{~a~^ ~}~%" (multiple-value-list (ballot-box-totals bb))))
  bb)

(defun vast-majority-p (a b)
  (and (< 0 a)
       (< b a)
       (or (> 0 b) (>= (/ 1 10) (/ b a)))))

(defun significant-majority-p (a b)
  (and (< 0 a)
       (< b a)
       (or (> 0 b) (>= (/ 7 10) (/ b a)))))

(defun ballot-box-vast-majority-p (balbox)
  (multiple-value-bind (right up wrong down) (ballot-box-totals balbox)
    (vast-majority-p (+ right up) (+ wrong down))))

(defun score-controversy (pos neg)
  (let* ((score (- pos neg))
         (effect (max 0 score))
         (balance (min pos neg)))
    ;;FIXME: Should this have a multiplier? Should it be a ratio?
    (values effect balance score)))

(defun ballot-box-controversy (balbox author iid)
  (let ((xbal (make-ballot-box)))
    (when author
      (cast-vote! xbal :up iid author))
    (merge-ballot-boxes! xbal balbox)
    (multiple-value-bind (right up wrong down) (ballot-box-totals xbal)
      (score-controversy (+ right up) (+ wrong down)))))

(defun apply-ballot-box-to-warstats! (balbox warstats author iid)
  (multiple-value-bind (right up wrong down) (ballot-box-totals balbox)
    (setf (gethash :x-right warstats) right)
    (setf (gethash :x-up warstats) up)
    (setf (gethash :x-wrong warstats) wrong)
    (setf (gethash :x-down warstats) down))
  (setf (gethash :x-right-source warstats) (tally-ballot-box balbox :right))
  (setf (gethash :x-up-source warstats) (tally-ballot-box balbox :up))
  (setf (gethash :x-wrong-source warstats) (tally-ballot-box balbox :wrong))
  (setf (gethash :x-down-source warstats) (tally-ballot-box balbox :down))
  (multiple-value-bind (effect controv) (ballot-box-controversy balbox author iid)
    (setf (gethash :effect warstats) effect)
    (setf (gethash :controversy warstats) controv)))

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



