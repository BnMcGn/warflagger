(in-package :warflagger)

;;;All contiguous whitespace characters will be matched as a single
;;;space, both in the excerpt and in the document.

(defstruct tdat text whitespace)

(defun create-textdata (text)
  ;;Leading whitespace can throw things off
  (let ((text (string-trim gadgets:*whitespace-characters* text)))
    (ret data (make-tdat)
      (setf (tdat-text data) text)
      (setf (tdat-whitespace data)
            (hu:collecting-hash-table (:mode :tally)
              (let ((found nil))
                (dotimes (i (length text))
                  (if (member (elt text i) *whitespace-characters*)
                      (progn
                        (unless found (setf found i))
                        (dolist (j (range found (1+ i)))
                          (hu:collect j t)))
                      (setf found nil)))))))))

(defun contiguous-whitespace? (tdat index)
  (gethash index (tdat-whitespace tdat) 0))

(defun excerpt-here? (tdat excerpt index)
  (declare (inline contiguous-whitespace?))
  (let ((exdat (create-textdata excerpt))
        (text (tdat-text tdat)))
    (loop with tind = index
          with eind = 0
          with tlen = (length text)
          with elen = (length excerpt)
          do (progn
               (when (= elen eind) (return tind))
               (when (= tlen tind) (return nil))
               (let ((ewhite (contiguous-whitespace? exdat eind))
                     (twhite (contiguous-whitespace? tdat tind)))
                 (if (and (= 0 ewhite) (= 0 twhite)
                          (eq (elt excerpt eind) (elt text tind)))
                     (progn (incf tind) (incf eind))
                     (if (or (= 0 ewhite) (= 0 twhite))
                         (return nil)
                         (progn (incf tind twhite) (incf eind ewhite)))))))))

;;FIXME: Doesn't handle excerpt that starts (or ends?) with whitespace
(defun find-excerpt-position (tdat excerpt &optional (offset 0))
  (dotimes (i (length (tdat-text tdat)))
    (awhen (excerpt-here? tdat excerpt i)
      (if (< 0 offset)
          (decf offset)
          (return (values i (- it i)))))))

(defun previous-break (text index)
  (position #\linefeed text :end index :from-end t))

(defun next-break (text index)
  (position #\linefeed text :start index))

(defun excerpt-context (text position length)
  (let*
      ((text (string-trim gadgets:*whitespace-characters* text))
       (tlength (length text))
       (estart position)
       (eend (+ length position))
       (tstart (previous-break text estart))
       (tend (next-break text eend))
       (leading-context (subseq text (if tstart (1+ tstart) 0) estart))
       (excerpt (subseq text estart eend))
       (trailing-context (subseq text eend (or tend tlength))))
    (list :leading leading-context :trailing trailing-context :excerpt excerpt)))

(defun excerpt-segment-points (opset end)
  "Find all the indices where excerpts start or stop. End is the length of the text. Assumes extended opinions that have text-position set. Second value is a matched list of lists of iids that target the identified segments."
  (let* ((stor (hu:collecting-hash-table (:mode :append)
                 (dolist (opin opset)
                   (let ((tpos (assoc-cdr :text-position opin)))
                     (print tpos)
                     (hu:collect (car tpos) nil)
                     (hu:collect (+ (car tpos) (second tpos)) (assoc-cdr :iid opin))))
                 (hu:collect 0 nil)
                 (hu:collect (1+ end) nil)))
         (stor (alexandria:hash-table-alist stor))
         (stor (sort stor #'< :key #'car)))
    (values
     (mapcar #'car stor)
     (mapcar (lambda (x) (remove-if #'null (cdr x))) stor))))

(defun clean-string-for-excerpt (str)
  (coerce
   (cl-utilities:collecting
     (let ((last-was-white nil))
       (dotimes (i (length str))
         (if (member (elt str i) *whitespace-characters*)
             (unless last-was-white
               (setf last-was-white t)
               (cl-utilities:collect #\ ))
             (progn
               (setf last-was-white nil)
               (cl-utilities:collect (elt str i)))))))
   'string))

;;Borrowed from flaglib
;; Placed here because it is somewhat related to excerpts. Might not be right spot.

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
          ((significant (gethash :controversy warstats) (gethash :effect warstats))
           (incf controv (gethash :controversy warstats)))
          ((eql :pro (gethash :direction warstats))
           (incf positive (gethash :effect warstats)))
          ((eql :con (gethash :direction warstats))
           (incf negative (gethash :effect warstats)))))
      (let ((top (max controv positive negative)))
        (cond
          ((> 0.5 top) :neutral)
          ((significant controv top) :contested)
          ((eql top positive)
           (if (significant negative positive)
               :contested
               :positive))
          ((eql top negative)
           (if (significant positive negative)
               :contested
               :negative)))))))
