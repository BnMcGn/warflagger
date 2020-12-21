(in-package :warflagger)

;;;Excerpt match spec:
;;;Because whitespace can vary so much around tags, it seems like it will
;;;be best to match excerpts in a target page without reference to whitespace.
;;;Therefore, an excerpt like "any time " will match "anytime" "any timed "
;;;"company timesheet" etc. Software that creates OpinML should therefore
;;;check target pages and compensate for false matches using the offset field.

;;;Alternate: All contiguous whitespace characters will be matched as a single
;;;space, both in the excerpt and in the document.

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
           (cl-utilities:with-collectors (offsets< lengths<)
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

;;;Currently used stuff below: Above is older, whitespaceless.

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

(defun find-excerpt-position (tdat excerpt &optional (offset 0))
  (dotimes (i (length (tdat-text tdat)))
    (awhen (excerpt-here? tdat excerpt i)
      (if (< 0 offset)
          (decf offset)
          (return (values i (- it i)))))))
