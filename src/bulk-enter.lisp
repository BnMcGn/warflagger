(in-package :warflagger)

;;;
;;; bulk-enter.lisp
;;;
;;; Tool for generating a conversation in a file for quick insertion.
;;;
;;;


(defun expand-flag (flagspec)
  (let ((flagspec (or flagspec :blank)))
    (loop
      for cat in *flag-category-keys*
      for flags in *flag-types-source*
      when
        (member flagspec flags)
      return (list cat flagspec)
      finally (error "Flag not found"))))

(defun enter-bulk-opinion (target &key comment author flag votevalue excerpt reference)
  (let* ((authid (or (find-author-id author :display-name)
                    ;;FIXME: Should add some URL to indicate fake author?
                    (insert-new-author :display-name author)))
         (flag (expand-flag flag))
         (votevalue (if votevalue
                        (if (integerp votevalue) votevalue
                            (error "Invalid votevalue"))
                        (getf *default-vote* (second flag))))
         (datestamp (clsql:get-time))) ;;FIXME: Want more sophisticated?
    (when (and reference (not (url-p reference)))
      (error "Reference needs to be an URL"))
    (let ((data (plist->alist
                 (list
                  :target target
                  :flag flag
                  :votevalue votevalue
                  :datestamp datestamp
                  :excerpt excerpt
                  :comment comment
                  :reference reference))))
      (when (opinion-may-exist data authid)
        (error "Opinion may already exist in database"))
      (insert-opinion data authid))))

(defun divide-on-end-of-keywords (items)
  (if-let ((divindex
            (some (lambda (i)
                    (and (not (keywordp (elt items i))) i))
                  (remove-if #'oddp (range (length items))))))
    (if (eq 0 divindex)
        (values nil items)
        (divide-on-index items divindex))
    (values items nil)))

(defun %proc-forms (forms target)
  (dolist (form forms)
    (cond
      ((string-equal (car form) 'opinion)
       (%proc-opinion form target))
      (t (error "Not supported")))))

(defun %proc-opinion (form target)
  (multiple-value-bind (keys other)
      (divide-on-end-of-keywords (cdr form))
    ;;First item in the opinion body (other) is the comment
    (let ((opid (apply #'enter-bulk-opinion target :comment (car other) keys)))
      (%proc-forms (cdr other) (make-opinion-url nil opid)))))


(defun bulk-enter (forms)
  (unless (string-equal (car forms) 'target)
    (error "Not a bulk opinion form"))
  (let ((target (fetch-keyword :url forms)))
    (%proc-forms (remove-if-not #'listp forms) target)))

(defun bulk-enter-file (path)
  (with-open-file (s path)
    (loop for data = (read s nil nil)
       while data
         do (bulk-enter data))))



