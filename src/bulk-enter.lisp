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

(defun handle-reference (author reference excerpt)
  (when reference
    (unless (url-p reference)
      (error "Reference needs to be an URL"))
    (if excerpt
        (or (find-usable-excerpt-opinion reference excerpt nil)
            (let ((data (plist->alist
                         (list
                          :target reference
                          :flag (expand-flag :blank)
                          :votevalue 0
                          :excerpt excerpt
                          :datestamp (clsql:get-time)))))
              (nth-value 1 (save-opinion-from-user data author))))
        reference)))

;;FIXME: we are ignoring excerpt offsets
(defun enter-bulk-opinion (target &key comment author flag votevalue excerpt reference
                                    reference-excerpt)
  (let* ((authid (or (find-author-id author :display-name)
                    ;;FIXME: Should add some URL to indicate fake author?
                    (insert-new-author :display-name author)))
         (flag (expand-flag flag))
         (votevalue (if votevalue
                        (if (integerp votevalue) votevalue
                            (error "Invalid votevalue"))
                        (getf *default-vote* (second flag))))
         (datestamp (clsql:get-time))) ;;FIXME: Want more sophisticated?
    (let ((data (plist->alist
                 (list
                  :target target
                  :flag flag
                  :votevalue votevalue
                  :datestamp datestamp
                  :excerpt excerpt
                  :comment comment
                  :reference (handle-reference authid reference reference-excerpt)))))
      (when (opinion-may-exist data authid)
        (error "Opinion may already exist in database"))
      (save-opinion-from-user data authid))))

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
      ((string-equal (car form) 'vote)
       (%proc-vote form target))
      (t (error "Not supported")))))

(defun %proc-opinion (form target)
  (multiple-value-bind (keys other)
      (divide-on-end-of-keywords (cdr form))
    ;;First item in the opinion body (other) is the comment
    (let ((opid (apply #'enter-bulk-opinion target :comment (car other) keys)))
      (%proc-forms (cdr other) (make-opinion-url nil opid)))))

(defun %proc-vote (form target)
  (multiple-value-bind (keys other)
      (divide-on-end-of-keywords (cdr form))
    ;;No comment. Any non-keyword items are replies
    (let ((opid (apply #'enter-bulk-opinion target keys)))
      (%proc-forms other (make-opinion-url nil opid)))))

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

(defun find-usable-excerpt-opinion (target excerpt offset)
  "Usable: same excerpt and a reasonable flag. Probably just Blank for now. No comment?"
  (dolist (replyid (target-replies target))
    (let ((reply (opinion-by-id replyid)))
      (when (and (equal excerpt (assoc-cdr :excerpt reply))
                 (if (assoc :excerpt-offset reply)
                     (eql (assoc-cdr :excerpt-offset reply) offset)
                     t))
        (return (values (assoc-cdr :url reply) replyid))))))



