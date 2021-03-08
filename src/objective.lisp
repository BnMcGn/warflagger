(in-package :cl-user)

(defpackage #:wf/ipfs
  (:use #:cl #:gadgets #:alexandria #:access))

(in-package :wf/ipfs)

;;;
;;; objective.lisp
;;;
;;; Tools for extracting the objective traits of a root discussion
;;;
;;;

;;; This is a replacement of summarizer.lisp and some things in ranking.lisp
;;; Preliminary to transition to IPFS.
;;; Should be able to operate without referring to the database.

(declaim (ftype (function ((warflagger::list-of-type pathname)) list)
                load-opinion-files))

(defun load-opinion-files (opfilelist)
  "Assumes that the filename is the iid of the opinion."
  ;;FIXME: check rooturl?
  (cl-utilities:collecting
    (dolist (fn opfilelist)
      (let ((iid (pathname-name fn))
            (opinion (with-open-file (s fn)
                       (warflagger:deserialize-opinion-from-stream s))))
        (push (cons :iid iid) opinion)
        (unless (assoc :url opinion)
          (push (cons :url (warflagger::make-experimental-opinion-url iid)) opinion))
        (cl-utilities:collect iid)
        (cl-utilities:collect opinion)))))

;;FIXME: Rethink me. This is temporary for testing opinml import.
;; - for one, we want a better check for when an url is an iid opinion.
(defmethod warflagger:get-target-text ((opiniid string))
  ;;FIXME: would be nice to dispatch on type
  (unless (typep opiniid 'warflagger::iid)
    (warn "Expecting type iid"))
  (let* ((op (warflagger:opinion-by-id opiniid))
         (treead (assoc-cdr :tree-address op)))
    (if (length1 treead)
        (gethash :text (warflagger:text-server-dispatcher (assoc-cdr :target op)))
        (warflagger::opinion-text (last-car (butlast treead))))))

(defun %extend-opinions (plist)
  (mapcan-by-2
   (lambda (k opinion)
     (list
      k
      (let ((opinion (if (assoc-cdr :comment opinion)
                          ;;FIXME: parsing error report?
                         (append (warflagger::parse-comment-field (assoc-cdr :comment opinion))
                                 opinion)
                          opinion)))
        opinion)))
   plist))

(defun extend-opinions (plist)
  (let ((warflagger:*opinion-store* (hu:plist->hash (%extend-opinions plist))))
    (do-hash-table (iid opinion warflagger:*opinion-store*)
      (setf (gethash iid warflagger:*opinion-store*)
            (cons (cons :tree-address (tree-address opinion warflagger:*opinion-store*)) opinion)))
    (do-hash-table (iid opinion warflagger:*opinion-store*)
      (setf (gethash iid warflagger:*opinion-store*)
            ;;FIXME: get-target-text may still rely on db.
            (warflagger::add-extras-to-opinion opinion (warflagger:get-target-text iid))))
    warflagger:*opinion-store*))

(defun iid-equal (id1 id2)
  "Might be the bare id string, or might be an URL with the string on the end"
  (gadgets:sequences-end-same id1 id2))

(defun tree-address (opinion &optional opinion-store)
  (when opinion
    (or (access opinion :tree-address)
        (if (string-equal (access opinion :target) (access opinion :rooturl))
            (list (access opinion :iid))
            (let ((tiid (warflagger::get-ipfs-hash-from-url (access opinion :target))))
              (nreverse (cons (access opinion :iid)
                              (reverse (tree-address (access opinion-store tiid) opinion-store)))))))))

(defun opinion-tree-from-opinions (opinions)
  (let ((root (gadgets:assoc-cdr :rooturl (car opinions)))
        (opinions (sort (copy-list opinions) #'string<
                        :key (lambda (x)
                               (warflagger::js-compatible-utcstamp (assoc-cdr :datestamp x))))))
    (proto:tree-by-feature
     opinions
     (lambda (x) (let ((treead (tree-address x)))
                   (if (eq 1 (length treead))
                       root
                       (last-car (butlast treead)))))
     :root root
     :format (lambda (x) (gadgets:assoc-cdr :iid x))
     :identity-func (alexandria:curry #'gadgets:assoc-cdr :iid))))




;;;;;
;; What do we need to know about opinions/ rooturl?
;; - what flags have been applied to target by whom?
;; - what opinions count for conversation?
;; - displayableness of opinion
;;   - summary only?
;;   - does it have a comment (once directives are subtracted)?
;; - outgoing references
;; - do we have all opinions?
;; - what opinions apply to the text?
;; - what opinions apply to the title?
;; - how to handle opinions down the tree? Say a disagree to a side type of flag?
;;   - maybe a separate category for guesses... Could make quite a difference to a discredited flag if
;;     someone reputable reinforces it.
;;   - different algos may treat differently.
;; - some stuff from the existing summarizer?
;; - rooturl text info should be included.
;; - Three opinion trees
;; - qualifies as a question or list of things?
;; - direction stuff is objective, right?
;; - replies total/immediate might be subjective... or should we not? Count of hidden.


(defun make-score-script (optree opinion-store)
  ;;FIXME: Any kind of max depth safety?
  (apply
   #'append
   (mapcar
    (lambda (subtree)
      (process-scsc-node
       (car subtree)
       opinion-store
       (make-score-script (cdr subtree) opinion-store)))
    optree)))

(declaim (ftype (function (warflagger:extended-opinion) boolean) opinion-collapsible-p))

(defun opinion-collapsible-p (opinion)
  (and (eq :blank (last-car (assoc :flag opinion)))
       ;;FIXME: Clean-comment might not be the correct test. Case of comment consisting only
       ;; of hash tags as an example.
       (not (not-empty (and (assoc :clean-comment opinion) (assoc-cdr :clean-comment opinion))))))

(defun process-scsc-node (iid opinion-store children)

  ;; what decisions?
  ;; flag, owner, iid, :excerpt 
  ;; what do we do with various directives?
  ;; attach hashtags
  ;; references?
  ;;
  (let* ((opinion (access opinion-store iid))
         (x ))
    )
  )
