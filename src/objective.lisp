(in-package :cl-user)


(defpackage #:wf/ipfs
  (:use #:cl #:gadgets #:alexandria #:access)
  (:export
   #:target-text
   #:suggest-target-title
   #:target-title
   #:text-comments-tree-p
   #:title-comments-tree-p
   #:text-script
   #:title-script
   #:general-script
   #:participants))

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
  (let ((warflagger:*opinion-store* (hu:plist->hash (%extend-opinions plist)
                                                    :existing (make-hash-table :test #'equal))))
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

;;FIXME: add type checking
(defun tree-address (opinion &optional opinion-store)
  (or (access opinion :tree-address)
      (if (string-equal (access opinion :target) (access opinion :rooturl))
          (list (access opinion :iid))
          (let ((tiid (warflagger::get-ipfs-hash-from-url (access opinion :target))))
            (if (warflagger:iid-p tiid)
                (append (tree-address (access opinion-store tiid) opinion-store)
                        (list (access opinion :iid)))
                (error "Target must be root or iid"))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Score script creation
;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;FIXME: add types/checking
(defun process-hashtag (tag opinion)
  (list 'hashtag tag :iid (assoc-cdr :iid opinion) :author (assoc-cdr :author opinion)))

;;FIXME: add types/checking, including known directives check.
(defun process-directive (dirc opinion)
  (append dirc (list :iid (assoc-cdr :iid opinion) :author (assoc-cdr :author opinion))))

(defun process-opinion (opinion)
  (let* ((flag (assoc-cdr :flag opinion))
         (flag (if (warflagger:recognized-flag-p flag)
                   (symb (car flag) '- (second flag))
                   'unknown-flag)))
    (list flag :iid (assoc-cdr :iid opinion) :author (assoc-cdr :author opinion))))

(defun process-scsc-node (iid opinion-store children)
  ;; FIXME: consider adding an excerpt wrapper where appropriate
  ;; FIXME: references should be handled
  (let* ((opinion (access opinion-store iid))
         (hashcode (mapcar (lambda (hashtag) (process-hashtag hashtag opinion))
                           (assoc-cdr :hashtags opinion)))
         (dircode (mapcar (lambda (dirc) (process-directive dirc opinion))
                          (assoc-cdr :directives opinion))))
    (list (append (process-opinion opinion) hashcode dircode children))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Score script interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *safe-symbols* (append warflagger::*known-directives*
                                     (mapcar (lambda (flag) (symb (car flag) '- (second flag)))
                                             (warflagger:known-flags))
                                     (list 'unknown-flag 'hashtag 'excerpt 'reference)))
(defparameter *safe-keywords* '(:iid :author))

(defpackage #:score-script
  (:use #:cl #:gadgets #:alexandria)
  (:nicknames #:scsc)
  (:export
   #:target-text
   #:suggest-target-text
   #:target-title
   #:suggest-target-title))

(defun scsc-safety-symbols (code &optional (package (find-package :score-script)))
  (proto:tree-search-replace
   code
   :predicate #'symbolp
   :valuefunc
   (lambda (sym)
     (if (keywordp sym)
         (if (member sym *safe-keywords*) sym
             (error "Unknown keyword in score script"))
         (if (member sym *safe-symbols* :test #'string-equal)
             (symbolize sym :package package)
             (error "Unknown symbol in score script"))))))



;;FIXME: add a score-script type and some type checking. Important for safety.

(defun prep-scsc-for-execution (code)
  "Symbols must be safetied before running"
  (proto:mapbranch
   (lambda (node)
     (if (member (car node) *safe-symbols*)
         (multiple-value-bind (main children) (splitfilter #'listp node)
           (when children
             (append
              main
              `(:modifiers
                (lambda ()
                  ,@children)))))
         node))
   code))

(defun text-comments-tree-p (tree)
  (when (member (car tree) '(scsc:target-text scsc:suggest-target-text))
    (return-from text-comments-tree-p t))
  (some (lambda (itm)
          (and (listp itm)
               (member (car itm) '(scsc:target-text scsc:suggest-target-text))
               (equal (gadgets:fetch-keyword :iid itm :in-list nil)
                      (gadgets:fetch-keyword :iid tree :in-list nil))))
        tree))

(defun title-comments-tree-p (tree)
  (when (member (car tree) '(scsc:target-title scsc:suggest-target-title))
    (return-from title-comments-tree-p t))
  (some (lambda (itm)
          (and (listp itm)
               (member (car itm) '(scsc:target-title scsc:suggest-target-title))
               (equal (gadgets:fetch-keyword :iid itm :in-list nil)
                      (gadgets:fetch-keyword :iid tree :in-list nil))))
        tree))

(defun text-script (tree)
  (remove-if-not #'text-comments-tree-p tree))

(defun title-script (tree)
  (remove-if-not #'title-comments-tree-p tree))

(defun general-script (tree)
  (remove-if (lambda (itm) (or (text-comments-tree-p itm) (title-comments-tree-p itm))) tree))

(defun participants (tree)
  (let ((curr (flatten tree)))
    (alexandria:hash-table-keys
    (hu:collecting-hash-table (:test #'equal :mode :keep)
      (loop
        do (setf curr (nth-value 1 (part-after-true (lambda (x) (eq x :author)) curr)))
        while curr
        do (hu:collect (car curr) t))))))

(defun scsc-for-tree-address (treead root-scsc)
  (proto:maptree
   (lambda (branch)
     (unless treead
       (error "Empty tree address"))
     (when (listp branch)
       (let ((iid (fetch-keyword :iid branch :in-list nil)))
        (cond
          ((equal iid (car treead))
           (if (length1 treead)
               (return-from scsc-for-tree-address branch)
               (progn
                 (setf treead (cdr treead))
                 (remove-if-not #'listp branch))))
          ((null iid)
           (remove-if-not #'listp branch))
          (t ;;iid found but not matching
           nil)))))
   root-scsc)
  nil)

(defvar *tree-address*)
(defvar *ballot-box*)
(defvar *text-ballot-box*)
(defvar *title-ballot-box*)
(defvar *warstats*)
(defvar *text-warstats*)
(defvar *title-warstats*)
(defvar *apply-to*)
(defvar *cascade*)

(defun cast-vote-at-target (axis author &optional (ballot-box *ballot-box*))
  (hu:collecting-hash-table (:existing ballot-box :mode :append)
    (hu:collect axis author)))

(defun stick-other-flag-on-target (flag ballot-box))

(defun apply-hashtag (hashtag ))

(defun apply-to (aspect iid)
  (unless (equal iid (lastcar *tree-address*))
    (error "IID problem: can't apply-to"))
  (if *apply-to*
      (warn "Apply-to is already set. No action taken.")
      (setf *apply-to* aspect)))

(defun collect-warstats (key warstats)
  )

(defun execute-modifiers (modifiers tree-address)
  (hu:collecting-hash-table (:mode :replace)
    (let ((*ballot-box* (make-hash-table))
          (*text-ballot-box* (make-hash-table))
          (*title-ballot-box* (make-hash-table))
          (*warstats* nil)
          (*text-warstats* nil)
          (*title-warstats* nil)
          (*apply-to* nil)
          (*cascade* t)
          (*tree-address* (append *tree-address* (list iid))))
      (funcall modifiers)
      (hu:collect :ballot-box *ballot-box*)
      (hu:collect :text-ballot-box *text-ballot-box*)
      (hu:collect :title-ballot-box *title-ballot-box*)
      (hu:collect :warstats *warstats*)
      (hu:collect :text-warstats *text-warstats*)
      (hu:collect :title-warstats *title-warstats*)
      (hu:collect :apply-to *apply-to*)
      (hu:collect :cascade *cascade*))))



(in-package :score-script)

;; Initial implementation of consensus scorer

(defun negative-spam (&key iid author modifiers)
  (hu:with-keys (ballot-box text-ballot-box title-ballot-box warstats text-warstats title-warstats
                            apply-to cascade)
      (if modifiers
          (execute-modifiers modifiers (append *tree-address* (list iid)))
          (make-hash-table))

    )
  
    ;;Save this set of warstats
    ;;Did or will directives need to modify things? How will we check?
 
    (dolist (vot *ballot-box*) ;;somehow
      (cast-vote-at-target &etc))
    (stick-other-flag-on-target :flag-spam *ballot-box* + mine)
    )
  #|
 What do we need to do?
  - Find out what effect we will have:
   - if there are any dislike or wrong, we have no effect
   - Has author retracted?
    - If so, what do we do with agreement? Under consensus, we drop it.
    - How do we make Retraction do the job itself? Don't want special cases!
  - Before calling modifiers, set self up as top of some stack of to-be-modified
   - There are things that can happen in modifiers that will disable this opinion, preventing
     other modifiers from effecting this or its target chain. How do we handle that?
   - The nature of this opinion determines whether or not its modifiers can touch parent targets
    - But what do they touch? Effect? Controversy? These are done through this, right? Count?
      That happens anyways...
   - Modifiers can be part of this opinion.
   - Modifiers can adjust the target (text, title) of this opinion. What do we do with contradictory
     modifiers?
   - No opinion is going to get its target modified in an unpredictable way. Or can it?
     - There are limits: so far only text/title
   - Modifiers might want to add their authors as contributing to parent vote.
  - What do we do if author is blacklisted?
   - Don't implement yet.
   - Don't cancel if non-blacklisted has added support.


  - Do we register spam attribute even if it is zero value?
  - Spam attribute is only applied to target. Dislike will flow up the tree.
  - Add to count of all parents, if we are doing count.
  - 
    -
|#

  

(defun negative-inflammatory (&key iid author modifiers))
(defun negative-disagree (&key iid author modifiers))
(defun negative-dislike (&key iid author modifiers))
(defun negative-language-warning (&key iid author modifiers))
(defun negative-disturbing (&key iid author modifiers))
(defun negative-logical-fallacy (&key iid author modifiers))
(defun negative-needs-evidence (&key iid author modifiers))
(defun negative-raise-question (&key iid author modifiers))
(defun negative-out-of-bounds (&key iid author modifiers))
(defun positive-funny (&key iid author modifiers))
(defun positive-agree (&key iid author modifiers))
(defun positive-like (&key iid author modifiers))
(defun positive-interesting (&key iid author modifiers))
(defun statements-evidence (&key iid author modifiers))
(defun custodial-redundant (&key iid author modifiers))
(defun custodial-out-of-date (&key iid author modifiers))
(defun custodial-retraction (&key iid author modifiers))
(defun custodial-correction (&key iid author modifiers))
(defun custodial-incorrect-flag (&key iid author modifiers))
(defun custodial-flag-abuse (&key iid author modifiers))
(defun custodial-offtopic (&key iid author modifiers))
(defun custodial-arcane (&key iid author modifiers))
(defun custodial-same-thing (&key iid author modifiers))
(defun custodial-blank (&key iid author modifiers))

;;FIXME: something should be set in warstats for suggest. 
(defun target-text (&key iid author)
  (declare (ignore author))
  (apply-to :text iid))
(defun suggest-target-text (&key iid author)
  (declare (ignore author))
  (apply-to :text iid))
(defun target-title (&key iid author)
  (declare (ignore author))
  (apply-to :title iid))
(defun suggest-target-title (&key iid author)
  (declare (ignore author))
  (apply-to :title iid))
