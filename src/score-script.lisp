(in-package :warflagger)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Score script interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *safe-symbols* (append warflagger::*known-directives*
                                     (mapcar (lambda (flag) (symb (car flag) '- (second flag)))
                                             (warflagger-core:known-flags))
                                     (list 'unknown-flag 'hashtag 'excerpt 'reference
                                           'statements-evidence)))
(defparameter *safe-keywords* '(:iid :author))

(defpackage #:score-script-support
  (:use #:cl #:gadgets #:alexandria)
  (:nicknames #:scss)
  (:export
   #:scsc-safety-symbols
   #:prep-scsc-for-execution
   #:*warstats*
   #:*text-warstats*
   #:*title-warstats*
   #:*dispatch*
   #:*score-data*))

(defpackage #:score-script
  (:use #:cl #:gadgets #:alexandria)
  (:nicknames #:scsc)
  (:export
   #:target-text
   #:suggest-target-text
   #:target-title
   #:suggest-target-title))


;; On incoming refs / refd stuff:

;; It might make a sort of sense to handle incoming refs within score-script. They should contribute
;; to the score of the target. But then we get hard-to-resolve loops of calculation and data coming
;; in from other trees... which may not be calculated yet. So, better to add the refd stuff at a later
;; stage.

;;; Typedefs

(defun score-script-p (item)
  (and (listp item)
       (every (lambda (sym)
                (if (keywordp sym)
                    (member sym *safe-keywords*)
                    (member sym *safe-symbols*)))
              (remove-if-not #'symbolp (flatten item)))))

(deftype score-script () `(satisfies score-script-p))

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

(in-package :score-script-support)

(defvar *dispatch*)
(defparameter *score-data* nil)

(in-package :warflagger)

(defun %execute-score-script (scsc rooturl opinion-store)
  (declare (type score-script scsc))
  (let* ((warflagger:*opinion-store* opinion-store)
         (scss:*score-data* (make-hash-table :test 'equal))
         ;; tree-address?
         (scss:*dispatch* (scsc-dispatch rooturl nil nil)))
    (mapc #'eval (prep-scsc-for-execution (scsc-safety-symbols scsc)))
    (funcall scss:*dispatch* :save nil)
    scss:*score-data*))

(defun execute-score-script (info-bundle &key score-script rooturl opinion-store)
  (let ((score-script (or score-script (getf info-bundle :score-script)))
        (rooturl (or rooturl (getf info-bundle :rooturl)))
        (opinion-store (or opinion-store (getf info-bundle :opinion-store))))
    (%execute-score-script score-script rooturl opinion-store)))

(defun scsc-dispatch (key parent-dispatch info)
  "key is iid or rooturl"
  (let ((ballot-box (warflagger:make-ballot-box (getf info :iid) (getf info :author)))
        (text-ballot-box (warflagger:make-ballot-box))
        (title-ballot-box (warflagger:make-ballot-box))
        (tree-freshness nil)
        (apply-to nil)
        (replies-immediate 0)
        (replies-total 0)
        (other-flag nil)
        (post-other-flag t)
        (other-flags (make-hash-table))
        (hashtags (make-hash-table :test 'equal))
        (direction :neutral)
        (direction-on-root :neutral)
        (alternatives nil)
        (on-post-procedures nil)
        (on-save-procedures nil)
        (enabled t))
    (labels ((which-bb (applied-to)
               (case applied-to
                 (:text text-ballot-box)
                 (:title title-ballot-box)
                 (otherwise ballot-box))))
      (lambda (cmd param &rest params)
        (case cmd
          (:direction
           (let ((newdir (or param :neutral)))
             (setf direction newdir)
             (setf direction-on-root (if (length1 (assoc-cdr :tree-address (getf info :opinion)))
                                         newdir
                                         (opinion-chain-direction
                                          (funcall parent-dispatch :info :direction-on-root)
                                          newdir)))))
          (:info
           (case param
             (:parent parent-dispatch)
             (:enabled enabled)
             (:direction direction)
             (:direction-on-root direction-on-root)
             (:ballot-box ballot-box)
             (:other-flag other-flag)
             (otherwise (getf info param))))
          (:apply-to
           (setf apply-to param))
          (:tree-freshness
           (if tree-freshness
               (when (string< (warflagger:js-compatible-utcstamp tree-freshness)
                              (warflagger:js-compatible-utcstamp param))
                 (setf tree-freshness param))
               (setf tree-freshness param)))
          ;;FIXME: t/t replies should be distinguished!
          (:replies
           (incf replies-immediate)
           (incf replies-total (1+ param)))
          (:merge-ballots
           (warflagger:merge-ballot-boxes! (which-bb (getf params :applied-to)) param))
          (:merge-ballots-inverted
           (warflagger:merge-with-inverted-ballot-boxes! (which-bb (getf params :applied-to)) param))
          (:merge-other-flag-ballots
           (if-let ((bb (gethash param other-flags)))
             (warflagger:merge-ballot-boxes! bb (getf params :ballot-box))
             (setf (gethash param other-flags)
                   (warflagger:copy-ballot-box (getf params :ballot-box)))))
          (:cast-own-vote
           (warflagger:cast-vote! (which-bb (getf params :applied-to)) param
                                  (getf params :iid) (getf params :author) (getf params :reference)))
          (:cast-own-other-vote
           (let ((bb (gethash param other-flags)))
             (warflagger:cast-vote! bb :up (getf params :iid) (getf params :author))))
          (:cast-vote
           (funcall parent-dispatch :cast-own-vote param :iid (getf info :iid)
                    :author (getf info :author) :reference (getf params :reference)
                    :applied-to apply-to))
          (:other-flag
           (setf other-flag param))
          (:post-other-flag
           (setf post-other-flag param))
          (:apply-hashtag
           (if-let ((bb (gethash param hashtags)))
             (merge-ballot-boxes! bb (car params))
             (setf (gethash param hashtags)
                   (copy-ballot-box (car params)))))
          (:add-alternative
           (push param alternatives))
          (:add-on-post-procedure
           (push param on-post-procedures))
          (:add-on-save-procedure
           (push param on-save-procedures))
          (:disable
           (setf enabled nil))
          (:save
           (mapcan #'funcall on-save-procedures)
           (let ((data (hu:hash
                        (:ballot-box ballot-box) (:text-ballot-box text-ballot-box)
                        (:title-ballot-box title-ballot-box) (:tree-freshness tree-freshness)
                        (:direction direction) (:direction-on-root direction-on-root)
                        (:replies-total replies-total)
                        (:replies-immediate replies-immediate) (:other-flags other-flags)
                        (:hashtags hashtags)
                        (:alternatives alternatives))))
             (setf (gethash key score-script-support::*score-data*) data)))
          (:post
           (mapcan #'funcall on-post-procedures)
           (funcall parent-dispatch :replies replies-total)
           (funcall parent-dispatch :tree-freshness tree-freshness)
           (cond
             ((null enabled) nil)
             ((and other-flag (not post-other-flag)) nil)
             (other-flag
              (funcall parent-dispatch :merge-other-flag-ballots other-flag :ballot-box ballot-box)
              ;; Auto-vote for other flag
              (funcall parent-dispatch :cast-own-other-vote
                       other-flag :iid (getf info :iid) :author (getf info :author)))
             ((eq direction :pro)
              (funcall parent-dispatch :merge-ballots ballot-box :applied-to apply-to))
             ((eq direction :con)
              (funcall parent-dispatch :merge-ballots-inverted ballot-box :applied-to apply-to)))))))))

(defun scsc-safety-symbols (code &optional (package (find-package :score-script)))
  (proto:tree-search-replace
   code
   :predicate #'symbolp
   :valuefunc
   (lambda (sym)
     (if (keywordp sym)
         (if (member sym warflagger::*safe-keywords*) sym
             (error "Unknown keyword in score script"))
         (if (member sym warflagger::*safe-symbols* :test #'string-equal)
             (symbolize sym :package package)
             (error "Unknown symbol in score script"))))))

;;FIXME: add a score-script type and some type checking. Important for safety.

(defun prep-scsc-for-execution (code)
  "Symbols must be safetied before running"
  (proto:mapbranch
   (lambda (node)
     (if (and (symbolp (car node)) (member (car node) warflagger::*safe-symbols* :test #'string-equal))
         (multiple-value-bind (children main) (splitfilter #'listp node)
           (if children
             (append
              main
              `(:modifiers
                (lambda ()
                  ,@children)))
             node))
         node))
   code))

(in-package :score-script-support)
;; Score-script-support is for tools that will be visible from within flags and directives

(defun set-direction (direction)
  (unless (member direction '(:pro :con :neutral))
    (error "Not a valid direction"))
  (funcall *dispatch* :direction direction))

(defun set-apply-to (type)
  (funcall *dispatch* :apply-to type))

(defun set-other-flag (flag)
  (funcall *dispatch* :other-flag flag))

(defun dont-flag ()
  (funcall *dispatch* :post-other-flag nil))

(defun set-tree-freshness (dt)
  (funcall *dispatch* :tree-freshness dt))

(defun disable ()
  (funcall *dispatch* :disable nil))

(defun disable-parent ()
  (when-let ((parent (funcall *dispatch* :info :parent)))
    (and (functionp parent)
         (funcall parent :disable))))

(defun reply-to-self-p ()
  (when-let ((parent (funcall *dispatch* :info :parent)))
    (and (functionp parent)
         (equal (funcall *dispatch* :info :author)
                (funcall parent :info :author)))))

(defun parent-is-root-p ()
  (length1 (assoc-cdr :tree-address (get-opinion))))

(defun blank-flag-p ()
  (eq :custodial-blank (funcall *dispatch* :info :other-flag)))

(defun vote-down ()
  (funcall *dispatch* :cast-vote :down))
(defun vote-up ()
  (funcall *dispatch* :cast-vote :up))
(defun vote-right (&optional ref)
  (funcall *dispatch* :cast-vote :right :reference ref))
(defun vote-wrong (&optional ref)
  (funcall *dispatch* :cast-vote :wrong :reference ref))

(defun apply-hashtag (tag)
  (funcall (funcall *dispatch* :info :parent)
           :apply-hashtag tag (funcall *dispatch* :info :ballot-box)))

(defun add-alternative (iid)
  (if-let ((parent (funcall *dispatch* :info :parent)))
    (funcall parent :add-alternative iid)
    (error "Shouldn't be called on root level")))

(defun get-opinion ()
  (funcall *dispatch* :info :opinion))

(defun get-opinion-created ()
  (assoc-cdr :datestamp (get-opinion)))

(defun get-ballot-box ()
  (funcall *dispatch* :info :ballot-box))

(defun directional-p ()
  (not (eq :neutral (funcall *dispatch* :info :direction))))

(defun enabledp ()
  (funcall *dispatch* :info :enabled))

;;FIXME: can't be run before post-flag, because of missing own vote
;;FIXME: likewise, can't be used for non other-flag for similar reasons.
;; could possibly just work with a copy of the ballot-box. might solve all of the problems
;;FIXME: At moment, this is the only subjective thing in score-script. Do we like that?
(defun approvedp ()
  "Essentially a vast-majority check on the current resource."
  (let ((balbox (get-ballot-box)))
    (if (warflagger:ballot-box-empty-p balbox)
        t
        (multiple-value-bind (right up wrong down) (warflagger:ballot-box-totals (get-ballot-box))
          (let ((pos (+ right up))
                (neg (+ wrong down)))
            (warflagger:vast-majority-p pos neg))))))

(defun run-modifiers ()
  (let ((mods (funcall *dispatch* :info :modifiers)))
    (when (functionp mods)
      (funcall mods))))

(defmacro on-post (&body body)
  `(funcall *dispatch* :add-on-post-procedure
            (lambda ()
              ,@body)))

(defmacro on-save (&body body)
  `(funcall *dispatch* :add-on-save-procedure
            (lambda ()
              ,@body)))

(defun save-flag ()
  (funcall *dispatch* :save nil))

(defun post-flag ()
  (funcall *dispatch* :post nil))

(defun post-error (err)
  (disable)
  ;;FIXME: implement error storage
  (warn err))

(eval-always
  (defmacro defflag (name &body body)
    (let ((opinion (gensym "opinion")))
      `(defun ,name (&key iid author modifiers)
         (let* ((,opinion (warflagger:opinion-by-id iid))
                (*dispatch* (warflagger::scsc-dispatch iid *dispatch*
                                           (list :iid iid :author author :opinion ,opinion
                                                 :modifiers modifiers))))
           ,@body)))))


(defflag scsc::negative-spam
  (set-other-flag :negative-spam)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::negative-inflammatory
  (set-other-flag :negative-inflammatory)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::negative-disagree
  (set-direction :con)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag)
  (when (and (enabledp) (directional-p))
    (vote-down)))

(defflag scsc::negative-dislike
  (set-direction :con)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag)
  (when (and (enabledp) (directional-p))
    (vote-down)))

(defflag scsc::negative-language-warning
  (set-other-flag :negative-language-warning)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::negative-disturbing
  (set-other-flag :negative-disturbing)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::negative-logical-fallacy
  (set-other-flag :negative-logical-fallacy)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::negative-needs-evidence
  (set-direction :con)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag)
  (when (and (enabledp) (directional-p))
    (vote-wrong)))

(defflag scsc::negative-raise-question
  (set-direction :con)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag)
  (when (and (enabledp) (directional-p))
    (vote-wrong)))

(defflag scsc::negative-out-of-bounds
  (set-other-flag :negative-out-of-bounds)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::positive-funny
  (set-other-flag :positive-funny)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::positive-agree
  (set-direction :pro)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag)
  (when (and (enabledp) (directional-p))
    (vote-up)))

(defflag scsc::positive-like
  (set-direction :pro)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag)
  (when (and (enabledp) (directional-p))
    (vote-up)))

(defflag scsc::positive-interesting
  (set-other-flag :positive-interesting)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

;;FIXME: this should go away
(defun scsc::statements-evidence (&key iid author modifiers)
  (let ((vv (assoc-cdr :vote-value (warflagger:opinion-by-id iid))))
    (cond ((eql vv 1) (scsc::positive-evidence :iid iid :author author :modifiers modifiers))
          ((eql vv -1) (scsc::negative-evidence :iid iid :author author :modifiers modifiers))
          ;;FIXME: why null? should be zero?
          ((null vv) (scsc::positive-evidence :iid iid :author author :modifiers modifiers))
          (t (warn "Unhandled statements-evidence flag")))))

(defflag scsc::negative-evidence
  (set-direction :con)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag)
  (when (and (enabledp) (directional-p))
    (dolist (ref (wf/ipfs:opinion-references (get-opinion)))
      (vote-wrong ref))))

(defflag scsc::positive-evidence
  (set-direction :pro)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag)
  (when (and (enabledp) (directional-p))
    (dolist (ref (wf/ipfs:opinion-references (get-opinion)))
      (vote-right ref))))

(defflag scsc::custodial-redundant
  (set-other-flag :custodial-redundant)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::custodial-out-of-date
  (set-other-flag :custodial-out-of-date)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::custodial-correction
  ;;FIXME: What do we actually do with this? Need to specify...
  (set-other-flag :custodial-correction)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::custodial-flag-abuse
  (set-other-flag :custodial-flag-abuse)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (when (parent-is-root-p)
      (post-error "Target is not an opinion"))
  (post-flag)
  (when (and (enabledp) (approvedp))
    (disable-parent)))

(defflag scsc::custodial-offtopic
  (set-other-flag :custodial-offtopic)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::custodial-arcane
  (set-other-flag :custodial-arcane)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::custodial-blank
  (set-other-flag :custodial-blank)
  (dont-flag)
  (set-tree-freshness (get-opinion-created))
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::statements-am-qualified
  (set-tree-freshness (get-opinion-created))
  (post-error "Deprecated flag")
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::negative-already-answered
  (set-tree-freshness (get-opinion-created))
  (post-error "Deprecated flag")
  (run-modifiers)
  (save-flag)
  (post-flag))

(defflag scsc::unknown-flag
  ;;FIXME: not sure the next two lines are the best approach. Review.
  (set-other-flag :unknown)
  (dont-flag)
  (set-tree-freshness (get-opinion-created))
  (post-error "Unknown flag")
  (run-modifiers)
  (save-flag)
  (post-flag))

;;FIXME: only one of these should be used at a time. Add a check. Or prime position?
(defun scsc::target-text (&key iid author)
  (declare (ignore author iid))
  (set-apply-to :text))
(defun scsc::suggest-target-text (&key iid author)
  (declare (ignore author))
  (cond
    ((not (parent-is-root-p))
     (post-error "Can't suggest text for non-root target"))
    ((not (blank-flag-p))
     (post-error "Text suggestions must be made with Custodial:Blank flag type"))
    (t
     (on-post
       (when (enabledp)
         (add-alternative iid))))))
(defun scsc::target-title (&key iid author)
  (declare (ignore author iid))
  (set-apply-to :title))
(defun scsc::suggest-target-title (&key iid author)
  (declare (ignore author))
  (if (not (blank-flag-p))
      (post-error "Title suggestions must be made with Custodial:Blank flag type")
      (on-post
        (when (enabledp)
          (add-alternative iid)))))

(defun scsc::vote-value (value &key iid author)
  (declare (ignore iid author))
  ;;FIXME: should we be able to apply to same author parent?
  (if (eql value 0)
      ;;FIXME: Should disable?
      (progn (set-direction :neutral) (disable))
      (post-error "#(vote-value): can only set to 0")))

(defun scsc::no-cascade (&key iid author)
  (declare (ignore iid author))
  (set-direction :neutral))

(defun scsc::hashtag (tag &key iid author)
  (declare (ignore tag iid author))
  ;; Some hashtags may get own functions, but this will need to be added in scsc maker.
  (on-save
    (apply-hashtag tag)))


