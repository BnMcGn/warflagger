(in-package :warflagger)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Score script interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *safe-symbols* (append warflagger::*known-directives*
                                     (mapcar (lambda (flag) (symb (car flag) '- (second flag)))
                                             (warflagger:known-flags))
                                     (list 'unknown-flag 'hashtag 'excerpt 'reference)))
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
   #:*tree-address*
   #:*text-ballot-box*
   #:*title-ballot-box*
   #:*warstat*
   #:*text-warstat*
   #:*title-warstat*
   #:*apply-to*
   #:*cascade*
   #:*other-flag*
   #:*direction*
   #:*target-author*
   #:stick-other-flag-on-target
   #:add-to-replies-count
   #:set-direction
   #:set-tree-freshness
   #:apply-to
   #:applied-to
   #:collect-warstats
   #:flag-core
   #:*ballot-box*))

(defpackage #:score-script
  (:use #:cl #:gadgets #:alexandria)
  (:nicknames #:scsc)
  (:export
   #:target-text
   #:suggest-target-text
   #:target-title
   #:suggest-target-title))

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

(defparameter *warstats* nil)
(defparameter *text-warstats* nil)
(defparameter *title-warstats* nil)

(defvar *tree-address*)
(defvar *ballot-box*)
(defvar *text-ballot-box*)
(defvar *title-ballot-box*)
(defvar *warstat*)
(defvar *text-warstat*)
(defvar *title-warstat*)
(defvar *apply-to*)
(defvar *cascade*)
(defvar *other-flag*)
(defvar *direction*)
(defvar *target-author* nil)

(in-package :warflagger)

(defun execute-score-script (scsc rooturl opinion-store)
  (declare (type score-script scsc))
  (let ((warflagger:*opinion-store* opinion-store)
        (scsc (scss:prep-scsc-for-execution (scss:scsc-safety-symbols scsc)))
        (scss:*warstats* (make-hash-table :test #'equal))
        (scss:*text-warstats* (make-hash-table :test #'equal))
        (scss:*title-warstats* (make-hash-table :test #'equal))

        (scss:*ballot-box* (warflagger:make-ballot-box))
        (scss:*text-ballot-box* (warflagger:make-ballot-box))
        (scss:*title-ballot-box* (warflagger:make-ballot-box))
        (scss:*warstat* (make-hash-table))
        (scss:*text-warstat* (make-hash-table))
        (scss:*title-warstat* (make-hash-table))
        (scss:*tree-address* nil))
    (mapc #'eval scsc)
    ;;FIXME: Should be writing blank warstat even if empty?
    (unless (warflagger:ballot-box-empty-p scss:*ballot-box*)
      (warflagger:apply-ballot-box-to-warstats scss:*ballot-box* scss:*warstat*)
      (scss:collect-warstats rooturl scss:*warstat*))
    (unless (warflagger:ballot-box-empty-p scss:*text-ballot-box*)
      (warflagger:apply-ballot-box-to-warstats scss:*text-ballot-box* scss:*text-warstat*)
      (scss:collect-warstats rooturl scss:*text-warstat* :text))
    (unless (warflagger:ballot-box-empty-p scss:*title-ballot-box*)
      (warflagger:apply-ballot-box-to-warstats scss:*title-ballot-box* scss:*title-warstat*)
      (scss:collect-warstats rooturl scss:*title-warstat* :title))
    (values scss:*warstats* scss:*text-warstats* scss:*title-warstats*)))

(in-package :score-script-support)

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

(defun initialize-warstats ()
  (hu:hash
   (:replies-immediate 0)
   (:replies-total 0)
   (:tree-freshness nil)
   (:x-right 0)
   (:x-wrong 0)
   (:x-up 0)
   (:x-down 0)
   (:effect 0)
   (:controversy 0)))

(defun stick-other-flag-on-target (flag ballot-box warstat)
  (multiple-value-bind (right up wrong down) (warflagger:ballot-box-totals ballot-box)
    (let ((pos (+ right up))
          (neg (+ wrong down)))
      (when (warflagger:score-vast-majority-p pos neg)
        (setf (gethash flag warstat) (nth-value 0 (warflagger:score-controversy pos neg)))))))

(defun add-to-replies-count (warstats childcount)
  (unless (key-in-hash? :replies-immediate warstats)
    (setf (gethash :replies-immediate warstats) 0))
  (unless (key-in-hash? :replies-total warstats)
    (setf (gethash :replies-total warstats) 0))
  (incf (gethash :replies-immediate warstats))
  (incf (gethash :replies-total warstats) (1+ childcount)))

(defun set-direction (warstat direction)
  (setf (gethash :direction warstat) (or direction :neutral)))

;;FIXME: doesn't seem to function. Tree freshness should never be nil
(defun set-tree-freshness (warstat &rest timestamps)
  (setf
   (gethash :tree-freshness warstat)
   (car (or (sort (remove-if #'null timestamps) #'string< :key #'warflagger:js-compatible-utcstamp)
            (error "No timestamp found in tree")))))

(defun apply-to (aspect iid)
  (unless (equal iid (lastcar *tree-address*))
    (error "IID problem: can't apply-to"))
  (if *apply-to*
      (warn "Apply-to is already set. No action taken.")
      (setf *apply-to* aspect)))

(defun applied-to (applyto)
  (case applyto
    (:text (values *text-warstat* *text-ballot-box*))
    (:title (values *title-warstat* *title-ballot-box*))
    (otherwise (values *warstat* *ballot-box*))))

(defun collect-warstats (key warstats &optional type)
  (let ((ws-store (case type
                    (:text *text-warstats*)
                    (:title *title-warstats*)
                    (otherwise *warstats*))))
    (setf (gethash key ws-store) warstats)))

(defun execute-modifiers (modifiers tree-address other-flag direction author)
  (hu:collecting-hash-table (:mode :replace)
    (let ((*target-author* author)
          (*ballot-box* (warflagger:make-ballot-box))
          (*text-ballot-box* (warflagger:make-ballot-box))
          (*title-ballot-box* (warflagger:make-ballot-box))
          (*warstat* (initialize-warstats))
          (*text-warstat* (make-hash-table))
          (*title-warstat* (make-hash-table))
          (*apply-to* nil)
          (*other-flag* other-flag)
          (*direction* direction)
          (*tree-address* tree-address))
      (when (functionp modifiers)
        (funcall modifiers))
      (hu:collect :ballot-box *ballot-box*)
      (hu:collect :text-ballot-box *text-ballot-box*)
      (hu:collect :title-ballot-box *title-ballot-box*)
      (hu:collect :warstat *warstat*)
      (hu:collect :text-warstat *text-warstat*)
      (hu:collect :title-warstat *title-warstat*)
      (hu:collect :apply-to *apply-to*)
      (hu:collect :direction *direction*)
      (hu:collect :other-flag *other-flag*))))

(defun flag-core (other-flag direction iid author modifiers)
  (hu:with-keys (:ballot-box :text-ballot-box :title-ballot-box :warstat :text-warstat :title-warstat
                 :apply-to :direction :other-flag)
      (execute-modifiers modifiers (append *tree-address* (list iid)) other-flag direction author)
    ;;accumulate own warstats
    (set-direction warstat direction)
    ;; ws and bb belong to the target, not the current item.
    (multiple-value-bind (ws bb) (applied-to apply-to)
      (unless (warflagger:ballot-box-empty-p ballot-box)
        (warflagger:apply-ballot-box-to-warstats ballot-box warstat))
      (collect-warstats iid warstat)
      ;;FIXME: do we deal with text warstats for nonroot targets?
      (unless (warflagger:ballot-box-empty-p text-ballot-box)
        (warflagger:apply-ballot-box-to-warstats text-ballot-box text-warstat))
      (collect-warstats iid text-warstat :text)
      (unless (warflagger:ballot-box-empty-p title-ballot-box)
        (warflagger:apply-ballot-box-to-warstats title-ballot-box title-warstat))
      (collect-warstats iid title-warstat :title)
      ;;FIXME: Are there cases when an opinion and descendants should not be counted?
      (add-to-replies-count ws
                            (+ (gethash :replies-total warstat 0)
                               (gethash :replies-total text-warstat 0)
                               (gethash :replies-total title-warstat 0)))
      (unless (gethash :tree-freshness warstat)
        (set-tree-freshness warstat (assoc-cdr :datestamp (warflagger:opinion-by-id iid))))
      (set-tree-freshness ws
                          (gethash :tree-freshness ws)
                          (gethash :tree-freshness warstat) (gethash :tree-freshness text-warstat)
                          (gethash :tree-freshness title-warstat))
      (when other-flag
        (warflagger:cast-vote! ballot-box :up iid author)
        (stick-other-flag-on-target other-flag ballot-box ws))
      (cond
        ;;We don't cast own vote if it's not an other-flag. Return appropriate ballot box instead.
        ((eq direction :pro)
         (warflagger:merge-ballot-boxes! bb ballot-box)
         bb)
        ((eq direction :con)
         (warflagger:merge-with-inverted-ballot-boxes! bb ballot-box)
         bb)
        (t nil)))))

(in-package :score-script)

;; Initial implementation of consensus scorer

(defun negative-spam (&key iid author modifiers)
  (scss:flag-core :spam nil iid author modifiers))
(defun negative-inflammatory (&key iid author modifiers)
  (scss:flag-core :inflammatory nil iid author modifiers))
(defun negative-disagree (&key iid author modifiers)
  (when-let ((balbox (scss:flag-core nil :con iid author modifiers)))
    (warflagger:cast-vote! balbox :down iid author)))
(defun negative-dislike (&key iid author modifiers)
  (when-let ((balbox (scss:flag-core nil :con iid author modifiers)))
    (warflagger:cast-vote! balbox :down iid author)))
(defun negative-language-warning (&key iid author modifiers)
  (scss:flag-core :language-warning nil iid author modifiers))
(defun negative-disturbing (&key iid author modifiers)
  (scss:flag-core :disturbing nil iid author modifiers))
(defun negative-logical-fallacy (&key iid author modifiers)
  (scss:flag-core :logical-fallacy nil iid author modifiers))
(defun negative-needs-evidence (&key iid author modifiers)
  (when-let ((balbox (scss:flag-core nil :con iid author modifiers)))
    (warflagger:cast-vote! balbox :wrong iid author)))
(defun negative-raise-question (&key iid author modifiers)
  (when-let ((balbox (scss:flag-core nil :con iid author modifiers)))
    (warflagger:cast-vote! balbox :wrong iid author)))
(defun negative-out-of-bounds (&key iid author modifiers)
  (scss:flag-core :out-of-bounds nil iid author modifiers))
(defun positive-funny (&key iid author modifiers)
  (scss:flag-core :funny nil iid author modifiers))
(defun positive-agree (&key iid author modifiers)
  (when-let ((balbox (scss:flag-core nil :pro iid author modifiers)))
    (warflagger:cast-vote! balbox :up iid author)))
(defun positive-like (&key iid author modifiers)
  (when-let ((balbox (scss:flag-core nil :pro iid author modifiers)))
    (warflagger:cast-vote! balbox :up iid author)))
(defun positive-interesting (&key iid author modifiers)
  (scss:flag-core :interesting nil iid author modifiers))
;;FIXME: this should go away
(defun statements-evidence (&key iid author modifiers)
  (let ((vv (assoc-cdr :vote-value (warflagger:opinion-by-id iid))))
    (cond ((eql vv 1) (positive-evidence :iid iid :author author :modifiers modifiers))
          ((eql vv -1) (negative-evidence :iid iid :author author :modifiers modifiers)))))
(defun negative-evidence (&key iid author modifiers)
  (when-let ((balbox (scss:flag-core nil :con iid author modifiers)))
    (dolist (ref (wf/ipfs:opinion-references (warflagger:opinion-by-id iid)))
      (warflagger:cast-vote! balbox :wrong iid author ref))))
(defun positive-evidence (&key iid author modifiers)
  (when-let ((balbox (scss:flag-core nil :pro iid author modifiers)))
    (dolist (ref (wf/ipfs:opinion-references (warflagger:opinion-by-id iid)))
      (warflagger:cast-vote! balbox :right iid author ref))))
(defun custodial-redundant (&key iid author modifiers)
  (scss:flag-core :redundant nil iid author modifiers))
(defun custodial-out-of-date (&key iid author modifiers)
  (scss:flag-core :out-of-date nil iid author modifiers))
(defun custodial-retraction (&key iid author modifiers)
  (if (equal author scss:*target-author*)
      (progn
        (scss:flag-core nil nil iid author modifiers)
        (setf (gethash :retraction scss:*warstat*) t))
      (warn "Retraction applied to target not by author")))
(defun custodial-correction (&key iid author modifiers)
  (warn "Correction flag not implemented")
  (scss:flag-core nil nil iid author modifiers))
(defun custodial-incorrect-flag (&key iid author modifiers)
  (scss:flag-core :incorrect-flag nil iid author modifiers))
(defun custodial-flag-abuse (&key iid author modifiers)
  (if (length1 (assoc-cdr :tree-address (warflagger:opinion-by-id iid)))
      (warn "Flag-Abuse applied to root URL")
      (progn
        ;;FIXME: Dirty hack. Need to rethink system to make this less ugly
        (scss:flag-core :flag-abuse nil iid author modifiers)
        (when (gethash :flag-abuse scss:*warstat*)
          (setf scss:*direction* :neutral)
          (setf scss:*other-flag* nil)))))
(defun custodial-offtopic (&key iid author modifiers)
  (scss:flag-core :offtopic nil iid author modifiers))
(defun custodial-arcane (&key iid author modifiers)
  (scss:flag-core :arcane nil iid author modifiers))
(defun custodial-same-thing (&key iid author modifiers)
  (warn "Same-thing flag not implemented")
  (scss:flag-core nil nil iid author modifiers))
(defun custodial-blank (&key iid author modifiers)
  (if (wf/ipfs:opinion-can-apply-dircs-to-parent (warflagger:opinion-by-id iid))
      ;;Will this work?
      (and modifiers (funcall modifiers))
      (scss:flag-core nil nil iid author modifiers)))

;;FIXME: add a general unknown-flag handler
(defun statements-am-qualified (&key iid author modifiers)
  (warn "Deprecated flag")
  (scss:flag-core nil nil iid author modifiers))
(defun negative-already-answered (&key iid author modifiers)
  (warn "Deprecated flag")
  (scss:flag-core nil nil iid author modifiers))

;;FIXME: something should be set in warstats for suggest.
(defun target-text (&key iid author)
  (declare (ignore author))
  (if (not (length1 scss:*tree-address*))
      (warn "Can't target the text of a non-rooturl target")
      (scss:apply-to :text iid)))
(defun suggest-target-text (&key iid author)
  (declare (ignore author))
  (if (not (length1 scss:*tree-address*))
      (warn "Can't target the text of a non-rooturl target")
      (scss:apply-to :text iid)))
(defun target-title (&key iid author)
  (declare (ignore author))
  (scss:apply-to :title iid))
(defun suggest-target-title (&key iid author)
  (declare (ignore author))
  (scss:apply-to :title iid))

(defun vote-value (value &key iid author)
  (declare (ignore iid author))
  (if (eql value 0)
      (setf scss:*direction* :neutral)
      (warn "Vote-value: only support setting to 0")))

(defun no-cascade (&key iid author)
  (declare (ignore iid author))
  ;;FIXME: this might not work...
  (setf scss:*direction* :neutral))
