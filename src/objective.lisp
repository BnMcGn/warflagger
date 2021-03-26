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
   #:participants
   #:flag-core
   #:opinion-references
   #:opinion-can-apply-dircs-to-parent))

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

(defun opinion-references (opinion)
  (let ((main (assoc-cdr :reference opinion))
        (commt (assoc-cdr :references opinion)))
    (if main (list* main commt) commt)))

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

(defun opinion-target-same-author-p (opinion)
  (when-let* ((treead (assoc-cdr :tree-address opinion))
              (ln (not (length1 treead)))
              (parent (warflagger:opinion-by-id (nelt treead 1))))
    (equal (assoc-cdr :author opinion) (assoc-cdr :author parent))
    (and (not (length1 treead)))))

;;FIXME: must be extended opinion. check type.
(defun opinion-can-apply-dircs-to-parent (opinion)
  (and (eq :blank (last-car (assoc :flag opinion)))
       (if-let ((cmt (assoc :clean-comment opinion)))
         (not (not-empty (cdr cmt)))
         t)
       (opinion-target-same-author-p opinion)))

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

(defun set-tree-freshness (warstat &rest timestamps)
  (setf
   (gethash :tree-freshness warstat)
   (car (sort (remove-if #'null timestamps) #'<))))

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
          (*warstat* (make-hash-table))
          (*text-warstat* (make-hash-table))
          (*title-warstat* (make-hash-table))
          (*apply-to* nil)
          (*other-flag* other-flag)
          (*direction* direction)
          (*tree-address* tree-address))
      (funcall modifiers)
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
      (if modifiers
          (execute-modifiers modifiers (append *tree-address* (list iid)) other-flag direction author)
          (make-hash-table))
    ;;accumulate own warstats
    (set-direction warstat direction)
    (multiple-value-bind (ws bb) (applied-to apply-to)
      (unless (warflagger:ballot-box-empty-p ballot-box)
        (warflagger:apply-ballot-box-to-warstats ballot-box warstat)
        (collect-warstats iid warstat))
      ;;FIXME: do we deal with text warstats for nonroot targets?
      (unless (warflagger:ballot-box-empty-p text-ballot-box)
        (warflagger:apply-ballot-box-to-warstats text-ballot-box text-warstat)
        (collect-warstats iid text-warstat :text))
      (unless (warflagger:ballot-box-empty-p title-ballot-box)
        (warflagger:apply-ballot-box-to-warstats title-ballot-box title-warstat)
        (collect-warstats iid title-warstat :title))
      ;;FIXME: Are there cases when an opinion and descendants should not be counted?
      (add-to-replies-count ws
                            (+ (gethash :replies-total warstat 0)
                               (gethash :replies-total text-warstat 0)
                               (gethash :replies-total title-warstat 0)))
      (set-tree-freshness ws
                          (assoc-cdr :timestamp (warflagger:opinion-by-id iid))
                          (gethash :timestamp warstat) (gethash :timestamp text-warstat)
                          (gethash :timestamp title-warstat))
      (when other-flag
        (warflagger:cast-vote! ballot-box :up iid author)
        (stick-other-flag-on-target other-flag ballot-box ws))
      (cond
        ;;We don't cast own vote if it's not an other-flag. Return appropriate ballot box instead.
        ((eq direction :pro)
         (setf bb (warflagger:merge-ballot-boxes bb ballot-box))
         bb)
        ((eq direction :con)
         (setf bb (warflagger:merge-with-inverted-ballot-boxes bb ballot-box))
         bb)
        (t nil)))))

(in-package :score-script)

;; Initial implementation of consensus scorer

(defun negative-spam (&key iid author modifiers)
  (wf/ipfs:flag-core :spam nil iid author modifiers))
(defun negative-inflammatory (&key iid author modifiers)
  (wf/ipfs:flag-core :inflammatory nil iid author modifiers))
(defun negative-disagree (&key iid author modifiers)
  (when-let ((balbox (wf/ipfs:flag-core nil :con iid author modifiers)))
    (warflagger:cast-vote! balbox :down author iid)))
(defun negative-dislike (&key iid author modifiers)
  (when-let ((balbox (wf/ipfs:flag-core nil :con iid author modifiers)))
    (warflagger:cast-vote! balbox :down author iid)))
(defun negative-language-warning (&key iid author modifiers)
  (wf/ipfs:flag-core :language-warning nil iid author modifiers))
(defun negative-disturbing (&key iid author modifiers)
  (wf/ipfs:flag-core :disturbing nil iid author modifiers))
(defun negative-logical-fallacy (&key iid author modifiers)
  (wf/ipfs:flag-core :logical-fallacy nil iid author modifiers))
(defun negative-needs-evidence (&key iid author modifiers)
  (when-let ((balbox (wf/ipfs:flag-core nil :con iid author modifiers)))
    (warflagger:cast-vote! balbox :wrong author iid)))
(defun negative-raise-question (&key iid author modifiers)
  (when-let ((balbox (wf/ipfs:flag-core nil :con iid author modifiers)))
    (warflagger:cast-vote! balbox :wrong author iid)))
(defun negative-out-of-bounds (&key iid author modifiers)
  (wf/ipfs:flag-core :out-of-bounds nil iid author modifiers))
(defun positive-funny (&key iid author modifiers)
  (wf/ipfs:flag-core :funny nil iid author modifiers))
(defun positive-agree (&key iid author modifiers)
  (when-let ((balbox (wf/ipfs:flag-core nil :pro iid author modifiers)))
    (warflagger:cast-vote! balbox :up author iid)))
(defun positive-like (&key iid author modifiers)
  (when-let ((balbox (wf/ipfs:flag-core nil :pro iid author modifiers)))
    (warflagger:cast-vote! balbox :up author iid)))
(defun positive-interesting (&key iid author modifiers)
  (wf/ipfs:flag-core :interesting nil iid author modifiers))
;;FIXME: this should go away
(defun statements-evidence (&key iid author modifiers)
  (let ((vv (assoc-cdr :vote-value (warflagger:opinion-by-id iid))))
    (cond ((eql vv 1) (positive-evidence :iid iid :author author :modifiers modifiers))
          ((eql vv -1) (negative-evidence :iid iid :author author :modifiers modifiers)))))
(defun negative-evidence (&key iid author modifiers)
  (when-let ((balbox (wf/ipfs:flag-core nil :con iid author modifiers)))
    (dolist (ref (wf/ipfs:opinion-references (warflagger:opinion-by-id iid)))
      (warflagger:cast-vote! balbox :wrong author iid ref))))
(defun positive-evidence (&key iid author modifiers)
  (when-let ((balbox (wf/ipfs:flag-core nil :pro iid author modifiers)))
    (dolist (ref (wf/ipfs:opinion-references (warflagger:opinion-by-id iid)))
      (warflagger:cast-vote! balbox :right author iid ref))))
(defun custodial-redundant (&key iid author modifiers)
  (wf/ipfs:flag-core :redundant nil iid author modifiers))
(defun custodial-out-of-date (&key iid author modifiers)
  (wf/ipfs:flag-core :out-of-date nil iid author modifiers))
(defun custodial-retraction (&key iid author modifiers)
  (if (equal author wf/ipfs::*target-author*)
      (progn
        (wf/ipfs:flag-core nil nil iid author modifiers)
        (setf (gethash :retraction wf/ipfs::*warstat*) t))
      (warn "Retraction applied to target not by author")))
(defun custodial-correction (&key iid author modifiers)
  (warn "Correction flag not implemented")
  (wf/ipfs:flag-core nil nil iid author modifiers))
(defun custodial-incorrect-flag (&key iid author modifiers)
  (wf/ipfs:flag-core :incorrect-flag nil iid author modifiers))
(defun custodial-flag-abuse (&key iid author modifiers)
  (if (length1 (assoc-cdr :tree-address (warflagger:opinion-by-id iid)))
      (warn "Flag-Abuse applied to root URL")
      (progn
        ;;FIXME: Dirty hack. Need to rethink system to make this less ugly
        (wf/ipfs:flag-core :flag-abuse nil iid author modifiers)
        (when (gethash :flag-abuse *warstat*)
          (setf *direction* :neutral)
          (setf *other-flag* nil)))))
(defun custodial-offtopic (&key iid author modifiers)
  (wf/ipfs:flag-core :offtopic nil iid author modifiers))
(defun custodial-arcane (&key iid author modifiers)
  (wf/ipfs:flag-core :arcane nil iid author modifiers))
(defun custodial-same-thing (&key iid author modifiers)
  (warn "Same-thing flag not implemented")
  (wf/ipfs:flag-core nil nil iid author modifiers))
(defun custodial-blank (&key iid author modifiers)
  (if (wf/ipfs:opinion-can-apply-dircs-to-parent (warflagger:opinion-by-id iid))
      ;;Will this work?
      (and modifiers (funcall modifiers))
      (wf/ipfs:flag-core nil nil iid author modifiers)))

;;FIXME: something should be set in warstats for suggest.
(defun target-text (&key iid author)
  (declare (ignore author))
  (if (not (length1 wf/ipfs::*tree-address*))
      (warn "Can't target the text of a non-rooturl target")
      (wfip::apply-to :text iid)))
(defun suggest-target-text (&key iid author)
  (declare (ignore author))
  (if (not (length1 wf/ipfs::*tree-address*))
      (warn "Can't target the text of a non-rooturl target")
      (wfip::apply-to :text iid)))
(defun target-title (&key iid author)
  (declare (ignore author))
  (wf/ipfs::apply-to :title iid))
(defun suggest-target-title (&key iid author)
  (declare (ignore author))
  (wfip::apply-to :title iid))

(defun vote-value (value &key iid author)
  (declare (ignore iid author))
  (if (eql value 0)
      (setf *direction* :neutral)
      (warn "Vote-value: only support setting to 0")))

(defun no-cascade (&key iid author)
  ;;FIXME: this might not work...
  (setf *direction* :neutral))
