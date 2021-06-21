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
   #:opinion-can-apply-dircs-to-parent
   #:objective-data-for-opinions
   #:ipfs-write-rooturl-data))

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

(defun prep-opinions-for-extension (opinions)
  "Equivalent of load-opinion-files where opinions are already in memory."
  (cl-utilities:collecting
    (dolist (op opinions)
      (let* ((iid (assoc :iid op))
             (iid (if iid
                      (cdr iid)
                      (error "Iid not found"))))
        (cl-utilities:collect iid)
        (cl-utilities:collect op)))))

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

(defun opinion-reference-attributes (opinion)
  (let ((ref (assoc-cdr :reference opinion)))
    (when (stringp ref)
      (let ((opinml (is-location-opinml? ref)))
        (if opinml
            (let* ((iid (warflagger:get-target-id-from-url (if (stringp opinml) opinml ref)))
                   (refd-opin (warflagger:opinion-by-id iid)))
              (list (cons :refd-opinion iid)
                    (cons :reference-domain (warflagger:uri-domain (assoc-cdr :rooturl refd-opin)))))
            (list (cons :reference-domain (warflagger:uri-domain ref))))))))

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
        ;;Additions for references placed here, outside of the *opinion-store* binding
        ;;FIXME: Currently using DB to look up outside reference opinions
        (when (assoc :reference opinion)
          (setf opinion (append opinion (opinion-reference-attributes opinion))))
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

(declaim (ftype
          (function (warflagger:opinion &optional warflagger:opinion-store) warflagger:iid-tree-address)
          tree-address))
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
    (proto:tree-by-parent
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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Score script creation
;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (warflagger:iid-opinion-tree warflagger:opinion-store)
                          warflagger:score-script) make-score-script))
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
  (let* ((*package* (find-package 'warflagger))
         (flag (assoc-cdr :flag opinion))
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
;; Objective bundle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun objective-data (op-plist)
  (let* ((opinion-store (extend-opinions op-plist))
         (optree (opinion-tree-from-opinions (alexandria:hash-table-values opinion-store)))
         (scsc (make-score-script optree opinion-store))
         (rooturl (assoc-cdr :rooturl (car (alexandria:hash-table-values opinion-store)))))
    (list :opinion-store opinion-store :opinion-tree optree :score-script scsc :rooturl rooturl)))

(defun objective-data-for-dir (dirpath)
  (objective-data (load-opinion-files (uiop:directory-files dirpath))))

(defun objective-data-for-opinions (opinions)
  (objective-data (prep-opinions-for-extension opinions)))
