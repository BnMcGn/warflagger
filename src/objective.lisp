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

(declaim (ftype (function (warflagger::list-of-pathname) list)
                load-opinion-files))

(defun load-opinion-files (opfilelist)
  "Assumes that the filename is the iid of the opinion."
  ;;FIXME: check rooturl?
  (cl-utilities:collecting
    (dolist (fn opfilelist)
      (let ((iid (pathname-name fn))
            (opinion
              (with-open-file (s fn)
                (restart-case
                    (warflagger:deserialize-opinion-from-stream s)
                  (skip-opinion () nil)))))
        (when opinion
          (push (cons :iid iid) opinion)
          (unless (assoc :url opinion)
            (push (cons :url (warflagger::make-experimental-opinion-url iid)) opinion))
          (cl-utilities:collect iid)
          (cl-utilities:collect opinion))))))

(defun skip-opinion (c)
  (declare (ignore c))
  (when-let ((restart (find-restart 'skip-opinion))) (invoke-restart restart)))

(defun prep-opinions-for-extension (opinions)
  "Equivalent of load-opinion-files where opinions are already in memory."
  (cl-utilities:collecting
    (dolist (op opinions)
      (let* ((iid (assoc :iid op))
             (iid (if iid
                      (cdr iid)
                      (error "IID not found"))))
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
    (if (and treead (not (length1 treead)))
        (warflagger::opinion-text (last-car (butlast treead)))
        (gethash :text (warflagger:text-server-dispatcher (assoc-cdr :target op))))))

(defun opinion-reference-attributes (opinion)
  (let ((ref (assoc-cdr :reference opinion)))
    (when (stringp ref)
      (let ((opinml (warflagger::is-location-opinml? ref)))
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
        (if (and (assoc :reference opinion) (gadgets:assoc-cdr :reference opinion))
            (append opinion (opinion-reference-attributes opinion))
            opinion))))
   plist))

(defun extend-opinions (plist)
  (let ((warflagger:*opinion-store* (hu:plist->hash (%extend-opinions plist)
                                                    :existing (make-hash-table :test #'equal)))
        (res (make-hash-table :test #'equal)))
    (do-hash-table (iid opinion warflagger:*opinion-store*)
      (setf (gethash iid res)
            (cons (cons :tree-address (tree-address opinion warflagger:*opinion-store*)) opinion)))
    (do-hash-table (iid opinion warflagger:*opinion-store*)
      (setf (gethash iid res)
            ;;FIXME: get-target-text may still rely on db.
            (warflagger::add-extras-to-opinion
             (cons (cons :tree-address (tree-address opinion)) opinion)
             (warflagger:get-target-text iid))))
    res))

(defun opinion-references (opinion)
  "Note: Strips the link text out of :references"
  (let ((main (assoc-cdr :reference opinion))
        (commt (mapcar #'second (assoc-cdr :references opinion))))
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
                (append (tree-address (if opinion-store
                                          (access opinion-store tiid)
                                          (warflagger:opinion-by-id tiid))
                                      opinion-store)
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

(defun subtree-for-address (optree treead)
  "Returns the portion of the opinion tree below the supplied tree address, ie. all replies to it."
  (labels ((proc (ot ta)
             (if ta
                 (dolist (branch ot nil)
                   (when (equal (car branch) (car ta))
                     (return-from proc (proc (cdr branch) (cdr ta)))))
                 ot)))
    (proc optree treead)))

(defun has-excerpt-p (opinion)
  (assoc :excerpt opinion))

(defun has-found-excerpt-p (opinion)
  (and (has-excerpt-p opinion)
       (when-let ((dat (assoc-cdr :text-position opinion)))
         (and (integerp (car dat))
              (integerp (second dat))))))

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

(defun opinion-can-apply-hashtag-to-parent (opinion)
  (let ((hashtags (assoc-cdr :hashtags opinion)))
    (and (eq :blank (last-car (assoc :flag opinion)))
         (length1 hashtags)
         (eq (length (car hashtags))
             (length (string-strip (assoc-cdr :comment opinion)))))))

(defun opinion-has-dirc (opinion dirc)
  (let ((dircs (assoc :directives opinion)))
    (when dircs
      (first-match (lambda (x) (eq (car x) dirc)) (cdr dircs)))))

(defun opinion-applies-to-text (opinion)
  ;;FIXME: We aren't handling the Correction flag yet
  (some (curry #'opinion-has-dirc opinion) '(:target-text :suggest-target-text)))

(defun opinion-applies-to-title (opinion)
  (some (curry #'opinion-has-dirc opinion) '(:target-title :suggest-target-title)))

(defun opinion-suggests-t/t (opinion)
  (some (curry #'opinion-has-dirc opinion) '(:suggest-target-text :suggest-target-title)))

(defun opinion-is-question (opinion)
  (member '(second (assoc-cdr :flag opinion)) '(:raise-question :needs-evidence)))

(defun opinion-is-answer (opinion)
  (destructuring-bind (cat flag) (assoc-cdr :flag opinion)
    (or (eq flag :disagree)
        (and (eq flag :evidence) (eq cat :negative)))))

(defun opinion-is-supporting-answer (opinion)
  (destructuring-bind (cat flag) (assoc-cdr :flag opinion)
    (or (eq flag :agree)
        (and (eq flag :evidence) (eq cat :positive)))))

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
  (list 'warflagger:hashtag tag :iid (assoc-cdr :iid opinion) :author (assoc-cdr :author opinion)))

;;FIXME: add types/checking
(defun process-directive (dirc opinion)
  (if-let ((dircsym (first-match (curry #'string-equal (car dirc)) warflagger::*known-directives*)))
    (append (cons dircsym (cdr dirc))
            (list :iid (assoc-cdr :iid opinion) :author (assoc-cdr :author opinion)))
    (error "Unknown directive")))

(defun process-opinion (opinion)
  (let* ((*package* (find-package 'warflagger))
         (flag (assoc-cdr :flag opinion))
         (flag (if (warflagger:recognized-flag-p flag)
                   (symb (car flag) '- (second flag))
                   'warflagger:unknown-flag)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Score script result tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :warflagger)

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
   (:controversy 0)
   (:hashtags nil)))

(defun warstats-from-scsc-results (result &optional bbox)
  "The optional bbox is to allow this function to prepare warstats from an alternate ballot box, such as for text or title."
  (hu:with-keys (:ballot-box :tree-freshness :direction :replies-total :replies-immediate
                             :other-flags :direction-on-root :hashtags) result
    (let ((warstat (initialize-warstats))
          (ballot-box (or bbox ballot-box)))
      (warflagger:apply-ballot-box-to-warstats! ballot-box warstat)
      (setf (gethash :tree-freshness warstat) tree-freshness)
      (incf (gethash :replies-immediate warstat) replies-immediate)
      (incf (gethash :replies-total warstat) replies-total)
      (setf (gethash :direction warstat) direction)
      (setf (gethash :direction-on-root warstat) direction-on-root)
      (gadgets:do-hash-table (flag balbox other-flags)
        (multiple-value-bind (right up wrong down) (ballot-box-totals balbox)
          (let ((pos (+ right up))
                (neg (+ wrong down)))
            (when (vast-majority-p pos neg)
              (setf (gethash flag warstat) (nth-value 0 (score-controversy pos neg)))))))
      (setf (gethash :hashtags warstat)
            (cl-utilities:collecting
              (gadgets:do-hash-table (tag balbox hashtags)
                (multiple-value-bind (right up wrong down) (ballot-box-totals balbox)
                  (let ((pos (+ right up))
                        (neg (+ wrong down)))
                    (when (vast-majority-p pos neg)
                      (cl-utilities:collect tag)))))))
            warstat)))

(defun text-warstats-from-scsc-results (result)
  (warstats-from-scsc-results result (gethash :text-ballot-box result)))

(defun title-warstats-from-scsc-results (result)
  (warstats-from-scsc-results result (gethash :title-ballot-box result)))

;;For now we just indicate that something is a question, and how we decided that.
(defun add-question-info (warstat result &optional opinion)
  (hu:with-keys (:hashtags :ballot-box) result
    (let* ((flag (when opinion (wf/ipfs:opinion-is-question opinion)))
           (tag (alexandria:when-let ((bbox (gethash "list-of-things" hashtags)))
             (ballot-box-vast-majority-p bbox)))
           (quantity
            ;;FIXME: Not very sophisticated.
            ;; - check for redundant flags, though could do that in ballot box
            ;; - don't count refs that are targetted at an excerpt
            ;; - threshold is arbitrary
            ;;3rd value of ballot-box-totals is :wrong
            (and opinion (< 5 (nth-value 2 (ballot-box-totals ballot-box)))))
           (res (cl-utilities:collecting
                 (when flag (cl-utilities:collect :flag))
                 (when flag (cl-utilities:collect :tag))
                 (when quantity (cl-utilities:collect :replies))))
           (answered
             ;;Our spec for answered is different for #list-of-things
             (if (member :tag res)
                 (and
                  ;;pick a number...
                  (< 6 (+ (length (tally-ballot-box ballot-box :right))
                          (length (tally-ballot-box ballot-box :wrong))))
                  (< 10 (+ (gethash :x-right warstat) (gethash :x-wrong warstat))))
                 (and
                  (< 5 (gethash :x-wrong warstat))
                  (significant-majority-p (gethash :x-wrong warstat) (gethash :x-right warstat))))))
      (when (gadgets:not-empty res) (setf (gethash :question warstat) res))
      (when answered (setf (gethash :question-answered warstat) t))
      warstat)))

(defun add-root-text-info (stor url)
  (hu:with-keys (:text :status :message) (text-server url)
    (hu:collecting-hash-table (:existing stor :mode :replace)
     (hu:collect :initial-text (and (not-empty text) text))
     (hu:collect :initial-status status)
     (hu:collect :initial-message (and (not-empty message) message)))))

;;FIXME: Handle texts attached by reference
;;FIXME: Should some other text cleaning be done?
(defun prep-alternate-text (iid)
  (assoc-cdr :clean-comment (warflagger:opinion-by-id iid)))

(defun text-info-from-scsc-results (results rooturl)
  (let* ((rootres (gethash rooturl results))
         (alts (gethash :alternatives rootres))
         (alts (remove-if-not (lambda (x) (wf/ipfs::opinion-applies-to-text (opinion-by-id x)))
                              alts))
         (key (if alts
                  (car
                   (rank-ballot-boxes
                    (cons (gethash :text-ballot-box rootres)
                          (mapcar (lambda (x) (gethash :ballot-box (gethash x results))) alts))
                    :keys (cons rooturl alts)))
                  rooturl))
         (result (gethash key results))
         (warstats (text-warstats-from-scsc-results result)))
    (add-root-text-info warstats rooturl)
    (when (not-empty alts)
      (setf (gethash :competitors warstats) (cons rooturl alts)))
    ;;FIXME: Should be a way to indicate how secure an item is against its competitors
    ;; probably should be added to ballot-box.lisp
    (if (equal rooturl key)
        (progn
          (setf (gethash :text warstats) (gethash :initial-text warstats))
          (setf (gethash :text-source warstats) :initial))
        (progn
          (setf (gethash :text warstats) (prep-alternate-text key))
          (setf (gethash :text-source warstats) key)))
    (remhash :initial-text warstats)
    warstats))

(defun add-root-title-info (stor url)
  (hu:with-keys (:title :status :message) (text-server url)
    (hu:collecting-hash-table (:existing stor :mode :replace)
      (hu:collect :initial-title (and (not-empty title) title))
      (hu:collect :initial-status status)
      (hu:collect :initial-message (and (not-empty message) message)))))

(defun title-info-from-scsc-results (results &key rooturl iid)
  (let* ((starting-key (or iid rooturl))
         (starting-res (gethash starting-key results))
         (alts (gethash :alternatives starting-res))
         (alts (remove-if-not (lambda (x) (wf/ipfs::opinion-applies-to-title (opinion-by-id x)))
                              alts))
         (key (if alts
                  (car
                   (rank-ballot-boxes
                    (cons (gethash :title-ballot-box starting-res)
                          (mapcar (lambda (x) (gethash :ballot-box (gethash x results))) alts))
                    :keys (cons starting-key alts)))
                  starting-key))
         (result (gethash key results))
         (warstats (title-warstats-from-scsc-results result))
         (replies (let ((*id-return-type* :iid))
                    (mapcar #'opinion-by-id (target-replies (get-target-url key)))))
         (replies (if (equal key starting-key)
                      (remove-if-not #'wf/ipfs::opinion-applies-to-title replies) replies))
         (replies (remove-if-not #'wf/ipfs:has-found-excerpt-p replies))
         (title nil))
    (when (not iid)
      (add-root-title-info warstats rooturl))
    (if (equal key starting-key)
        (progn
          (setf title (gethash :initial-title warstats))
          (setf (gethash :title-source warstats) :initial))
        (progn
          (setf title (prep-alternate-text key))
          (setf (gethash :title-source warstats) key)))
    (setf (gethash :title warstats) title)
    (remhash :initial-title warstats)
    (when (and replies (not-empty title))
      (multiple-value-bind (segpoints segopins) (excerpt-segment-points replies (length title))
        (when segpoints
          (setf (gethash :title-segments warstats) segpoints)
          (setf (gethash :title-flavors warstats)
                (mapcar (lambda (x) (apply #'flavor-from-warstats x)) segopins)))))
    warstats))


