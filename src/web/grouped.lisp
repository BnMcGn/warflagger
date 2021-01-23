(in-package :wf/web)

;;What will our datafile need?
;; - opins, warstats (opin and rooturl), headlines
;; - direction data
;; - can't do looks. Needs to be per user, but maybe a list of ids to request
;; - excerpt context (in opinions probably)
;; - titles for references?
;; - warstats for refs? Doesn't really make sense...
;; - group reference data

(defun gather-grouped-data-requirements (components)
  ;;What about looks?
  (cl-utilities:with-collectors (opins< warstats< headlines< )
    (dolist (c components)
      (case (gethash :rowtype c)
        (:rooturl
         (warstats< (gethash :url c))
         (headlines< (gethash :url c))
         (when (gethash :refparent c)
           (opins< (gethash :refid c))
           (let ((opinion (opinion-by-id (gethash :refid c))))
             ;;Could also add warstats/headlines for rooturl of opinion. Don't think we use now.
             (warstats< (assoc-cdr :url opinion))
             (headlines< (assoc-cdr :url opinion)))))
        (:reference
         (let ((opinion (opinion-by-id (gethash :refopinid c))))
           (warstats< (assoc-cdr :url opinion))
           (headlines< (assoc-cdr :url opinion))
           (opins< (gethash :refopinid c))
           (if (gethash :refd-opinion-id c)
               (let* ((ropin (opinion-by-id (gethash :refopinid c)))
                      (url (assoc-cdr :url ropin))
                      (rootid (assoc-cdr :rooturl ropin))
                      (rooturl (get-rooturl-by-id rootid)))
                 (warstats< url) (headlines< url)
                 (opins< (gethash :refopinid c))
                 (warstats< rooturl) (headlines< rooturl))
               (headlines< (assoc-cdr :reference c)))))
        (:question
         (let* ((opinion (gethash :id c))
                (url (assoc-cdr :url opinion)))
           (opins< (gethash :id c))
           (headlines< url) (warstats< url)))))))

(defun format-group-component-data (opinid depth)
  (if (question-opinion-p opinid)
      (format-group-question-data opinid depth)
      (format-group-reference-data opinid depth)))

(defun format-group-question-data (opinid depth)
  (let ((opinion (opinion-by-id opinid)))
    (hu:plist->hash
     (list
      :id opinid
      :rowtype :question
      :display-depth depth
      :rootid (assoc-cdr :rooturl opinion)
      :comment (assoc-cdr :comment opinion)))))

(defun format-group-reference-data (refid depth)
  (let* ((refopin (opinion-by-id refid))
         (refurl (assoc-cdr :reference refopin))
         (rootid (when (rooturl-p refurl)
                   (get-rooturl-id refurl)))
         (parent-rootid (assoc-cdr :rooturl refopin)))
    ;;We are ignoring references to opinions (for now?)
    ;;FIXME: No we aren't! This is broken. Ambiguous URLs slip through.
    (unless (opinion-exists-p refurl)
      (hu:plist->hash
       (if rootid
           (list
            :url refurl
            :display-depth depth
            :rowtype :rooturl
            :refparent parent-rootid
            :refid refid
            :title (grab-title refurl)
            :looks (when (authenticated?)
                     (get-looks (get-user-name) rootid))
            :rootid rootid)
           (let ((refdat (warflagger::outgoing-reference-data refid)))
             ;;FIXME: warstats should come from context
             (setf (getf refdat :warstats) (hu:plist->hash (getf refdat :warstats)))
             (setf (getf refdat :refd-opinion-warstats)
                   (hu:plist->hash (getf refdat :refd-opinion-warstats)))
             (list*
              :display-depth depth
              :rowtype :reference
              :refparent parent-rootid
              refdat)))))))

(defun format-group-data (discrootid tree)
  (cl-utilities:collecting
    (cl-utilities:collect
      (let ((url (get-rooturl-by-id discrootid)))
        (hu:plist->hash
         (list
          :rowtype :rooturl
          :display-depth 0
          :url url
          :rootid discrootid
          :title (grab-title url)
          :looks (when (authenticated?)
                   (get-looks (get-user-name) discrootid))))))
    (proto:dotree (opid tree :proc-leaf t :proc-branch nil)
      (when-let ((row (format-group-component-data opid (1- (length proto:*tree-stack*)))))
        (cl-utilities:collect row)))))

(defun reply-to-question-p (opinid)
  (multiple-value-bind (targid type) (get-target-id opinid)
    (and (eq type :opinion) (question-opinion-p targid) targid)))

(defun question-in-tree-address-p (opinid)
  (first-match #'question-opinion-p (cdr (reverse (tree-address opinid)))))

(defun %superquestion (qid questionids)
  "Are any of the items in questionids an ancestor of qid?"
  (first-match (lambda (x) (member x questionids)) (cdr (reverse (tree-address qid)))))

(defun add-questions-to-discussion-tree (tree)
  ;;Expects a list of lists. Each sublist has an opinid as its car.
  (when tree
    (let ((accum nil)
          (primary-questions
            (dolist (br tree)
              (cl-utilities:collecting
                (when (question-in-tree-address-p (car br))
                  (cl-utilities:collect (car br))))))
          (questions (make-hash-table)))
      (hu:collecting-hash-table (:mode :append :existing questions)
        (dolist (branch tree)
          (if-let ((qid (question-in-tree-address-p (car branch))))
            (progn
              (unless (key-in-hash? qid questions)
                ;; question and its answers will get inserted at first reply to question.
                (if-let ((superq (%superquestion qid primary-questions)))
                  (hu:collect superq qid)
                  (push qid accum)))
              (hu:collect qid branch))
            (push branch accum))))
      (labels ((proc (currlist &optional first)
                 (cl-utilities:collecting
                   (dolist (itm currlist)
                     (if (atom itm)
                         (if first
                             (progn
                               (cl-utilities:collect (list itm))
                               (mapc #'cl-utilities:collect (proc (gethash itm questions))))
                             (cl-utilities:collect (list itm (proc (gethash itm questions)))))
                         (cl-utilities:collect
                             (cons (car itm) (add-questions-to-discussion-tree (cdr itm)))))))))
        (proc (nreverse accum) t)))))

(defun grouped-data ()
  (let ((discroots (discussion-roots)))
    (multiple-value-bind (groups rootids)
        (cl-utilities:with-collectors (groups< rootids<)
          (dolist (dr discroots)
            (multiple-value-bind (tree rtids) (discussion-tree-for-root dr discroots)
              (groups< dr)
              (groups< (add-questions-to-discussion-tree tree))
              (rootids< (cons dr rtids)))))
      (cl-utilities:collecting
        (dolist (discroot (order-discussions-by-most-recent-opinion rootids))
          (cl-utilities:collect (format-group-data discroot (getf groups discroot))))))))

(defun grouped-page ()
  (mount-component (grouped-main)
    :data (lisp (ps-gadgets:as-ps-data (list* 'list (grouped-data))))))

;;FIXME: rework, maybe, someday
;; This retrieves a manually created list of discussion roots for the grouped (main) page.
;; - Grouped might not always be the main page
;; - Right now we don't have an automated way to collect keywords
;; - Automatic grouped page is creating a mess
;; So this will hopefully go away some day. Quick fix for now. MVP, all that stuff...
(defun load-static-grouped-list ()
  (with-open-file (s (asdf:system-relative-pathname 'warflagger "src/grouped.data"))
    (read s)))

(defun dump-static-grouped-suggestions ()
  (with-open-file (s (asdf:system-relative-pathname 'warflagger "src/grouped.sugg")
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (id (discussion-roots))
      (print (get-rooturl-by-id id) s))))
