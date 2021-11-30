(in-package :wf/web)

;;What will our datafile need?
;; - opins, warstats (opin and rooturl), headlines
;; - direction data
;; - can't do looks. Needs to be per user, but maybe a list of ids to request
;; - excerpt context (in opinions probably)
;; - titles for references?
;; - warstats for refs? Doesn't really make sense...
;; - group reference data
;; - keywords (hashtags). These could come out of warstats eventually. But maybe not per group.

(defun gather-grouped-data-requirements (components)
  ;;What about looks?
  (cl-utilities:with-collectors (opins< warstats< headlines<)
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
               (let* ((ropin (opinion-by-id (gethash :refd-opinion-id c)))
                      (url (assoc-cdr :url ropin))
                      (rooturl (assoc-cdr :rooturl ropin)))
                 (mapcar #'opins< (butlast (tree-address (gethash :refd-opinion-id c))))
                 (warstats< url) (headlines< url)
                 (opins< (gethash :refd-opinion-id c))
                 (warstats< rooturl) (headlines< rooturl)
               (headlines< (gethash :reference c))))))
        (:question
         (let* ((opinion (opinion-by-id (gethash :id c)))
                (url (assoc-cdr :url opinion)))
           (opins< (gethash :id c))
           (headlines< url) (warstats< url)))))))

;;Will replace gather-grouped-data-requirements
(defun gather-grouped-id-requirements (components)
  (cl-utilities:with-collectors (opins< roots<)
    (dolist (c components)
      (case (gethash :rowtype c)
        (:rooturl
         (roots< (gethash :url c))
         (when (gethash :refparent c)
           (opins< (assoc-cdr :iid (opinion-by-id (gethash :refid c))))))
        (:reference
         (opins< (assoc-cdr :iid (opinion-by-id (gethash :refopinid c))))
         (if (gethash :refd-opinion-id c)
             (let* ((ropin (opinion-by-id (gethash :refd-opinion-id c)))
                    (rrooturl (assoc-cdr :rooturl ropin)))
               (mapcar #'opins< (wf/ipfs::tree-address ropin))
               (roots< rrooturl))
             ;;FIXME: how do I know reference will be in title/warstats?
             (roots< (gethash :reference c))))
        (:question
         (opins< (assoc-cdr :iid (opinion-by-id (gethash :id c)))))))))

(defun format-group-component-data (opinid depth)
  (if (question-opinion-p opinid)
      (format-group-question-data opinid depth)
      (format-group-reference-data opinid depth)))

(defun format-group-question-data (opinid depth)
  (let ((opinion (opinion-by-id opinid)))
    (hu:plist->hash
     (list
      :id opinid
      :iid (assoc-cdr :iid opinion)
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
    (hu:plist->hash
     (if rootid
         (list
          :url refurl
          :display-depth depth
          :rowtype :rooturl
          :refparent parent-rootid
          :refid refid
          :refiid (assoc-cdr :iid refopin)
          :title (grab-title refurl)
          :title-key refurl
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
            :refid refid
            :refparent parent-rootid
            refdat))))))

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
          :title-key url
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

(defun grouped-data (&optional discroots)
  "Returns a list of lists. Each list represents a group. Each item in a group contains data for a single entry (row) on the grouped page. There are three types of entries: :rooturl :reference :question. Parameter discroots supplies rooturl ids upon which to base the groups. If not supplied, a guess will be made."
  (let ((discroots (or discroots (discussion-roots))))
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

(defun prep-data-for-grouped-json (rootlist)
  (let* ((discroots (mapcar (rcurry #'getf :url) rootlist))
         (discrootids (mapcar #'get-rooturl-id discroots))
         (keywords (mapcar (rcurry #'getf :keywords) rootlist))
         (groups (grouped-data discrootids)))
    (multiple-value-bind (opinions warstats headlines)
        (gather-grouped-data-requirements (flatten groups))
      (multiple-value-bind (opinion-iids rooturls)
          (gather-grouped-id-requirements (flatten groups))
        (list
         :groups groups
         :keywords
         (hu:alist->hash (pairlis discroots keywords))
         ;;FIXME: dual system. drop the stores.
         :opinion-store
         (hu:collecting-hash-table (:mode :replace)
           (dolist (opid opinions)
             (hu:collect opid (opinion-by-id opid :extra t))))
         :warstats-store
         (hu:collecting-hash-table (:mode :replace)
           (dolist (target warstats)
             (hu:collect target (hu:plist->hash (warstats-for-target target)))))
         :headlines
         (hu:collecting-hash-table (:mode :replace)
           (dolist (target headlines)
             (hu:collect target (get-headline-for-url target))))
         :opinions opinion-iids
         :rooturls rooturls)))))

(defun write-grouped-data-file ()
  (with-open-file (s (merge-pathnames "grouped.json" wf/local-settings:*warstats-path*)
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (cl-json:encode-json-plist (prep-data-for-grouped-json (load-static-grouped-list)) s)))

;;FIXME: rework, maybe, someday
;; This retrieves a manually created list of discussion roots for the grouped (main) page.
;; - Grouped might not always be the main page
;; - Right now we don't have an automated way to collect keywords
;; - Automatic grouped page is creating a mess
;; So this will hopefully go away some day. Quick fix for now. MVP, all that stuff...
(defun load-static-grouped-list ()
  (with-open-file (s (merge-pathnames wf/local-settings:*cache-path* "grouped.data")
                     :if-does-not-exist nil)
    (when s (read s))))

(defun dump-static-grouped-suggestions ()
  (with-open-file (s (merge-pathnames wf/local-settings:*cache-path* "grouped.sugg")
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (id (discussion-roots))
      (print (get-rooturl-by-id id) s))))
