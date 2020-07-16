(in-package :wf/web)

(define-ps-lib grouped-components ()
  (ps

    (def-component grouped
        (psx
         (:div :style (create 'margin-bottom "2em" position "relative")
               :key 1
               :class "grouped"
               (when (prop warstats)
                 (collecting
                   (dolist (itm (prop group))
                     (cond
                       ((eq (@ itm rowtype) :rooturl)
                        (collect
                            (if (getprop (prop warstats) (@ itm rootid))
                                (psx
                                 (:target-title
                                  :key (unique-id)
                                  :hide-reply t
                                  :show-count t
                                  :intro-text " "
                                  :warflagger-link (make-rootid-url (@ itm rootid))
                                  :extra-styling
                                  (grouped-styling-data itm (@ %thisref props))
                                  :warstats (getprop (prop warstats) (@ itm rootid))
                                  :... itm))
                                (psx (:div :key (unique-id) "Loading...")))))
                       ((eq (@ itm rowtype) :reference)
                        (collect
                            (psx
                             (:reference
                              :key (unique-id)
                              ;;FIXME: URL generation should only be in one place!
                              :headline {}
                              :styling-data
                              (create :data-display-depth (@ itm display-depth)
                                      :data-replies-total 0)
                              :warflagger-link (make-missing-rootid-url (@ itm url))
                              :reference-domain (url-domain (@ itm url))
                              :reference (@ itm url))))))))))))

    (defun %grouped-warstats-urls (group)
      (let ((res (create)))
        (dolist (item group)
          (when (chain item (has-own-property :rootid))
            (setf (getprop res (@ item rootid))
                  (make-warstats-url (@ item rootid) 'warstats))))
        res))

    (defun grouped-styling-data (data props)
      (let* ((pardata
              (when (@ data refparent)
                (getprop (@ props warstats) (@ data refparent))))
             (warstats
              (when pardata
                (getprop pardata (@ data refid)))))
        (if warstats
            (create :data-direction (@ warstats 'direction-on-root))
            (create))))

    (def-component grouped-loader
        (psx
         (:json-loader
          :store-name "warstats"
          :sources (%grouped-warstats-urls (prop group))
          :reducer #'copy-merge-all
          (:grouped :... (@ this props)))))

    (def-component grouped-main
        (psx
         (:div
          (:h2 :key "a1" "Discussions:")
          (collecting
            (dolist (group (prop data))
              (collect
                  (psx (:grouped-loader
                        :key (unique-id)
                        :group group))))))))

    ))

(defun format-group-component-data (refid depth)
  (let* ((refopin (opinion-by-id refid))
         (refurl (assoc-cdr :reference refopin))
         (rootid (when (rooturl-p refurl)
                   (get-rooturl-id refurl)))
         (parent-rootid (assoc-cdr :rooturl refopin)))
    ;;We are ignoring references to opinions (for now?)
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
           (list
            :url refurl
            :display-depth depth
            :rowtype :reference
            :refparent parent-rootid
            :refid refid))))))

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
    (proto:dotree (refid tree :proc-leaf t :proc-branch nil)
      (when-let ((row (format-group-component-data refid (1- (length proto:*tree-stack*)))))
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
              (groups< tree)
              (rootids< (cons dr rtids)))))
      (cl-utilities:collecting
        (dolist (discroot (order-discussions-by-most-recent-opinion rootids))
          (cl-utilities:collect (format-group-data discroot (getf groups discroot))))))))

(defun grouped-page ()
  (mount-component (grouped-main)
    :data (lisp (ps-gadgets:as-ps-data (list* 'list (grouped-data))))))

