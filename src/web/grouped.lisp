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
                     (dotree (itm (ensure-array (prop group))
                              :proc-branch nil :proc-leaf t)
                       ;;*tree-stack* is from dotree
                       (let ((depth (@ *tree-stack* length)))
                         (cond ((numberp itm)
                                (collect
                                    (if (getprop (prop warstats) itm)
                                        (psx
                                         (:target-title
                                          :key (unique-id)
                                          :display-depth depth
                                          :hide-reply t
                                          :show-count t
                                          :intro-text " "
                                          :warflagger-link (make-rootid-url itm)
                                          :extra-styling
                                          (grouped-styling-data itm
                                                                (@ %thisref props))
                                          :warstats (getprop (prop warstats) itm)
                                          :... (getprop (prop roots) itm)))
                                        ;;FIXME: creates error or ugly
                                        (psx (:div "Loading...")))))
                               ((stringp itm)
                                (collect
                                    (psx
                                     (:reference
                                      :key (unique-id)
;;;FIXME: URL generation should only be in one place!
                                      :headline {}
                                      :styling-data
                                      (create :data-display-depth depth
                                              :data-replies-total 0)
                                      :warflagger-link (make-missing-rootid-url itm)
                                      :reference-domain (url-domain itm)
                                      :reference itm))))))))))))

    (defun %grouped-warstats-urls (tree)
      (let ((res (create)))
          (dolist (id (flatten tree) res)
            (when (equal (typeof id) "number")
              (setf (getprop res id)
                    (make-warstats-url id 'warstats))))))

    (defun grouped-styling-data (id props)
      (let* ((data (getprop (@ props roots) id))
             (pardata
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
#|
          (lambda (existing incoming)
            (create-from-list
             (list :warstats (copy-merge-all (@ existing warstats)
                                             (@ incoming warstats)))))
          |#
          (:grouped :... (@ this props)))))

    (def-component grouped-main
        (psx
         (:div
          (:h2 :key "a1" "Discussions:")
          (collecting
              (dolist (rootid (prop order))
                (collect
                    (psx (:grouped-loader
                          :key (unique-id)
                          :... (@ %thisref props)
                          :root-article-id (parse-int rootid)
                          :group (getprop (prop tree) rootid 0)))))))))

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
          :title (grab-title url)
          :looks (when (authenticated?)
                   (get-looks (get-user-name) discrootid))))))
    (proto:dotree (refid tree)
      (when-let ((row (format-group-component-data refid (length proto:*tree-stack*))))
        (cl-utilities:collect row)))))

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
    :data (lisp (ps-gadgets:as-ps-data (grouped-data)))))

;;OBSOLETE
(defun grouped-pagex ()
        ;;Treed list of refopin ids.
  (let* ((tree (cluster-discussions))
         ;;Ordered list of discussion root keys
         (discroots (map-by-2 (lambda (&rest params) (car params)) tree))
         (refids (flatten
                  (map-by-2 (lambda (&rest params) (car (second params))) tree)))
         (roots
           (collecting-hash-table (:mode :replace)
             (dolist (id refids)
               (let* ((refopin (opinion-by-id id))
                      (refurl (assoc-cdr :reference refopin))
                      (rootid (when (rooturl-p refurl)
                                (get-rooturl-id refurl)))
                      (parent-rootid (assoc-cdr :rooturl refopin)))
                 ;;We are ignoring references to opinions (for now?)
                 (unless (opinion-exists-p refurl)
                   (hu:collect
                       (or rootid refurl)
                     (hu:plist->hash
                      (if rootid
                          (list
                           :url refurl
                           :refparent parent-rootid
                           :refid id
                           :title (grab-title refurl)
                           :looks (when (authenticated?)
                                    (get-looks (get-user-name) rootid))
                           :rootid rootid)
                          (list
                           :url refurl
                           :refparent parent-rootid
                           :refid id)))))))

             (dolist (id discroots)
               (let ((url (get-rooturl-by-id id)))
                 (hu:collect
                     id
                   (hu:plist->hash
                    (list
                     :url url
                     :title (grab-title url)
                     :looks (when (authenticated?)
                              (get-looks (get-user-name) id)))))))))
         (tree
           (flatten-1
            (map-by-2
             (lambda (k v)
               (list
                k (list
                   (list*
                    k (and (car v)
                           (proto:mapleaves
                            (lambda (x)
                              (warflagger::reference-end-result
                               (assoc-cdr :reference (opinion-by-id x))))
                            (car v)))))))
             tree))))
    (mount-component (grouped-main)
      :roots (lisp (ps-gadgets:as-ps-data roots))
      :tree (lisp (let ((ps-gadgets:*assume-list* t))
                    (ps-gadgets:as-ps-data (hu:plist->hash tree))))
      :order (lisp (if discroots
                       (ps-gadgets:as-ps-data discroots)
                       '(list))))))
