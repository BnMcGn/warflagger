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
                                 (:direction-arrow
                                  :key (unique-id)
                                  :id (@ itm refid)
                                  :display-depth (@ itm display-depth)
                                  :... (@ this props)
                                  (:target-title
                                   :style (create :margin-left "0px")
                                   :key (unique-id)
                                   :hide-reply t
                                   :show-count t
                                   :intro-text " "
                                   :warflagger-link (make-rootid-url (@ itm rootid))
                                   :extra-styling
                                   (grouped-styling-data itm (@ %thisref props))
                                   :warstats (getprop (prop warstats) (@ itm rootid))
                                   :... itm
                                   )))
                                (psx (:div :key (unique-id) "Loading...")))))
                       ((eq (@ itm rowtype) :reference)
                        (collect
                            (psx
                             (:direction-arrow
                              :key (unique-id)
                              :id (@ itm refid)
                              :display-depth (@ itm display-depth)
                              :... (@ this props)
                              (:reference
                               :key (unique-id)
                               ;;FIXME: URL generation should only be in one place!
                               :headline {}
                               :styling-data
                               (create :data-display-depth (@ itm display-depth)
                                       :data-replies-total 0)
                               :warflagger-link (make-missing-rootid-url (@ itm url))
                               :reference-domain (url-domain (@ itm url))
                               :reference (@ itm url))))))
                       ((eq (@ itm rowtype) :question)
                        (collect
                            (psx
                             (:question
                              :key (unique-id)
                              :comment (@ itm comment)
                              :opinion (getprop (prop opinion-store) (@ itm id))
                              :warstats (getprop (prop warstats) (@ itm rootid))
                              ;;FIXME: Want grouped-styling-data for direction-on-root arrow?
                              ;;FIXME: Might want full styling-data, but need opinion-store
                              :styling-data
                              (create :data-display-depth (@ itm display-depth)
                                      :data-replies-total 0)
                              :... itm)))))))))))

    (defun popup-style ()
      (create
       :style
       (create
        :background "rgba(255, 255, 255, 0.7)"
        :padding "7px")
       'arrow-style
       (create
        :background "transparent"
        :border-bottom-color "transparent")))

    ;;FIXME: This duplicates tooltip display code from opinion-icon.
    (def-component direction-arrow
        (let* ((elid (strcat "direction-arrow-" (state unid)))
               (opinion (getprop (prop opinion-store) (prop id)))
               (warstats (when opinion
                           ;;FIXME: warstats are not formatted correctly.
                           (getprop (prop warstats) (@ opinion rooturl))))
               (warstat (when warstats
                          (getprop warstats (prop id))))
               (imgsrc (when warstat
                         (strcat "/static/img/direction-" (@ warstat 'direction-on-root) ".svg"))))
          (psx
           (:div
            :style (create :position :relative)
            (:span :class "direction-arrow"
                   :id elid
                   :key 4
                   :... (create :data-display-depth (prop display-depth))
                   :style (create :position :absolute
                                  :top "-7px"
                                  :left "-24px")
                   :on-mouse-enter (@ this handle-mouse-enter)
                   :on-mouse-leave (@ this handle-mouse-leave)
                   (:display-if
                    :key 0
                    :test imgsrc
                    (:img
                     :key 0
                     :style (create :width "18px" :height "45px")
                     :src imgsrc))
                   (:display-if
                    :key 1
                    :test (and (prop opinion-store) (prop warstats))
                    (:tool-tip
                     :key 1
                     :active (state viewable)
                     :style (popup-style)
                     :position "bottom"
                     :arrow "left"
                     :group "two"
                     :parent (strcat "#" elid)
                     (:opinion-info
                      :... (@ this props)
                      :warstats warstats
                      :opinion opinion))))
            (prop children))))
      get-initial-state
      (lambda () (create viewable false unid (unique-id)))
      handle-mouse-enter
      (lambda (e)
        (set-state viewable true)
        ;;FIXME: looks disabled here. Does it make sense to enable? Perhaps we aren't seeing enough
        ;; of the conversation?
        (when (and nil (prop opinion-store) (prop warstats)) ;Wasn't looked at! see above.
          (funcall (prop look-handler) (prop id))))
      handle-mouse-leave
      (lambda (e)
        (set-state viewable false)))

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
          :key 1
          :store-name "warstats"
          :sources (%grouped-warstats-urls (prop group))
          :reducer #'copy-merge-all
          (:grouped :key 1 :... (@ this props)))))

    (def-component opinion-store-loader
        (psx
         (:json-loader
          :store-name 'opinion-store
          :sources (mapcar (lambda (id) (make-warstats-url id :opinions)) (prop rootids))
          :reducer
          (lambda (store incoming)
            (let ((res (reformat-opinions incoming)))
              (copy-merge-all store (@ res 1))))
          (prop children))))

    (def-component grouped-main
        (psx
         (:div
          (:h2 :key "a1" "Discussions:")
          (:opinion-store-loader
           :key 2
           :rootids (unique (collecting
                              (dolist (group (prop data))
                                (dolist (itm group)
                                  (when (chain itm (has-own-property :rootid))
                                    (collect (@ itm rootid)))))))
           (collecting
             (dolist (group (prop data))
               (collect
                   (psx (:grouped-loader
                         :key (unique-id)
                         :group group)))))))))

    ))

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

