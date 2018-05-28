(in-package :wf/web)

(defun grouped-page ()
  (ps

    (def-component grouped
        (psx
         (:div :style (create "margin-bottom" "2em")
               :class "grouped"
               (when (prop warstats)
                 (collecting
                     (dotree (itm (ensure-array (prop group))
                                  :proc-branch nil :proc-leaf t)
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
             (warstats
              (when (@ data refid)
                (getprop (@ props warstats) (@ data refparent) (@ data refid)))))
        (if warstats
            (create :data-direction (@ warstats 'direction-on-root))
            (create))))

    (def-component grouped-loader
        (psx
         (:json-loader
          :store-name "warstats"
          :sources (%grouped-warstats-urls (prop group))
          :dispatch
          (lambda (existing incoming)
            (create-from-list
             (list :warstats (copy-merge-all (@ existing warstats)
                                             (@ incoming warstats)))))
          (:grouped :... (@ this props)))))

    (def-component grouped-page
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
