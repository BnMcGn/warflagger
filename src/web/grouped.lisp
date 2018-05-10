(in-package :wf/web)

(defun grouped-page ()
  (ps

    (def-component grouped
        (psx
         (:div :style (create "margin-bottom" "1em")
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
                                          :intro-text "Article: "
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
                                      :url (strcat "/target/?newurl="
                                                   (encode-u-r-i-component itm))
                                      :reference-domain (url-domain itm)
                                      :external-link itm))))))))))))

    (defun %grouped-warstats-urls (tree)
      (let ((res (create)))
          (dolist (id (flatten tree) res)
            (when (equal (typeof id) "number")
              (setf (getprop res id)
                    (make-warstats-url id 'warstats))))))

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
          (collecting
              (do-keyvalue (rootid grp (prop tree))
                (collect
                    (psx (:grouped-loader
                          :key (unique-id)
                          :... (@ %thisref props)
                          :root-article-id (parse-int rootid)
                          :group (@ grp 0)))))))))

    ))
