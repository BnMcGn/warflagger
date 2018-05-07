(in-package :wf/web)

(defun grouped-page ()
  (ps

    (def-component rooturl-summary
        (psx
         (:div
          :... (format-styling-data (@ this props))
          (:h3
           :class (strcat (flavor-from-warstats (prop warstats root))
                          "-old target-title")
           "Article: "
           (:headline :key 1
                      :title (prop title)
                      :external-link (prop url)
                      :url (strcat "/target/"
                                   ;;FIXME: id?
                                   (prop id (to-string))))
           (:display-warstats2 :key 2)))))

    (def-component grouped
        (psx
         (:div :style (create "margin-bottom" "1em")
               (when (prop warstats)
                 (collecting
                     (dotree (itm (ensure-array (prop group))
                                  :proc-branch nil :proc-leaf t)
                       (let ((depth (@ *tree-stack* length)))
                         (collect
                             ;;FIXME: support non-integer itm
                             (if (getprop (prop warstats) itm)
                                 (psx
                                  (:target-title
                                   :key (unique-id)
                                   :display-depth depth
                                   :warstats (getprop (prop warstats) itm)))
                                 (psx (:div "Loading...")))))))))))

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
                          :root-article-id rootid
                          :group (@ grp 0)))))))))

    ))
