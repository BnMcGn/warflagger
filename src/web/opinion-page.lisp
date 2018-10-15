(in-package :wf/web)

(defparameter *grandchild-shift* 15)

(defun opinion-page ()
  (ps

    (def-component opinion-root
        (psx
         (:div
          :style (create position :absolute width "80%" 'margin-bottom "20em")
          :on-click (@ this handle-click)
          (:target-title
           :key 0
           :... (@ this props)
           " ")
          (:hilited-text
           :key 2
           :text (prop text)
           :focus (prop focus)
           :root-target-url (prop url)
           :tree-address (list)
           :warstats (prop warstats)
           :opinion-store (prop opinion-store)
           :looks (prop looks)
           :look-handler (prop look-handler))))
      handle-click
      (lambda (ev) ));;FIXME: link to

    (def-component opinion-layer
        (let* ((opinion (getprop (prop opinion-store) (prop opinion-id)))
               (treead (@ opinion tree-address)))
          (psx
           (:div
            :class (chain "opinion-thread depth-"
                          (concat (chain treead length (to-string))))
            :... (format-styling-data
                  (copy-merge-all (@ this props) (create 'tree-address treead)))
            :on-click (lambda (e)
                        (setf (@ window location) (@ opinion url)))
            (:vote-value :key 1 :opinion opinion) " "
            (:flag-name :key 2 :opinion opinion) " "
            (:date-stamp :key 3 :opinion opinion) " "
            (:author-long :key 4 :opinion opinion) " "
            (:display-warstats2 :key 5)
            (:reply-link :key 6 :url (@ opinion url))
                         ;;:excerpt (state :reply-excerpt) :offset (state :reply-offset))
            (:div
             :key 9 :class "opinion-comment-wrapper"
             (when (@ opinion comment)
               (psx
                (:hilited-text
                 :key 8
                 :text (@ opinion comment)
                 :tree-address treead
                 :focus (prop focus)
                 :warstats (prop warstats)
                 :opinion-store (prop opinion-store)
                 :dispatch (@ this dispatch)
                 :hide-popup t
                 :looks (prop looks)
                 :look-handler (prop look-handler))))
             (:div
              :key 12
              :class "opinion-extras"
              (when (prop reference)
                (psx (:reference :key 10
                                 :... (prop reference)
                                 :styling-data
                                 (format-reference-styling-data (prop reference)))))
              (when (prop question)
                (psx (:question :key 11
                                        ;:... (prop question)
                                )))))))))

    (def-component opinion-page
        (psx
         (:div
          (:opinion-root
           :... (@ this props))
          (collecting
              (prop focus)
            (dolist (id (prop focus))
              (collect
                  (psx
                   (:opinion-layer
                    :key (unique-id)
                    :... (@ this props)
                    :opinion-id id))))))))))

