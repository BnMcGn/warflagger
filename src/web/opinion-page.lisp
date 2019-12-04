(in-package :wf/web)

(defparameter *grandchild-shift* 15)

(define-ps-lib opinion-page ()
  (ps

    (def-component opinion-root
        (psx
         (:div
          :class "opinion-root"
          :style (create position :absolute width "80%" 'margin-bottom "20em")
          :on-click (@ this handle-click)
          (:target-title
           :key 0
           :hide-reply t
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
      (lambda (ev) (setf (@ window location) (make-rootid-url (prop rootid)))))

    (def-component opinion-layer
        (let* ((opinion (getprop (prop opinion-store) (prop opinion-id)))
               (treead (@ opinion tree-address))
               (topmost (eql (@ treead length) (prop focus length))))
          (psx
           (:div
            :class (chain "opinion-layer opinion-thread depth-"
                          (concat (chain treead length (to-string))))
            :... (format-styling-data
                  (copy-merge-all (@ this props) (create 'tree-address treead)))
            :on-click (lambda (e)
                        (setf (@ window location) (make-opinionid-url (@ opinion id))))
            (:opinion-icon :key 1 :opinion opinion
                           :look-handler (prop look-handler) :looks (prop looks)) " "
            (:flag-name :key 2 :opinion opinion) " "
            (:date-stamp :key 3 :opinion opinion) " "
            (:author-long :key 4 :opinion opinion) " "
            (:display-warstats2 :key 5)
            (when topmost
              ;;FIXME: Maybe shouldn't instantly set look if comment text is longer than a page?
              (funcall (prop look-handler) (@ opinion id))
              (psx (:reply-link
                    :key 6 :url (@ opinion url)
                    :excerpt (state :reply-excerpt) :offset (state :reply-offset))))
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
                                ))))
             (when topmost
               (psx (:excerptless-opinions
                     :key 13
                     :... (@ this props)
                     :tree-address treead)))))))
      get-initial-state
      (lambda () (create))
      dispatch
      (lambda (action)
        (when (eq (@ action type) :selection)
          (set-state :reply-excerpt (@ action excerpt)
                     :reply-offset (@ action offset)))))

    (def-component opinion-page
        (psx
         (:div
          :class "opinion-page"
          (:look-loader
           :looks (prop looks)
           (:opinion-root
            :... (@ this props))
           (collecting

             (prop focus)
             (dolist (id (prop focus))
               (collect
                   (psx
                    (:opinion-layer
                     :key id
                     :... (@ this props)
                     :opinion-id id)))))))))))

