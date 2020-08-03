(in-package :wf/web)

(define-ps-lib titlebar-components ()
  (ps

    (def-component flag-name
        (let ((label (prop opinion flag 1)))
          (psx
           (:span (+ " "
                     (chain label (char-at 0) (to-upper-case))
                     (chain label (slice 1)))))))

    (def-component opinion-icon
        ;;This opinion may be represented in other places on page, hence unid
        (let ((elid (strcat "opinion-icon-" (state unid))))
          (psx (:span
                :class "opinion-badge"
                :id elid
                :on-mouse-enter (@ this handle-mouse-enter)
                :on-mouse-leave (@ this handle-mouse-leave)
                (:a :key 0 :href (make-opinionid-url (prop opinion id))
                    (:img
                     :src (strcat "/static/img/small/wf_flag-"
                                  (chain
                                   (getprop (lisp
                                             (ps-gadgets:as-ps-data
                                              (hu:plist->hash warflagger:*flag-colors*)))
                                            (prop opinion flag 1))
                                   (slice 1))
                                  ".svg")))
                (:display-if
                 :key 1
                 :test (and (prop opinion-store) (prop warstats))
                 (:tool-tip
                  :style (popup-style)
                  :active (state viewable) :position "bottom"
                  :arrow "right"
                  :group "two"
                  :parent (strcat "#" elid)
                  (:opinion-info
                   :... (@ this props)
                   :opinion (prop opinion)))))))
      get-initial-state
      (lambda () (create viewable false unid (unique-id)))
      handle-mouse-enter
      (lambda (e)
        (set-state viewable true)
        (when (and (prop opinion-store) (prop warstats)) ;Wasn't looked at! see above.
          (funcall (prop look-handler) (prop opinion id))))
      handle-mouse-leave
      (lambda (e)
        (set-state viewable false)))

    ;;Reuse the vote-value name
    ;;(def-component vote-value
    ;;    (psx (:opinion-icon :opinion (prop opinion))))

    (def-component display-tree-address
        (psx
         (:span
          :class "tree_address"
          (collecting
            (dolist (id (prop tree-address))
              (unless (eq id (prop tree-address 0))
                (collect " > "))
              (collect
                  (psx
                   (:opinion-icon
                    :key id
                    ;;FIXME: Should maybe have full styling data, which means it should be cached
                    ;; elsewhere
                    :... (if (prop looks)
                             (format-looks-data {} id (prop looks))
                             {})
                    :opinion (getprop (prop opinion-store) id)
                    :warstats (prop warstats)
                    :opinion-store (prop opinion-store)
                    :look-handler (prop look-handler)))))))))

    ;;FIXME: React/CSS version of display-warstats in mood.lisp. Other should probably
    ;; go away eventually.
    (def-component display-warstats2
        (let ((names (lisp (cons 'create *indicator-names*)))
              (descs (lisp (cons 'create *warstat-text*))))
          (psx
           (:span :class "display-warstats"
                  (collecting
                      (dolist (k (lisp (cons 'list
                                          (map-by-2 (lambda (&rest x) (car x))
                                                    *warstat-text*))))
                        (collect
                            (psx (:span
                                  :key k
                                  :class k
                                  (:img
                                   :src
                                   (strcat "/static/img/" (getprop names k) ".svg")
                                   :title (getprop descs k)))))))))))

    (defun %plurify (value single multiple)
      (if (< 1 value)
          (strcat (chain value (to-string)) multiple)
          (strcat (chain value (to-string)) single)))

    (defun display-date-nicely (dstamp)
      (let ((past (ago dstamp)))
        (cond
          ((< 1 (chain past (get-years)))
           (%plurify (chain past (get-years)) " year ago" " years ago"))
          ((< 0 (chain past (get-months)))
           (%plurify (chain past (get-months)) " month ago" " months ago"))
          ((< 0 (chain past (get-weeks)))
           (%plurify (chain past (get-weeks)) " week ago" " weeks ago"))
          ((< 0 (chain past (get-days)))
           (%plurify (chain past (get-days)) " day ago" " days ago"))
          ((< 0 (chain past (get-hours)))
           (%plurify (chain past (get-hours)) " hour ago" " hours ago"))
          ((< 0 (chain past (get-minutes)))
           (%plurify (chain past (get-minutes)) " minute ago" " minutes ago"))
          (t (%plurify (chain past (get-seconds)) " second ago" " seconds ago")))))

    (def-component date-stamp
        (psx
         (:span
          (display-date-nicely
           (new (-date (chain -date (parse (prop opinion datestamp)))))))))

    ;;FIXME: lots of improvement here... Avatar? Stats? Faction?
    (def-component author-long
        (psx (:span (prop opinion authorname))))

    (def-component reply-link
        (let ((excerpt (prop excerpt))
              (offset (prop offset)))
          (psx
           (:form :class "opinion-reply" :action "/opinion/" :method "GET"
                  :style (create :display "inline-block")
                  (:input :type "hidden" :name "target" :key 1
                          :value (prop url))
                  (when excerpt
                    (psx (:input :type "hidden" :name "excerpt" :key 3
                                 :value (encode-u-r-i-component excerpt))))
                  (when offset
                    (psx (:input :type "hidden" :name "offset" :key 4
                                 :value offset)))
                  (if excerpt
                      (psx (:input :type "submit"
                                   :key 2
                                   :title (strcat "Reply to the excerpt: \"" excerpt "\"")
                                   :value "Reply to Excerpt"))
                      (psx (:input :type "submit" :value "Reply" :key 2)))))))

    (def-component reply-count
        (let* ((immediate (prop warstats replies-immediate))
               (total (prop warstats replies-total))
               (immed (and immediate (chain immediate (to-string))))
               (tot (and total (chain total (to-string)))))
          (psx
           (:span
            :class "reply-count"
            :title
            (strcat immed " direct responses, " tot " in conversation")
            (strcat " (" immed "/" tot ")")))))

    ;;FIXME: Headline will get considerably more complex in future. Placeholder.

    ;;Note: renamed to headline-core to make room for a loader in headline
    (def-component headline-core
        (let* ((title (prop title))
               (ext-link (when (prop external-link)
                           (psx (:a :key 1 :href (prop external-link)
                                    :title "Original article"
                                    (:span :class "headline-external")))))
               (elclass (if title "headline" "headline headline-empty"))
               (domain (if (prop domain)
                           (strcat " (" (prop domain) ")")
                           ""))
               (core (if (not-empty title)
                         (psx (:span :key 2 title domain))
                         (psx (:span :key 2 "[Title Not Available]" domain)))))
          (if (prop url)
              (psx (:span :key 3 :class elclass
                          (:a :key 4 :href (prop url) core) ext-link))
              (psx (:span :key 3 :class elclass core ext-link)))))

    (def-component headline
        (if (prop title)
            (psx (:headline-core :... (@ this props)))
            (psx (:text-server-client
                  :url (prop external-link)
                  (:headline-core :... (@ this props))))))

    (def-component comment-summary
        (let ((comment
                (if (prop comment)
                    (prop comment)
                    (let ((opin (prop opinion)))
                      (if (chain opin (has-own-property 'comment))
                          (@ opin comment)
                          nil)))))
          (if comment
              (psx (:span
                    :style (create 'white-space "nowrap" overflow "hidden" 'text-overflow "ellipsis")
                    comment))
              (psx (:span "")))))

    ))
