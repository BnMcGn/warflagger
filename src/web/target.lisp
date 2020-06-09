(in-package :wf/web)

(define-parts target-parts
  :@css-link "/static/css/target.css"
  ;;FIXME: Need better way to include:
  :@css-link "/static/css/react-tabs.css")

;; Depends on titlebar-components displayables target-article target-thread target-summary

(define-ps-lib target-components ()
  (ps

    (setf -react-tabs (require "react-tabs"))
    (setf tabs (@ -react-tabs -tabs))
    (setf tab (@ -react-tabs -tab))
    (setf tab-list (@ -react-tabs -tab-list))
    (setf tab-panel (@ -react-tabs -tab-panel))

    ;;FIXME: Url should remember tab settings. Also remember user's pref in session
    (def-component target-tabs
        (psx
         (:tabs
          :default-index (prop tmode)
          :on-select (lambda (index)
                       (setf (@ window location href)
                             (update-url-parameter (@ window location href)
                                                   "tmode"
                                                   index)))
          (:tab-list
           :key 1
           (:tab :key 2 "Article View")
           (:tab :key 3 "Comment View")
           (:tab :key 4 "Summary"))
          (:tab-panel :key 5
                      (:target-root-article :... (@ this props)))
          (:tab-panel :key 6
                      (:target-root-thread :... (@ this props)))
          (:tab-panel :key 7
                      (:target-root-summary :... (@ this props))))))

    (def-component target-root
        (psx
         (:look-saver-for-rooturl
          (:look-loader
           :looks (prop looks)
           (:target-tabs
            :... (@ this props))))))

    (def-component look-loader
        (children-map (prop children)
                      (lambda (child)
                        (clone-element child (create :looks (state looks)
                                                     'look-handler (@ %thisref look-handler)))))
      get-initial-state
      (lambda ()
        ;;FIXME: Looks should be loaded directly from server. Needs REST or something.
        (create looks (prop looks)))
      look-handler
      (lambda (opinid)
        (when (state looks) ;;Are looks enabled?
          (unless (getprop (state looks) opinid)
           (when (prop username)
             (json-post-bind (res "/look-post/" (create :opinion opinid))))
           (set-state looks (set-copy (state looks) opinid t))))))

    (def-component look-saver-for-rooturl
        (prop children)
      component-did-mount
      (lambda ()
        (when (prop username)
          (json-post-bind (res "/look-post/" (create :root (prop rootid)))))))

    (defun sort-compare-opinions (opa opb)
      ;;FIXME: Special treatment of missing excerpt opinions?
      (let ((posa (and (chain opa (has-own-property 'text-position))
                       (< 0 (@ opa 'text-position length))
                       (@ opa 'text-position 0)))
            (posb (and (chain opb (has-own-property 'text-position))
                       (< 0 (@ opb 'text-position length))
                       (@ opb 'text-position 0))))
        (if posa
            (if posb
                (- posa posb)
                1) ;;Excerptless comes first
            (if posb
                -1 ;; Ditto
                (- (@ opa datestamp) (@ opb datestamp))))))

    (defun %reformat-opinions (opins)
      (let* ((opinstore (create))
             (opins
              (collecting
                  (labels ((proc (tree address)
                             (dolist (branch tree)
                               (let ((newadd (chain address (concat (@ branch 0 id)))))
                                 (setf (@ opinstore (@ branch 0 id)) (@ branch 0))
                                 (setf (@ branch 0 tree-address) newadd)
                                 (collect newadd)
                                 (when (< 1 (@ branch length))
                                   (proc (chain branch (slice 1)) newadd))))))
                    (proc opins (list))))))
        ;;FIXME: can't sort here. They're trees!
        ;; will need to think out sorting.
        ;;(chain opins (sort sort-compare-opinions))
        (list opins opinstore)))

    (def-component target-loader
        ;;FIXME: Needs error handling
        (if (and (state warstats) (state opinions) (state text))
            (let ((opdat (%reformat-opinions (state opinions)))
                  (child-element (or (prop child) target-root)))
              (psx (:child-element
                    :... (@ this props)
                    :warstats (state warstats)
                    :references (state references)
                    :questions (state questions)
                    :opinions (state opinions)
                    :tree-addresses (@ opdat 0)
                    :opinion-store (@ opdat 1)
                    :text (state text))))
            ;;FIXME: decorate
            ;; Should have a link to opine even when missing. Also, a link to supply text.
            (psx (:h3 "Loading...")))
      component-did-mount
      (lambda ()
        (let ((url-root (strcat "/static/warstats"
                                (make-id-path (prop rootid))
                                (chain (prop rootid) (to-string)) "/")))
          (json-bind (res (strcat url-root "warstats.json") ())
              (set-state warstats res))
          (json-bind (res (strcat url-root "references.json") ())
              (set-state references res))
          (json-bind (res (strcat url-root "questions.json") ())
              (set-state questions res))
          (json-bind (res (strcat url-root "opinions.json") ())
                     (set-state opinions res))
          ;;FIXME: can we do this with text-server?
          (text-bind (res (strcat url-root "page.txt"))
                     (set-state text res))))
      get-initial-state
      (lambda () (create warstats nil opinions nil text nil references {})))

    ;;New target stuff

    ;;FIXME: This is not reliably retrying on "wait"
    (def-component text-server-client
        (children-map (prop children)
                      (lambda (child)
                        (clone-element child (state data))))
      get-initial-state
      (lambda () (create data (create text "" title "")
                         interval (or (prop interval) 3000)
                         attempts (or (prop attempts) 10)))
      component-did-mount
      (lambda ()
        (chain this (load-from-server)))
      load-from-server
      (lambda ()
        (json-bind (results "/text-server/" (:url (prop url)))
           (let ((data (create text (@ results text)
                               title (@ results title)
                               'text-status (@ results status)
                               'text-message (@ results message))))
              (case (@ results status)
                ("success" (set-state data data))
                ("failure" (set-state data data))
                ("wait"
                 (if (< 0 (state attempts))
                     (progn
                       (set-state data data attempts (1- (state attempts)))
                       (set-timeout (@ this load-from-server) (state interval)))
                     (progn
                       (setf (@ data text-status) "failure"
                             (@ data text-message) "Timed out")
                       (set-state data data)))))))))

    (def-component new-target
        (psx
         (:text-server-client
          :url (prop url)
          (:new-target-body :... (@ this props)))))

    (def-component new-target-body
        (if (eq "success" (prop text-status))
            (psx (:target-root-article
                  :text (prop text)
                  :opinions (list)
                  :url (prop url)
                  ;;FIXME: It might not be new by the time the link is followed. We should check.
                  :opinion-store (create)
                  :warstats (lisp (as-ps-data (blank-warstat-for-web)))))
            (psx (:text-missing
                  :... (@ this props)
                  :warstats (lisp (as-ps-data (blank-warstat-for-web)))))))

    ;; Display of missing text:
    ;; What can the user do about a missing text?
    ;; FIXME: check that the opinion form will allow a non-found excerpt to be posted.
    ;; Why might the text be missing?
    ;; - text-server has not had time to process. Wait
    ;; - 404 error. User might supply the text, or supply an alternate URL.
    ;; - robots.txt forbids. User could supply.
    ;; - Copyright issue. Original publisher forbids us from displaying whole text. Can we check
    ;; excerpts while not displaying text?
    ;; - Misc. error. User can supply text.
    ;; - Textract decides that text is too large. What was the user trying to do???
    ;; - User can post declaration that a given page contains a given text at a given date. Other users
    ;; can dispute the truth of this.
    ;; FIXME: we do not have such a mechanism at this time.

    (def-component text-missing
        (psx
         (:div
          (:target-title
           :key 1
           :... (@ this props))
          (:h3
           :style (create 'font-style "italic" :background "lightgrey")
           :key 2
           "Text from article at " (url-domain (prop url)) " is not currently available." )
          (:h4
           :key 3
           "Reason: " (prop text-message))
          (:ul :key 4
               (:li :key 1
                    "You may still post flags on this article, though excerpts must be filled by hand.")
               (:li :key 2
                    "If the same text is available at another URL, please indicate the alternative with the SameThing flag.")
               (:li :key 3
                    "Texts may be manually inserted using [not implemented]")))))


    ))

