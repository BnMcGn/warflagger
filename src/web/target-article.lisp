(in-package :wf/web)

(define-ps-lib target-article ()
  (ps

    (def-component target-root-article
        (psx
         (:div
          :style (create position :absolute width "80%" 'margin-bottom "20em")
          :on-click (@ this handle-click)
          (:target-title
           :key 0
           :... (@ this props)
           :reply-excerpt (state :reply-excerpt)
           :reply-offset (state :reply-offset)
           " ")
          (:h3
           :style (create 'font-style "italic" :background "lightgrey")
           :key 2
           "Text from article at " (url-domain (prop url)))
          (:hilited-text
           :key 3
           :text (prop text)
           :opinions (prop opinions)
           :focus (state focus)
           :focusfunc (@ this focus-func)
           :root-target-url (prop url)
           :tree-address (list)
           :warstats (prop warstats)
           :opinion-store (prop opinion-store)
           :dispatch (@ this dispatch)
           :looks (prop looks)
           :look-handler (prop look-handler))
          (:excerptless-opinions
           :key 4
           :... (@ this props))))
      get-default-state
      (lambda ()
        (create 'reply-excerpt "" 'reply-offset nil))
      handle-click
      (lambda () (set-state focus (list)))
      focus-func
      (lambda (new-focus) (set-state focus new-focus))
      get-initial-state
      (lambda () (create focus (prop focus)))
      dispatch
      (lambda (action)
        (when (eq (@ action type) :selection)
          (set-state :reply-excerpt (@ action excerpt)
                     :reply-offset (@ action offset)))))

    ))

