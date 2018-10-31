(in-package :wf/web)

(define-parts target-parts
  :@css-link "/static/css/target.css"
  ;;FIXME: Need better way to include:
  :@css-link "/static/css/react-tabs.css")

;; Depends on titlebar-components target-article target-thread target-summary

(define-ps-lib target-components ()
  (ps

    (setf -react-tabs (require "react-tabs"))
    (setf tabs (@ -react-tabs -tabs))
    (setf tab (@ -react-tabs -tab))
    (setf tab-list (@ -react-tabs -tab-list))
    (setf tab-panel (@ -react-tabs -tab-panel))

    (let ((counter 0))
      (defun unique-id ()
        (incf counter)))

    (defun rebreak (text)
      (chain (collecting
                 (dolist (string (chain text (split #\linefeed)))
                   (collect string)
                   (collect (psx (:br :key (unique-id))))))
             (slice 0 -1)))

    (defun opinion-p (itm)
      (and (not (atom itm))
           (chain itm (has-own-property "votevalue"))))

    (defun %overlap-p (start1 end1 start2 end2)
      (not (or (> start1 end2) (> start2 end1))))

    ;;Find all the indices where excerpts start or stop.
    (defun excerpt-segment-points (opset end)
      "End is the length of the text."
      (chain
       (collecting-set
           (dolist (itm opset)
             (collect (@ itm text-position 0))
             (collect (+ (@ itm text-position 0) (@ itm text-position 1))))
         (collect 0)
         (collect (1+ end)))
       (sort (lambda (a b) (- a b)))))

    (defun has-excerpt-p (opin)
      (chain opin (has-own-property :excerpt)))

    (defun has-found-excerpt-p (opin)
      (and (has-excerpt-p opin)
           (not (equal null (@ opin 'text-position 0)))))

    (defun %get-excerptless-opinions (opins)
      (collecting
          (dolist (o opins)
            (unless (and (@ o 0 excerpt)
                         (< 0 (@ o 0 excerpt length)))
              (collect o)))))

    ;;FIXME: Overall use of url, external-link, warflagger-link is inconsistent.
    (def-component target-title
        (psx
         (:div
          :... (format-styling-data (@ this props))
          (:h3
           :class (strcat (flavor-from-own-warstats (prop warstats root))
                          "-old target-title")
           (or (prop intro-text) "Target Page: ")
           (:headline :key 1
                      :title (prop title)
                      :url (prop warflagger-link)
                      :external-link (prop url))
           (:display-warstats2 :key 2)
           (prop children)
           (unless (prop hide-reply)
             (psx (:reply-link
                   :key 3
                   :url (prop url)
                   :excerpt (prop reply-excerpt)
                   :offset (prop reply-offset))))
           (when (prop show-count)
             (psx (:reply-count :key 4 :warstats (prop warstats root))))))))

    (defun %format-looks (looks)
      (let ((res (create)))
        (when looks
          (dolist (itm looks)
            (when (@ itm 1)
              (setf (getprop res (@ itm 1))
                    (@ itm 0)))))
        res))

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
         (:target-tabs
          :... (@ this props)
          :looks (state looks)
          :look-handler (@ this look-handler)))
      get-initial-state
      (lambda ()
        (create looks (%format-looks (prop looks))))
      look-handler
      (lambda (opinid)
        (unless (getprop (state looks) opinid)
          (when (prop username)
            (json-post-bind (res "/look-post/" (create :opinion opinid))))
          (set-state looks (set-copy (state looks) opinid t))))
      component-did-mount
      (lambda ()
        (when (prop username)
          (json-post-bind (res "/look-post/" (create :root (prop rootid)))))))

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
        (if (and  (state warstats) (state opinions) (state text))
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
            (psx (:h3 "Loading...")))
      component-did-mount
      (lambda ()
        (let ((url-root (strcat "/static/warstats"
                                (make-id-path (prop rootid))
                                (chain (prop rootid) (to-string)) "/")))
          (json-bind (res (strcat url-root "warstats.json"))
              (set-state warstats res))
          (json-bind (res (strcat url-root "references.json"))
              (set-state references res))
          (json-bind (res (strcat url-root "questions.json"))
              (set-state questions res))
          (json-bind (res (strcat url-root "opinions.json"))
              (set-state opinions res))
          (text-bind (res (strcat url-root "page.txt"))
                     (set-state text res))))
      get-initial-state
      (lambda () (create warstats nil opinions nil text nil)))

    ))

