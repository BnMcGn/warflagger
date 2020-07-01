(in-package #:wf/web)

(defun format-flags ()
  (cl-utilities:collecting
      (cl-utilities:collect (list nil "--Choose a Flag--"))
    (loop for cat in *flag-categories*
       for flags in *flag-labels*
       do (dolist (flag flags)
            (let ((flagstring (format nil "~a: ~a" cat flag)))
              (cl-utilities:collect (list flagstring flagstring)))))))

(defun format-flag-descriptions ()
  (cl-hash-util:collecting-hash-table (:mode :replace)
    (loop for cat in *flag-categories*
       for descs in *flag-types-source*
       for flags in *flag-labels*
       do (loop for (k desc) on descs by #'cddr
             for flag in flags
             do (cl-hash-util:collect (format nil "~a: ~a" cat flag) desc)))))

(defparameter *opinion-form-specs*
  (gadgets:mapcan-by-2
   (lambda (k v) (list k (webhax-validate:normalize-fieldspec-body v)))
   `(:target
     ((:url :notnull) :description "Target URL")
     :excerpt
     (:string)
     :excerpt-offset
     (:integer :description "Excerpt Offset")
     :flag
     ((:pickone :options ,(format-flags) :notnull)
      ;;FIXME: pickone-long was a bare symbol. What should it be? 
            :widget "pickoneLong")
     :votevalue
     ((:pickone :options ((-1 "-1") (0 "0") (1 "1"))
                      :notnull)
            :description "Vote Value")
     :reference (:url :description "Reference URL")
     :comment (:string :widget :textentry))))

(proto:watch-for-recompile
  (defun opinion-form-page ()
    (check-signed-up)
    (bind-validated-input
        (&key
         (target :url)
         (target-id :unsigned-integer)
         (opinion-id :unsigned-integer)
         (excerpt :string)
         (offset :unsigned-integer))
      (let ((target (cond
                      (target target)
                      (target-id (get-rooturl-by-id target-id))
                      (opinion-id (assoc-cdr :url (opinion-by-id opinion-id))))))
        (html-out
         (:h2 "Enter an opinion")
         (mount-component (webhax-form)
           :fieldspecs
           (lisp
            (webhax-validate:convert-fieldspecs-to-ps-data *opinion-form-specs*))
           :data (create :target (lisp target)
                         :excerpt (lisp (when excerpt (quri:url-decode excerpt)))
                         :excerpt-offset (lisp offset))
           :layout custom-opform-layout
           :wrapwidget false
           'validation-url "/opinion-post/"))))))

;;FIXME: Needs to update automatically from any updates to the userfig, as per
;; userfig settings. Also, need default homepage url + provision for override.
(defun create-author-spec-from-current-user ()
  (list
   :wf-user (get-user-name)
   :screen-name (userfig:userfig-value 'screen-name)
   ;;:display-name (get-display-name)
   ;;:homepage (make-user-url (get-display-name))
   ))

(proto:watch-for-recompile
  (defun opinion-post-response ()
    (check-signed-up)
    (multiple-value-bind (values sig)
        (webhax-validate:validate-batch
         *key-web-input*
         *opinion-form-specs*)
      (when sig
        (let ((aid (get-local-user-id (get-user-name))))
          (unless aid
            (setf aid (apply #'insert-new-author
                             (create-author-spec-from-current-user))))
          (setf (gethash :flag values)
                (split-sequence-on-subseq ": " (gethash :flag values)))
          ;;FIXME: db needs a constraint for this
          (setf (gethash :datestamp values)
                (clsql:get-time))
          (let* ((newid (save-opinion-from-user (hu:hash->alist values) aid))
                 (savedopin (opinion-by-id newid)))
            ;;FIXME: Should be done in separate thread to reduce delay for user
            ;;Generate references
            (save-new-references (assoc-cdr :url savedopin))
            (write-all-rootid-warstats (assoc-cdr :rooturl savedopin)))))
      (list 200 '(:content-type "text/json")
            (list (webhax-validate:batch-response-json values sig))))))

;; Depends on: webhax-widgets:ps-widgets
(define-ps-lib opinion-components ()
  (ps

    (defun hilite-a-slice (text start end)
      (list
       (chain text (slice 0 start))
       (psx (:span :class "hilited" :key 1
                   :style (create 'background-color "orange")
                   :ref
                   (lambda (el)
                     (when el
                       (chain el (scroll-into-view))))
                   (chain text (slice start end))))
       (chain text (slice end))))

    (defun hilite-excerpt (textdata excerpt offset)
      (if (and (not-empty excerpt) (not-empty (@ textdata text)))
          (let ((bounds
                 (find-excerpt-start/end textdata excerpt (or offset 0))))
            (if bounds
                (hilite-a-slice (@ textdata text)
                                (elt bounds 0) (elt bounds 1))
                (progn
                  (say "Excerpt not found")
                  (@ textdata text))))
          (@ textdata text)))

    (def-component message
        (psx (:span (prop message))))

    (def-component flag-description
        (let ((descs (lisp (ps-gadgets:as-ps-data
                            (format-flag-descriptions)))))
          (psx (:div
                (:h5 :key 1 "Flag Info:")
                (getprop descs (prop formdata flag))))))


    (def-pure-component text-sample-core
        (psx
         (:pre :id "textsample"
               :style (create :overflow "auto" :background "lightgrey"
                              'white-space "pre-wrap":border "1px"
                              :height "15em" :width "40em" :cursor "text")
               :on-mouse-up (lambda (ev)
                              (chain %thisref (selection-change ev)))
               :on-key-press (lambda (ev)
                               (chain %thisref (selection-change ev)))
               (hilite-excerpt (prop textdata) (prop excerpt)
                               (prop excerpt-offset))))
      (selection-change (ev)
       (let* ((tsample (chain document (get-element-by-id "textsample")))
              (range (when (< 0 (chain rangy (get-selection) range-count))
                       (chain rangy (get-selection) (get-range-at 0)
                              (to-character-range tsample)))))
         (when range
           (destructuring-bind (excerpt offset)
               (get-location-excerpt (prop textdata)
                                     (@ range start)
                                     (@ range end))
             (funcall (prop dispatch)
                      (create :type :edit
                              :data (create :excerpt excerpt
                                            'excerpt-offset offset))))))))

    ;;FIXME: Find a way to make this not pound the server per keystroke.
    (def-component text-sample
        (psx
         (:text-sample-core
          :dispatch (prop dispatch)
          :text (state text)
          :textdata (state textdata)
          :excerpt (prop excerpt)
          :excerpt-offset (prop excerpt-offset)))
      get-initial-state
      (lambda ()
        (create :text "" :url "" :timeout nil :textdata []))
      get-default-props
      (lambda ()
        (create :url ""))
      component-will-mount
      (lambda ()
        (chain this (start-load-from-server (prop url))))
      component-will-receive-props
      (lambda (new-props)
        (chain this (start-load-from-server (@ new-props url))))
      reset-timeout
      (lambda ()
        (when (state timeout)
          (clear-timeout (state timeout))
          (set-state timeout nil)))
      set-message
      (lambda (mess)
        (funcall (prop dispatch) (create :type "message" :message mess)))
      start-load-from-server
      (lambda (url)
        (when (not (equal url (state url)))
          (set-state url url)
          (set-state text "")
          (chain this (reset-timeout))
          (set-state timeout
                     (set-timeout
                      (@ this load-from-server)
                      1000 url))))
      load-from-server
      (lambda (url)
        (let ((msgfunc (@ this set-message)))
          (json-bind (results "/text-server/" (:url url))
              (case (@ results status)
                ("success"
                 (set-state text (@ results text))
                 (set-state textdata (create-textdata (@ results text)))
                 (msgfunc (@ results message)))
                ("failure"
                 (msgfunc (@ results message)))
                ("wait"
                 (msgfunc (@ results message))
                 (when (equal url (state url))
                   (set-state timeout
                              (set-timeout
                               (@ %thisref load-from-server)
                               2000 url)))))))))

    (def-component opform-item
        (let ((count 0))
          (psx
           (:tr :key (prop keydata)
                (:td :key "m1"
                     :title (prop error)
                     :... (if (prop error)
                              (create :data-has-error "true")
                              (create))
                     (or (prop description)
                         (capitalize-first (ensure-string (prop name)))))
                (children-map (prop children)
                              (lambda (child)
                                (psx (:td :key (incf count)
                                          :style (create padding-left "1em")
                                          child))))))))

    (defun %has-errors (errors)
      (do-keyvalue (k v errors)
        (when v (return-from %has-errors t))))

    (def-component custom-opform-layout
        (let ((state (@ this :state))
              (props (@ this props))
              (count 0)
              (cdispatch (@ this custom-dispatch)))
          (psx
           (:form
            (:table
             (:tbody
              (children-map
               (prop children)
               (lambda (child)
                 (let ((name (@ child props name)))
                   (psx
                    (:opform-item
                     :keydata (incf count)
                     :description (@ child props fieldspec description)
                     :name name
                     :error (getprop (prop errors) name)
                     child
                     (case name
                       ("target"
                        (psx (:message :message (@ state message) :key 1)))
                       ("excerpt"
                        (psx (:text-sample
                              :key 1
                              :url (@ props formdata target)
                              :excerpt (@ props formdata excerpt)
                              :excerpt-offset
                              (@ props formdata excerpt-offset)
                              :dispatch cdispatch)))
                       ("flag"
                        (psx
                         (:flag-description :formdata (@ props formdata)
                                            :key 1)))))))))
              (:tr
               :key "user1"
               (:td
                (:input :type "button" :value "Post"
                        :on-click (@ this post-form))))
              (:tr
               :key "user0"
               (:td (if (%has-errors (prop errors))
                        "Opinion not posted: Some fields have errors"
                        (if (@ props success) "Opinion Posted" "")))))))))
      get-initial-state
      (lambda ()
        (create :message ""))
      custom-dispatch
      (lambda (data)
        (if (eql :message (@ data :type))
            (set-state :message (@ data :message))
            (funcall (prop dispatch) data)))
      post-form
      (lambda (data)
        (funcall (prop dispatch)
                 (create :type :submit))))

    ))

