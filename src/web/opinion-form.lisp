(in-package #:wf/web)

(defun format-flags ()
  (gadgets:collecting
      (gadgets:collect (list nil "--Choose a Flag--"))
    (loop for cat in *flag-categories*
       for flags in *flag-labels*
       do (dolist (flag flags)
            (let ((flagstring (format nil "~a: ~a" cat flag)))
              (gadgets:collect (list flagstring flagstring)))))))

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
            :widget pickone-long)
     :votevalue
     ((:pickone :options (("-1" "-1") ("0" "0") ("1" "1"))
                      :notnull)
            :description "Vote Value")
     :reference (:url :description "Reference URL")
     :comment (:string))))

(watch-for-recompile
  (defun opinion-form-page ()
    (bind-validated-input
        (&key
         (target :url)
         (excerpt :string)
         (offset :unsigned-integer))
      (html-out
        (:h2 "Enter an opinion")
        (mount-component (webhax-form)
          :fieldspecs
          (lisp-raw
           (webhax-validate:convert-fieldspecs-to-json *opinion-form-specs*))
          :data (create :target (lisp target)
                           :excerpt (lisp excerpt)
                           :excerpt-offset (lisp offset))
          :layout custom-opform-layout
          :wrapwidget false
          'validation-url "/opinion-post/")))))

(watch-for-recompile
  (defun opinion-post-response ()
    (multiple-value-bind (values sig)
        (webhax-validate:validate-batch
         *key-web-input*
         *opinion-form-specs*)
      (when sig
        )
      (list 200 '(:content-type "text/json")
            (list (webhax-validate:batch-response-json values sig))))))

(define-parts opinion-components
  :@javascript #'webhax-widgets:ps-widgets
  :@javascript-link "/static/node_modules/rangy/lib/rangy-core.js"
  :@javascript-link "/static/node_modules/rangy/lib/rangy-textrange.js"
  :@javascript
  (lambda ()
    (ps

      ;;FIXME: Duplicate of lisp functions in excerpt.lisp
      ;;Would be nice to only implement once.
      (defun create-textdata (text)
        (let ((res (create :text text :whitespace (create)))
              (found nil))
          (dotimes (i (length text))
            (when (member (elt text i) *whitespace-characters*)
              (unless found (setf found i))
              (dolist (j (range found (1+ i)))
                (incf (getprop res 'whitespace j)))
              (setf found nil)))
          res))

      (defun contiguous-whitespace? (tdat index)
        (or (getprop tdat 'whitespace index) 0))

      (defun excerpt-here? (tdat excerpt index)
        (let ((exdat (create-textdata excerpt))
              (text (@ tdat text)))
          (loop with tind = index
             with eind = 0
             with tlen = (length text)
             with elen = (length excerpt)
             do (progn
                  (when (eq elen eind) (return-from excerpt-here? tind))
                  (when (eq tlen tind) (return-from excerpt-here? nil))
                  (let ((ewhite (contiguous-whitespace? exdat eind))
                        (twhite (contiguous-whitespace? tdat tind)))
                    (if (and (eq 0 ewhite) (eq 0 twhite)
                             (eq (elt excerpt eind) (elt text tind)))
                        (progn (incf tind) (incf eind))
                        (if (or (eq 0 ewhite) (eq 0 twhite))
                            (return-from excerpt-here? nil)
                            (progn (incf tind twhite)
                                   (incf eind ewhite)))))))))

      (defun find-excerpt-position (tdat excerpt &optional (offset 0))
        (dotimes (i (length (@ tdat text)))
          (let ((loc (excerpt-here? tdat excerpt i)))
            (when loc
              (if (< 0 offset)
                  (decf offset)
                  (return (list i (- loc i))))))))

      ;;End of duplicate functions

      (defun clean-string-for-excerpt (the-string)
        (collecting-string
          (let ((last-was-white nil))
            (dotimes (i (length the-string))
              (if (member (elt the-string i) *whitespace-characters*)
                  (unless last-was-white
                    (setf last-was-white t)
                    (collect #\ ))
                  (progn
                    (setf last-was-white nil)
                    (collect (elt the-string i))))))))

      (defun calculate-offset (tdat excerpt startloc)
        (if (not-empty excerpt)
            (let ((res 0))
              (dotimes (i startloc)
                (when (excerpt-here? tdat excerpt i)
                  (incf res)))
              res)
            nil))

      (defun get-location-excerpt (tdat start end)
        (let* ((excerpt (chain tdat text (slice start end)))
               (excerpt (clean-string-for-excerpt excerpt))
               (offset (calculate-offset tdat excerpt start)))
          (list excerpt offset)))

      (defun find-excerpt-start/end (tdat excerpt &optional (offset 0))
        (let ((pos (find-excerpt-position tdat excerpt offset)))
          (when pos
            (list (elt pos 0) (+ (elt pos 0) (elt pos 1))))))

      (defun hilite-a-slice (text start end)
        (list
         (chain text (slice 0 start))
         (psx (:span :class "hilited" :key 1
                     :style (create 'background-color "orange")
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
          (let ((descs (lisp-raw (json:encode-json-to-string
                                  (format-flag-descriptions)))))
            (psx (:div
                  (:h5 :key 1 "Flag Info:")
                  (getprop descs (prop formdata flag))))))

      (def-component text-sample-core
          (psx
           (:pre :id "textsample"
                 :style (create :overflow "auto" :background "lightgrey"
                                'white-space "pre-wrap":border "1px"
                                :height "15em" :width "40em" :cursor "text")
                 :on-mouse-up (@ this selection-change)
                 :on-key-press (@ this selection-change)
                 (hilite-excerpt (prop textdata) (prop excerpt)
                                 (prop excerpt-offset))))
        selection-change
        (lambda (ev)
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
            (json-bind (results "/text-server/" :url url)
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
                                 (@ component-this-ref load-from-server)
                                 2000 url)))))))))

      (def-component opform-item
          (let ((count 0))
            (psx
             (:tr :key (prop keydata)
                  (:td :key "m1"
                       (or (prop description) (prop name)))
                  (children-map (prop children)
                                (lambda (child)
                                  (psx (:td :key (incf count)
                                            :style (create padding-left "1em")
                                            child))))))))

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
                   (psx
                    (:opform-item
                     :keydata (incf count)
                     :description (@ child props description)
                     :name (@ child props name)
                     child
                     (case (@ child props name)
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
                                            :key 1))))))))
                (:tr
                 :key "user1"
                 (:td
                  (:input :type "button" :value "Post"
                          :on-click (@ this post-form)))))))))
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

      )))

