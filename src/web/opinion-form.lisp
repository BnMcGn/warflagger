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
     (:type (:url :notnull) :description "Target URL")
     :excerpt
     (:type :string)
     :excerpt-offset
     (:type :integer :description "Excerpt Offset")
     :flag
     (:type (:pickone :options ,(format-flags) :notnull)
            :widget pickone-long)
     :votevalue
     (:type (:pickone :options (("-1" "-1") ("0" "0") ("1" "1"))
                      :notnull)
            :description "Vote Value")
     :reference (:type :url :description "Reference URL")
     :comment (:type :string))))

(defun opinion-form-page ()
  (html-out
    (:h2 "Enter an opinion")
    (:div :id "opform")
    (:script
     :type "text/javascript"
     (str
      (ps
        (render
         (create-element
          webhax-form
          (create :fieldspecs
                  (lisp-raw
                   (json:encode-json-to-string
                    (cl-hash-util:collecting-hash-table (:mode :replace)
                      (gadgets:map-by-2
                      (lambda (k v)
                        (cl-hash-util:collect k
                          (webhax-validate:prep-fieldspec-body-for-json v)))
                      *opinion-form-specs*))))
                  :data (create) ;FIXME: add prefills here
                  :layout custom-opform-layout
                  :wrapwidget false))
         (chain document
                (get-element-by-id "opform"))))))))

(define-parts opinion-components
  (add-part :@javascript #'webhax-widgets:ps-widgets)
  (add-part
   :@javascript
   (lambda ()
    (ps
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
           (:textarea
            :style (create :overflow "auto" :background "lightgrey" :border "1px"
                           :height "15em" :width "40em" :cursor "text")
            :on-mouse-up (@ this selection-change)
            :on-key-press (@ this selection-change)
            :value (prop text)))
        selection-change
        (lambda (ev)
          (say ev)))

      (def-component text-sample
          (psx
           (:text-sample-core
            :dispatch (prop dispatch)
            :text (state text)
            :excerpt (prop excerpt)
            :excerpt-offset (prop excerpt-offset)))
        get-initial-state
        (lambda ()
          (create :text "" :url "" :timeout nil))
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
            (chain this (load-from-server url))))
        load-from-server
        (lambda (url)
          (let ((msgfunc (@ this set-message)))
            (json-bind (results "/text-server/" :url (prop url))
               (case (@ results status)
                 ("success"
                  (set-state text (@ results text))
                  (msgfunc (@ results message)))
                 ("failure"
                  (msgfunc (@ results message)))
                 ("wait"
                  (msgfunc (@ results message))
                  (when (equal url (state url))
                    (set-state timeout
                               (set-timeout
                                (@ this load-from-server) 2000 url)))))))))

      (def-component opform-item
          (let ((count 0)) 
            (psx
             (:tr :key (prop keydata)
                  (:td :key "m1"
                       (or (prop description) (prop name)))
                  (children-map (prop children)
                                (lambda (child)
                                  (psx (:td :key (incf count) child))))))))

      (def-component custom-opform-layout
          (let ((state (@ this :state))
                (props (@ this props))
                (count 0))
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
                              :... props :key 1)))
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
          (create :message "")))

      ))))

#|
function set_target_text(url) {
	if ($("#target").val().length > 0) {
  $.getJSON("/opinion/get_text.json", {url: url},
		function(data) {
			if (data['status']=='success') {
				$('#textsample').val(data['text']);
				$('#message').html(data['message']);
				window.whiteless = $.grep(data['text'], complement(is_white_char));
				window.whiteless = window.whiteless.join("");
				window.white_indices = indexes(data['text'], is_white_char);
			}
			else if (data['status']=='failure') {
				$('#message').html(data['message']);
			} else if (data['status']=='wait') {
			  $('#message').html(data['message']);
				window.textTimeout = 
				  window.setTimeout(set_target_text, 2000, url);
		}})}
}

function string_locs_in_text(st) {
	wst = $.grep(st, complement(is_white_char)).join("");
	var locs = locations(window.whiteless, wst);
	return _.map(locs,
		function(itm) {
			return [itm, itm+wst.length];
		});
}

//ts.val().substring(ts[0].selectionStart, ts[0].selectionEnd)
//.replace(/\s+/g, ' ');
	
//-->
  |#
