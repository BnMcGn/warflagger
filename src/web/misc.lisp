(in-package :wf/web)


(defmethod json:encode-json ((object clsql-sys:wall-time) &optional stream)
  (write-char #\" stream)
  (clsql:format-time stream object :format :iso8601)
  (write-char #\" stream))

(defmethod json:encode-json ((object local-time:timestamp) &optional stream)
  (write-char #\" stream)
  (local-time:format-timestring stream object)
  (write-char #\" stream))

(defun test-js ()
  (ps
    (define-react-class thing
        (cl-react:psx
         (:a :href "http://www.google.com"
             (:span :class "text-green" "Click heare!" ))))
    (chain |ReactDOM|
           (render (create-element thing)
                   (chain document (get-element-by-id "things"))))))


(defun %opinion-data (opid text)
  (let ((data (opinion-from-id opid)))
    (if (assoc-cdr :excerpt data)
        (cons
         (list*
          :text-position
          (multiple-value-list
           (find-excerpt-position text (assoc-cdr :excerpt data)
                                  (or (assoc-cdr :excerptoffset data) 0))))
         data)
        data)))

(defun %fill-out-opinion-tree (tree text)
  (if (null tree)
      nil
      (let ((op (%opinion-data (caar tree) text)))
        (cons
         (cons
          op
          (%fill-out-opinion-tree (cdar tree)
                                  (create-textdata
                                   (aif (assoc :comment op) (cdr it) ""))))
         (%fill-out-opinion-tree (cdr tree) text)))))

(defun target-data (id)
  (let* ((url (get-rooturl-by-id id))
         (text
           (progn
             (unless (is-cached url)
               (error "Don't have that page!"))
             ;;FIXME: Suboptimal place, but somebody has to do it...
             (make-rooturl-real id)
             (grab-text url)))
         (opins
           (%fill-out-opinion-tree
            (opinion-tree-for-rooturl url) (create-textdata text)))
         (looks (get-looks (get-user-name) id))
         (warstats (generate-rooturl-warstats url)))

    (values
     (json:encode-json-to-string text)
     (json:encode-json-to-string opins)
     (json:encode-json-to-string looks)
     (json:encode-json-to-string warstats))))

(defun large-logo ()
  (html-out
    (:img :src "/static/img/wf_logo_large.png"
          :alt "[WarFlagger: Because someone is wrong on the internet]"
          :style "display: block; margin-left: auto; margin-right: auto; margin-top: 3em; margin-bottom: 3em;")))

(define-parts main-page-parts
  :@css-link "/static/css/push_button.css"
  :@notifications
  (let ((info (warflagger-user-info-bundle)))
    (lambda ()
      (unless (signed-up?)
        (html-out
          (:div :class "featurebox_side"
                (:h3 "Account")
                (:a :href (assoc-cdr :login-url info)
                    :class "push_button pb_green" "Sign Up")
                (:a :href (assoc-cdr :login-url info) "Log In"))))))
  :@inner
  (lambda ()
    (large-logo)
    (unless (warflagger:get-local-user-id (get-user-name))
      ;;Local user won't have an entry in DB if hasn't posted yet.
      (html-out
        ;;FIXME: find a proper way to style/display notifications
        (:div :style "margin: 4px; background-color: lightgreen; text-align: center"
         "You don't seem to have posted on WarFlagger yet."
         (:a :href "/introduction/" "Click here")
         " for a guide to posting.")))
    (html-thing-lister:render-list-for-sidebar
     (list :lister-type :thing :thing 'target)
     :label "Active targets:"
     :class "featurebox"
     :summary-width 40
     :pagequantity 20)))
