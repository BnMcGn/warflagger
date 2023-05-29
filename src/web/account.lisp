(in-package #:wf/web)

(defun login-link-js (url)
  (if clath:*in-logout-page*
      (ps-inline
       (setf (@ window location href) (lisp url)))
      (ps-inline
       (setf (@ window location href)
             (strcat
              (lisp (concatenate 'string url "?destination="))
              (@ window location href))))))

(defun %logo ()
  (html-out
    (:a :href wf/local-settings:*base-url*
        :style "padding-right: 1.5rem"
        (:img :src "/static/img/wf_logo_small.png" :alt "[WarFlagger]"))))

(defun get-apparent-display-name (loginid)
  (if-let ((id (get-local-user-id loginid)))
    (author-representation-from-row (get-author-data id))
    (userfig:userfig-value-for loginid 'screen-name)))

(defun account-bar ()
  (let* ((info (warflagger-user-info-bundle))
         (name (when (webhax-user:signed-up?)
                 (get-apparent-display-name (get-user-name))))
         (links
           (if (webhax-user:signed-up?)
               (lambda ()
                 (html-out
                   (:a :href (assoc-cdr :settings-url info) "Settings")
                   (:a :href (assoc-cdr :logout-url info) "Sign&nbsp;out")))
               (lambda ()
                 (html-out
                   (:a :onclick
                       (login-link-js (assoc-cdr :login-url info))
                       :href "#" "Sign&nbsp;Up")
                   (:a :onclick
                       (login-link-js (assoc-cdr :login-url info))
                       :href "#" "Log&nbsp;In"))))))
    (html-out
      (:div
       :class "container-fluid"
       (:div
        :class "row"
        (:div :class "col-sm-2 wf-sidebar-width" " ")
        (:div
         :class "col"
         (:div
          :class "d-inline-flex flex-row align-items-center"
          :style "position: relative; width: 100%; top: 2px;"
          (%logo)
          (:span
           :class "flex-grow-1"
           (if name
               (htm (:a :href "/user/" (str name)))
               (str "Not Signed In")))
          (:span (funcall links))))
        (:div :class "col-sm-2 wf-sidebar-width" " "))))))

(defun tw-account-bar ()
  (let* ((info (warflagger-user-info-bundle))
         (name (when (webhax-user:signed-up?)
                 (get-apparent-display-name (get-user-name))))
         (links
           (if (webhax-user:signed-up?)
               (lambda ()
                 (html-out
                   (:a :href (assoc-cdr :settings-url info) "Settings")
                   (:a :href (assoc-cdr :logout-url info) "Sign&nbsp;out")))
               (lambda ()
                 (html-out
                   (:a :onclick
                       (login-link-js (assoc-cdr :login-url info))
                       :href "#" "Sign&nbsp;Up")
                   (:a :onclick
                       (login-link-js (assoc-cdr :login-url info))
                       :href "#" "Log&nbsp;In"))))))
    (html-out
      (:div
       :class (lisp (gadgets:strcat
                     "flex flex-col sm:flex-row sm:h-9"
                     " text-white bg-black lineHeight-none w-full"
                     " text-sm m-0 p-0 border-0 items-center"
                     " heir-a:text-color-white heir-a:ml-4 heir-a:font-bold"))
       (:div :class "basis-2 sm:basis-44")
       (:div
        :class "grow"
        (:div
         :class "inline-flex sm:flex-row flex-col items-center relative w-full top-2px"
         (%logo)
         (:span
          :class "grow"
          (if name
              (htm (:a :href "/user/" (str name)))
              (str "Not Signed In")))
         (:span (funcall links))))
       (:div :class "basis-2 sm:basis-44")))))

(defun user-home-page ()
  (check-signed-up)
  (let ((since (userfig:userfig-value 'signed-up))
        (name (get-apparent-display-name (get-user-name))))
    ;;Don't use: wrong name
    ;;(userfig:userfig-value 'screen-name)
    (html-out
      (:h3
       (format *webhax-output* "User: ~a" name))
      (:h4 (format
            *webhax-output* "Member since ~a ~a"
            (elt local-time:+month-names+ (local-time:timestamp-month since))
            (local-time:timestamp-year since)))
      (when-let ((id (get-local-user-id (get-user-name))))
        (htm (:div (:a :href (make-author-url id) "View public page"))))
      (:br)))
  (let ((*thing-sidebox-length* 20)
        (*thing-sidebox-width* 40)
        (uid (get-local-user-id (get-user-name))))
    (thing-lister:display-things-sidebar
     #'user-recently-viewed
     (list (get-user-name))
     (lambda (row)
       (destructuring-bind (key type) row
         (case type
           (target (display-target-line key))
           (opinion (display-opinion-line key)))))
     ;;FIXME: For now:
     nil
     :label "Recently Viewed:"
     :class "featurebox")
    (thing-lister:display-thing-block-in-sidebar
     (tag-as-opinion #'author-opinions)
     (list (get-local-user-id (get-user-name)))
     #'mount-react-thing
     (format nil "/author-opinions/~a" uid)
     :trim thing-lister:*thing-summary-width*
     :label "Your Opinions:"
     :class "featurebox")
    (thing-lister:display-thing-block-in-sidebar
     (tag-as-opinion #'%author-replies)
     (list (get-local-user-id (get-user-name)))
     #'mount-react-thing
     (format nil "/author-replies/~a" uid)
     :trim thing-lister:*thing-summary-width*
     :label "Replies to your Posts:"
     :class "featurebox")))

(define-parts author-page-parts
  :@side-content
  (lambda ()
    (bind-validated-input
        ((author :string))
      (let ((authid (find-author-id author)))
        (author-discussions-sidebar authid)
        (author-opinions-sidebar authid))))
  :@inner
  (lambda ()
    (bind-validated-input
        ((author :string))
      (let* ((authid (find-author-id author))
             (auth-data (get-author-data authid))
             (user (get-local-user-from-id authid))
             (screen-name (author-representation-from-row auth-data))
                      ;;(if user
                          ;;    (userfig:userfig-value-for user 'screen-name)
                          ;;    (author-representation-from-row auth-data)))
             (num (get-count
                   (unexecuted (clsql:select
                                (colm 'author)
                                :from (tabl 'opinion)
                                :where (clsql:sql-= (colm 'author) authid)))))
             (since (when user
                      (userfig:userfig-value-for user 'signed-up))))
        (html-out
          (:h3 (format *webhax-output*
                       "User: ~a" screen-name))
          (when user
            (htm
             (:h4 (format
                   *webhax-output*
                   "WarFlagger member since ~a ~a"
                   (elt local-time:+month-names+
                        (local-time:timestamp-month since))
                   (local-time:timestamp-year since)))))
          (:h4 (format *webhax-output* "Opinions Posted: ~a" num)))
        (thing-lister:display-thing-block-in-sidebar
         #'%author-references
         (list authid)
         #'mount-react-thing
         (format nil "/author-references/~a" authid)
         :trim thing-lister:*thing-summary-width*
         :label "Author: References Made"
         :class "featurebox")))))
