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

(defun user-home-page ()
  (check-signed-up)
  (let ((since (userfig:userfig-value 'signed-up)))
    (html-out
      (:h3
       (format *webhax-output* "User: ~a" (userfig:userfig-value 'screen-name)))
      (:h4 (format
            *webhax-output* "Member since ~a ~a"
            (elt local-time:+month-names+ (local-time:timestamp-month since))
            (local-time:timestamp-year since)))
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
    (thing-lister:display-things-sidebar
     #'author-opinions
     (list (get-local-user-id (get-user-name)))
     #'display-opinion-line
     (format nil "/author-opinions/~a" uid)
     :label "Your Opinions:"
     :class "featurebox")
    (thing-lister:display-things-sidebar
     #'author-replies
     (list (get-local-user-id (get-user-name)))
     #'display-opinion-line
     (format nil "/author-replies/~a" uid)
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
          (:h4 (format *webhax-output* "Opinions Posted: ~a" num)))))))
