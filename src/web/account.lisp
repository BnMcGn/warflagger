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
        (:img :src "/static/img/wf_logo_small.png" :alt "[WarFlagger]"
              :style "position: absolute; top: 2px;"))))

(defun get-apparent-display-name (loginid)
  (if-let ((id (get-local-user-id loginid)))
    (author-representation-from-row (get-author-data id))
    (userfig:userfig-value-for loginid 'screen-name)))

(defun account-bar ()
  (let ((info (warflagger-user-info-bundle)))
    (html-out
      (if (webhax-user:signed-up?)
          (htm
           (:div
            (%logo)
            (:a :style "position:relative; left: 180px;"
                ;;FIXME: Should be link at /user/ to :user-url
                :href "/user/" ;(assoc-cdr :user-url info)
                (str (get-apparent-display-name (get-user-name))))
            (:div :style "float: right; margin-right: 30px;"
                  (:a :href (assoc-cdr :settings-url info) "Settings")
                  (:a :href (assoc-cdr :logout-url info) "Sign out"))))
          (htm
           (:div
            (%logo)
            (:span :style "position:relative; left: 180px;" "Not Signed In")
            (:div :style "float: right; margin-right: 30px;"
                  (:a :onclick
                      (login-link-js (assoc-cdr :login-url info))
                   :href "#" "Sign Up")
                  (:a :onclick
                      (login-link-js (assoc-cdr :login-url info))
                      :href "#" "Log In"))))))))

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
