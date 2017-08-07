(in-package #:wf/web)

(defun login-link-js (url)
  (if webhax:*should-login-return*
      (ps-inline
       (setf (@ window location href)
             (strcat
              (lisp (concatenate 'string url "?destination="))
              (@ window location href))))
      (ps-inline
       (setf (@ window location href) (lisp url)))))

(defun %logo ()
  (html-out
    (:img :src "/static/img/wf_logo_small.png" :alt "[WarFlagger]"
          :style "position: absolute; top: 2px;")))

(defun account-bar ()
  (let ((info (warflagger-user-info-bundle)))
    (html-out
      (if (webhax-user:signed-up?)
          (htm
           (:div
            (%logo)
            (:a :style "position:relative; left: 180px;"
                :href (assoc-cdr :user-url info)
                (str (let ((userfig:*userfig-user*
                            (gethash :username webhax:*session*)))
                       (userfig:userfig-value 'webhax-user:screen-name))))
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
  (html-thing-lister:render-list-for-sidebar
   (list :lister-type :connector :thing 'user :name 'recently-viewed
         :lister-param (get-user-name))
   :label "Recently Viewed:"
   :class "featurebox" ;;??? This is a guess
   :summary-width 40
   :pagequantity 20)
  (when-let ((aid (get-local-user-id (get-user-name))))
    (html-thing-lister:render-list-for-sidebar
     (list :lister-type :connector :thing 'author :name 'opinion
           :lister-param aid)
     :label "Your Opinions:"
     :class "featurebox"
     :summary-width 40
     :pagequantity 20))
  (html-thing-lister:render-list-for-sidebar
   (list :lister-type :connector :thing 'user :name 'replies
         :lister-param (get-user-name))
   :label "Replies to Your Posts:"
   :class "featurebox"
   :summary-width 40
   :pagequantity 20))
