(in-package #:wf/web)

(defun login-link-js (url)
  (ps-inline
    (setf (@ window location href)
          (strcat
           (lisp (concatenate 'string url "?destination="))
           (@ window location href)))))

(defun account-bar ()
  (let ((info (warflagger-user-info-bundle)))
    (html-out
      (if (webhax-user:signed-up?)
          (htm
           (:div
            (:span "WarFlagger")
            (:a :href (assoc-cdr :user-url info)
                (userfig:userfig-value :screen-name))
            (:div :style "float: right; margin-right: 180px;"
                  (:a :href (assoc-cdr :settings-url info) "Settings")
                  (:a :href (assoc-cdr :logout-url info) "Sign out"))))
          (htm
           (:div
            (:span "WarFlagger")
            (:span "Not Signed In")
            (:div :style "float: right; margin-right: 180px;"
                  (:a :onclick (lisp (login-link-js (assoc-cdr :login-url info)))
                   :href "#" "Sign Up")
                  (:a :onclick (lisp (login-link-js (assoc-cdr :login-url info)))
                   :href "#" "Log In"))))))))


