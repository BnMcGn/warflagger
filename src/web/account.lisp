(in-package #:wf/web)

(defun account-bar ()
  (let ((info (warflagger-user-info-bundle)))
    (html-out
      (if (webhax-user:signed-up?)
          (htm
           (:span
            (:span "WarFlagger")
            (:a :href (assoc-cdr :user-url info)
                (userfig:userfig-value :screen-name))
            (:a :href (assoc-cdr :settings-url info) "Settings")
            (:a :href (assoc-cdr :logout-url info) "Sign out")))
          (htm
           (:span
            (:span "WarFlagger")
            (:span "Not Signed In")
            (:a :href (assoc-cdr :login-url info) "Log In")
            (:a :href (assoc-cdr :login-url info) "Sign Up")))))))
