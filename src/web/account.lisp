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
            (:a :onclick (lisp (login-link-js (assoc-cdr :login-url info)))
                :href "#" "Log In")
            (:a :onclick (lisp (login-link-js (assoc-cdr :login-url info)))
                :href "#" "Sign Up")))))))



