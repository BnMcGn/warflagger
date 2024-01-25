(in-package :wf/web)

(defvar *app* (make-instance 'ningle:<app>))

(defun large-logo ()
  (html-out
    (:img :src "/static/img/wf_logo_large.png"
          :alt "[WarFlagger: Because someone is wrong on the internet]"
          :style "display: block; margin-left: auto; margin-right: auto; margin-top: 3em; margin-bottom: 3em;")))

(defparameter *input-tw-classes* "block flex-none py-1 px-3 m-0 min-w-0 h-8 min-h-0 text-sm bg-white bg-none rounded border border-solid cursor-text border-stone-300 text-neutral-600 focus:border-blue-400 leading-snug shadow-sm transition-[border-color_0.15s_ease-in-out_0s] transition-[box-shadow-0.15s-ease-in-out_0s] pr-3.5")

(defun %sign-up-form (screen-name email screen-name-error email-error)
  (html-out
   (:div
    (:h2 "New Account")
    (:p "Please confirm a few details to create your account.")
    (:div :class "border-solid border-8 rounded-lg m-2 p-2 border-blue-200 inline-block"
     (:form
      :class "flex flex-col gap-4 m-4"
      :action "/sign-up/" :method "POST"
      (:div
       (:label :for "screen-name" "Screen Name:")
       (:input :class *input-tw-classes*
               :type "text" :id "screen-name" :name "screen-name" :value screen-name)
       (when screen-name-error
         (htm (:div :class "text-red-700" screen-name-error))))
      (:div
       (:label :for "email" "Email:")
       (:input :class *input-tw-classes*
               :type "text" :id "email" :name "email":value email)
       (when email-error
         (htm (:div :class "text-red-700" email-error))))
      (:input :type "submit" :value "Create"
              :class "py-1 px-3 text-center whitespace-nowrap align-center self-end text-sm font-normal whitespace-nowrap bg-none rounded border border-solid cursor-pointer select-none leading-snug inline-block rc-button w-32"))))))

(defun webhax-user:sign-up-page ()
  (funcall
   (cljs-page ()
     (bind-tested-input (&key (page-test-enabled :boolean)
                         (screen-name (:unique :options-func 'webhax-user:list-of-screen-names))
                         (email :email))
       (unless page-test-enabled (check-authenticated))
       (if (and screen-name (not *bvi-errors*))
           (progn
             (unless page-test-enabled
               (save-signed-up-user (hu:hash (:screen-name screen-name :email email))))
             (html-out (:script (str (ps (setf (@ window location)
                                               (lisp (webhax-user:login-destination))))))))
           (if *bvi-errors*
               (%sign-up-form (or screen-name (assoc-cdr :screen-name *key-web-input*))
                              (or email (assoc-cdr :email *key-web-input*))
                              (assoc-cdr :screen-name *bvi-errors*)
                              (assoc-cdr :email *bvi-errors*))
               (%sign-up-form (get-openid-display-name) (login-provider-fields :email) nil nil)))))))

(define-parts main-page-parts
  ;;:@css-link "/static/css/push_button.css"
  :@notifications
  (let ((info (warflagger-user-info-bundle)))
    (lambda ()
      (unless (signed-up?)
        #|(html-out
          (:div :class "featurebox_side"
                (:h3 "Account")
                (:a :href (assoc-cdr :login-url info)
                    :class "push_button pb_green" "Sign Up")
                (:a :href (assoc-cdr :login-url info) "Log In")))
        |#)))
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
    (mount-cljs-component ("grouped"))))

;;Not the greatest thing to have on the live server, but we want to have the sign up code tested.
(defun cleanup-test-user ()
  (when wf/local-settings:*test-user-name*
    (when-let ((user (get-user-by-screenname wf/local-settings:*test-user-name*)))
      (userfig:remove-user user))))

(eval-always
  (defmacro terminate-thread-on-broken-pipe (&body body)
   "The live server eventually clogs with hunchentoot-worker threads. These seem to be wedged by
sb-int:broken-pipe conditions. This macro should fix the problem when wrapped around the startup code."
   ;;Because we might not be on sbcl
   (if-let ((sym (and (find-package 'sb-int)
                      (find-symbol "BROKEN-PIPE" (find-package 'sb-int)))))
     `(handler-case
          (progn ,@body)
        ;;Terminate the thread
        (,sym () (invoke-restart 'cl-user::abort)))
     `(progn ,@body))))

;;Seems to be a problem between firefox and SSL hunchentoot, dumping a variety of errors into
;; the interactive environment. Are they of consequence? Probably not, so we are going to ignore.
(eval-always
 (defmacro terminate-thread-on-a-few-different-things (&body body)
   "The live server eventually clogs with hunchentoot-worker threads. These seem to be wedged by
sb-int:broken-pipe conditions. This macro should fix the problem when wrapped around the startup code."

   `(terminate-thread-on-broken-pipe
     (handler-case
         (progn ,@body)
       (cl+ssl::ssl-error-syscall () (invoke-restart 'cl-user::abort))))))

(defun handle-response-shim (res)
  (terminate-thread-on-a-few-different-things
    (%handle-response-copy res)))

;;FIXME: We are getting hung threads because of broken-pipe conditions on the server
;; This should allow us to terminate them gracefully, for now. But it would be better to
;; find out why clack is not catching these errors by default.
(defun shim-handle-response ()
    (unless (fboundp '%handle-response-copy)
      (def-as-func %handle-response-copy
          (symbol-function (symbolize 'handle-response :package 'clack.handler.hunchentoot)))
      (def-as-func (symbolize 'handle-response :package 'clack.handler.hunchentoot)
        #'handle-response-shim)))

(defun clsql-middleware (dbtype connspec)
  (lambda (app)
    (lambda (env)
      (let* ((conn (clsql:connect connspec :database-type dbtype :if-exists :new :make-default nil))
             (clsql:*default-database* conn))
        (unwind-protect
             (funcall app env)
          (clsql:disconnect :database conn))))))
