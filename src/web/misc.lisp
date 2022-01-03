(in-package :wf/web)

(defvar *app* (make-instance 'ningle:<app>))

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
       (grouped-page)
    ))


(defun tracking-code ()
 "<!-- Start of StatCounter Code for Default Guide -->
<script type=\"text/javascript\">
var sc_project=11547060;
var sc_invisible=1;
var sc_security=\"13ae87ef\";
var scJsHost = ((\"https:\" == document.location.protocol) ?
\"https://secure.\" : \"http://www.\");
document.write(\"<sc\"+\"ript type='text/javascript' src='\" +
scJsHost+
\"statcounter.com/counter/counter.js'></\"+\"script>\");
</script>
<noscript><div class=\"statcounter\"><a title=\"Web Analytics\"
href=\"http://statcounter.com/\" target=\"_blank\"><img
class=\"statcounter\"
src=\"//c.statcounter.com/11547060/0/13ae87ef/1/\" alt=\"Web
Analytics\"></a></div></noscript>
<!-- End of StatCounter Code for Default Guide -->")

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

(defun handle-response-shim (res)
  (terminate-thread-on-broken-pipe
    (%handle-response-copy res)))

;;FIXME: We are getting hung threads because of broken-pipe conditions on the server
;; This should allow us to terminate them gracefully, for now. But it would be better to
;; find out why clack is not catching these errors by default.
(unless (fboundp '%handle-response-copy)
  (def-as-func %handle-response-copy #'clack.handler.hunchentoot::handle-response)
  (def-as-func clack.handler.hunchentoot::handle-response #'handle-response-shim))


(defun clsql-middleware (dbtype connspec)
  (lambda (app)
    (lambda (env)
      (let* ((conn (clsql:connect connspec :database-type dbtype :if-exists :new :make-default nil))
             (clsql:*default-database* conn))
        (unwind-protect
             (funcall app env)
          (clsql:disconnect :database conn))))))
