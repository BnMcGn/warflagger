;;;
;;; Script to build a lisp server image for warflagger.net
;;;

(ql:register-local-projects)
(ql:quickload 'wf-web :silent nil)
(ql:quickload 'swank)
;;(ql:quickload 'trivial-features)

(defun main ()
  (swank:create-server :port 4004 :dont-close t)
  (wf/web::run-production-server)
  (clerk:start))

(sb-ext:save-lisp-and-die
 "wf-server.new" :toplevel #'main :executable t)
