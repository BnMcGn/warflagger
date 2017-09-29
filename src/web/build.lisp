;;;
;;; Script to build a lisp server image for warflagger.net
;;;

(ql:quickload 'wf-web :silent nil)
(ql:quickload 'swank)

(defun main ()
  (setf swank:*use-dedicated-output-stream* nil)
  (swank:create-server :port 4004 :dont-close t)
  (wf/web::run-server))

(sb-ext:save-lisp-and-die
 "warflagger-server.fcgi" :toplevel #'main :executable t)
