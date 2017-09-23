;;;
;;; Script to build a lisp server image for warflagger.net
;;;

(ql:quickload 'wf-web :silent nil)

(defun main ()
  (wf-web:run-server))

(sb-ext:save-lisp-and-die
 "warflagger-server.fcgi" :toplevel #'main :executable t)
