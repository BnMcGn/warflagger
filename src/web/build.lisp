;;;
;;; Script to build a lisp server image for warflagger.net
;;;

(ql:quickload 'wf-web :silent nil
(ql:quickload 'swank)

(defun main ()
  ;;(setf swank:*use-dedicated-output-stream* nil)
  (setf asdf:*user-cache*
        (gadgets:extend-pathname (asdf/cl:user-homedir-pathname)
                                 ".cache" "common-lisp"
                                 (asdf:implementation-identifier)))
  (swank:create-server :port 4004 :dont-close t)
  (wf/web::run-server))

(sb-ext:save-lisp-and-die
 "wf-server.img" :toplevel #'main :executable t)
