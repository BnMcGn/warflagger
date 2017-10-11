;;;
;;; Script to build a lisp server image for warflagger.net
;;;

;(pushnew (truename "/home/ben/quicklisp/local-projects/")
;         ql:*local-project-directories*)

;(ql:quickload 'wf-web :silent nil)
;(ql:quickload 'swank)
(require 'wf-web)
(require 'swank)

(defun main ()
  ;;(setf swank:*use-dedicated-output-stream* nil)
;  (setf asdf:*user-cache*
;        (gadgets:extend-pathname (asdf/cl:user-homedir-pathname)
;                                 ".cache" "common-lisp"
;                                 (asdf:implementation-identifier)))
  (swank:create-server :port 4004 :dont-close t)
  (wf/web::run-server))

(sb-ext:save-lisp-and-die
 "wf-server.img" :toplevel #'main :executable t)
