(load "~/quicklisp/setup.lisp")

(ql:quickload 'swank)

(swank:create-server :port 4004 :dont-close t)
