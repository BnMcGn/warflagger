(ql:quickload 'clack :silent t)
;(ql:quickload 'lack-middleware-backtrace :silent t)
;(ql:quickload 'clack-handler-fcgi :silent t)

(defun main ()
  (swank:create-server :port 4004 :dont-close t)
  (clack:clackup
   (lambda (env)
     (declare (ignore env))
     '(200
       (:content-type "text/plain")
       ("Hello, Clack!")))
                                        ;:server :fcgi
                                        ;:use-thread nil
                                        ;:fd 0
   :port 5005)
  (sb-thread:join-thread
   (find-if
    (lambda (th)
      (string= (sb-thread:thread-name th) "clack-handler-hunchentoot"))
    (sb-thread:list-all-threads))))

(sb-ext:save-lisp-and-die "test.server" :toplevel #'main :executable t)
