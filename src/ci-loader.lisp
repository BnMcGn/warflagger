(load "/tmp/quicklisp.lisp")
(quicklisp-quickstart:install)

(ql:register-local-projects)

(ql:quickload 'test-warflagger)
(if (test-warflagger:test-warflagger)
    (uiop:quit 0)
    (uiop:quit 1))
