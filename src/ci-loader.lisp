(load "~/quicklisp/setup.lisp")

(ql:register-local-projects)
(ql:update-all-dists)

(ql:quickload 'test-warflagger)
(if (test-warflagger:test-warflagger)
    (uiop:quit 0)
    (uiop:quit 1))
