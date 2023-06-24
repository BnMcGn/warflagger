;;;; warflagger.lisp

(in-package #:warflagger-core)

(setf (readtable-case *readtable*) :upcase)

(defun known-flags ()
  (cl-utilities:collecting
    (loop for cat in *flag-category-keys*
          for flags in *flag-types-source*
          do (gadgets:do-window (pair flags :size 2 :step 2)
               (cl-utilities:collect (list cat (car pair)))))))

