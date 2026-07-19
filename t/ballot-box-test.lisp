(in-package #:test-warflagger)

(def-suite test-ballot-box
  :description "Tests of the score-script ballot-box system"
  :in wf-tests)

(in-suite test-ballot-box)

(defun init-test-ballot-box ()
  )

;;FIXME; Assumes that value for unknown author is 1. Not future proof, should jimmy that...
(test ballot-box-basics "Basic ballot box tests"
      (is (warflagger::ballot-box-empty-p (warflagger:make-ballot-box)))
      (let ((bbox (warflagger:make-ballot-box)))
        (cast-vote! bbox :up (tid 0) "qwert")
        (is (warflagger::ballot-box-vast-majority-p bbox))
        (multiple-value-bind (effect balance score)
            (warflagger::ballot-box-controversy
             (warflagger:merge-with-inverted-ballot-boxes bbox bbox) nil nil)
          (is (= 0 score))
          (is (= 1 balance))
          (is (= 0 effect)))))

(test
    ballot-box-totals "Tests for ballot-box-totals"
    (let ((bbox (warflagger:make-ballot-box)))
      (cast-vote! bbox :up (tid 0) "asdf")
      (cast-vote! bbox :up (tid 1) "asdf")
      (cast-vote! bbox :up (tid 2) "asdf")
      (is (= 1 (nth-value 1 (warflagger:ballot-box-totals bbox))))
      (cast-vote! bbox :right (tid 3) "asdf" :reference "x")
      (cast-vote! bbox :right (tid 4) "asdf" :reference "y")
      ;; :up gets cancelled by :right
      (is (= 0 (nth-value 1 (warflagger:ballot-box-totals bbox))))
      ;;FIXME: default :right value is 2. Not long term solution
      (is (= 4 (nth-value 0 (warflagger:ballot-box-totals bbox))))))

