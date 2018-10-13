(in-package :wf/web)

(defparameter *grandchild-shift* 15)

(defun opinion-page ()
  (ps

    (def-component opinion-page
        (psx
         (:target-root-article
          :... (@ this props))))))

