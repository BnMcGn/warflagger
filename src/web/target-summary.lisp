(in-package :wf/web)

;; Summary tab for root target page

(defun target-summary ()
  (ps

    (def-component target-root-summary
        (psx
         (:div
          (:div :key 1
                "Title:" (prop title)))))


    ))
