(in-package :wf/web)

(define-parts target-components
  (add-part
   :@javascript
   (lambda ()
     (ps
      (define-react-class
          hilited-text
          (psx
           (:div (@ this props text))))))))

