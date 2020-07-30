(in-package :wf/web)

;;FIXME: think of a better name
(define-ps-lib radmin ()
  (ps

    (setf radmin (@ (require "react-admin") -admin))
    (setf json-server-provider (@ (require "ra-data-json-server") default))

    (def-component admin-page
        (psx
         (:radmin
          :data-provider (json-server-provider (lisp wf/local-settings:*base-url*)))))

    ))
