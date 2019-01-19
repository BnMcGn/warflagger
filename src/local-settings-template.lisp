;;;; local-settings.lisp
; 
; Local settings that should not be included in the public source distribution
; go in this file.
;
; local-settings-template.lisp:
;
; A blank version of local-settings for public distribution. Copy to 
; local-settings.lisp and fill out before running warflagger.
;
;;;;

(defpackage :wf/local-settings
  (:use #:cl)
  (:export
   #:*base-url*
   #:*cache-path*
   #:*static-path*
   #:*text-extractor-script*
   #:*db-connect-type*
   #:*db-connect-spec*
   #:*test-db-connect-spec*
   #:*warstats-path*
   #:*production*))

(in-package :wf/local-settings)

(defparameter *base-url* "")
(defparameter *cache-path* "")
(defparameter *static-path* "quicklisp/local-projects/wf-static/")
(defparameter *text-extractor-script*
  (asdf:system-relative-pathname 'warflagger "src/text-extract/textract.py"))
(defparameter *db-connect-type* nil)
(defparameter *db-connect-spec* nil)
(defparameter *test-db-connect-spec* nil)
(defparameter *warstats-path* "")

(defparameter *production* nil)

(pushnew #p"" webhax:*named-text-locations*)
