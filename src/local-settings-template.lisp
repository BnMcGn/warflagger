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
   #:*db-connect-type*
   #:*db-connect-spec*
   #:*targinfo-path*
   #:*production*))

(in-package :wf/local-settings)

(defparameter *base-url* "")
(defparameter *cache-path* "")
(defparameter *db-connect-type* nil)
(defparameter *db-connect-spec* nil)
(defparameter *targinfo-path* "")

(defparameter *production* nil)
