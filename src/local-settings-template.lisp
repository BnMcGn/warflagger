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
   #:*session-db-connect-spec*
   #:*test-db-connect-spec*
   #:*warstats-path*
   #:*production*
   #:*auto-run*
   #:*test-user-name*
   #:*supply-file*
   #:*supply-text*
   #:*ssl-key-file*
   #:*ssl-cert-file*
   #:*ssl-password*
   #:*screeenly-api-key*))

(in-package :wf/local-settings)

(defparameter *base-url* "")
(defparameter *cache-path* "")
(defvar *static-path*)
(defparameter *text-extractor-script*
  (asdf:system-relative-pathname 'warflagger "src/text-extract/textract.py"))
(defparameter *db-connect-type* nil)
(defparameter *db-connect-spec* nil)
(defparameter *session-db-connect-spec* nil)
(defparameter *test-db-connect-spec* nil)
(defparameter *warstats-path* "")

(defparameter *production* nil)
(defparameter *auto-run* nil)
(defparameter *test-user-name* nil)

(defparameter *supply-file* "")
(defparameter *supply-text* "")

(defparameter *ssl-key-file* nil)
(defparameter *ssl-cert-file* nil)
(defparameter *ssl-password* nil)

(defparameter *screeenly-api-key* "")

(pushnew (asdf:system-relative-pathname 'warflagger "src/texts/ ")
         webhax:*named-text-locations* :test #'equal)
