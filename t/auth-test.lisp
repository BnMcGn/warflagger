
(in-package #:test-warflagger)

(def-suite test-auth
  :description "Tests for login/logout/sign-up"
  :in wf-tests)

(in-suite test-auth)

(defparameter *test-env-1*
  (list
   :REQUEST-METHOD :GET :SCRIPT-NAME "" :PATH-INFO "/clath/login/"
   :SERVER-NAME "127.0.0.1" :SERVER-PROTOCOL :HTTP/1.0
   :REQUEST-URI
   "/clath/login/?destination=https://127.0.0.1/o/bafkreiab3ysddpms3i4hzfx2oyr42ysm6fmpdd2zm3qug73n4g3tz5zsge"
   :URL-SCHEME "http" :REMOTE-ADDR "127.0.0.1" :REMOTE-PORT 46878
   :QUERY-STRING
   "destination=https://127.0.0.1/o/bafkreiab3ysddpms3i4hzfx2oyr42ysm6fmpdd2zm3qug73n4g3tz5zsge"
   :CONTENT-LENGTH NIL :CONTENT-TYPE NIL :CLACK.STREAMING T
   :HEADERS
   (CL-HASH-UTIL:ALIST->HASH
    '(("host" . "127.0.0.1") ("connection" . "close")
      ("user-agent"
       . "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/113.0")
      ("accept"
       . "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8")
      ("accept-language" . "en-CA,en-US;q=0.7,en;q=0.3")
      ("accept-encoding" . "gzip, deflate, br")
      ("referer"
       . "https://127.0.0.1/o/bafkreiab3ysddpms3i4hzfx2oyr42ysm6fmpdd2zm3qug73n4g3tz5zsge")
      ;("cookie" . "lack.session=754416306758666dab95ca84df81349d088c84c5")
      ("upgrade-insecure-requests" . "1") ("sec-fetch-dest" . "document")
      ("sec-fetch-mode" . "navigate") ("sec-fetch-site" . "same-origin")
      ("sec-fetch-user" . "?1"))
    :EXISTING (MAKE-HASH-TABLE :TEST #'EQUAL))))

(defun test-env-2 (cookie)
  (list
   :REQUEST-METHOD :GET :SCRIPT-NAME "" :PATH-INFO "/"
   :SERVER-NAME "127.0.0.1" :SERVER-PROTOCOL :HTTP/1.0
   :REQUEST-URI "/"
   :URL-SCHEME "http" :REMOTE-ADDR "127.0.0.1" :REMOTE-PORT 46878
   :QUERY-STRING ""
   :CONTENT-LENGTH NIL :CONTENT-TYPE NIL :CLACK.STREAMING T
   :HEADERS
   (CL-HASH-UTIL:ALIST->HASH
    `(("host" . "127.0.0.1") ("connection" . "close")
      ("user-agent"
       . "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/113.0")
      ("accept"
       . "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8")
      ("accept-language" . "en-CA,en-US;q=0.7,en;q=0.3")
      ("accept-encoding" . "gzip, deflate, br")
      ("cookie" . ,cookie)
      ("upgrade-insecure-requests" . "1") ("sec-fetch-dest" . "document")
      ("sec-fetch-mode" . "navigate") ("sec-fetch-site" . "same-origin")
      ("sec-fetch-user" . "?1"))
    :EXISTING (MAKE-HASH-TABLE :TEST #'EQUAL))))

(defun test-app-1 (core)
  (lack.builder:builder
   :session
   (clath:component "http://127.0.0.1/")
   (webhax-user:webhax-user :userfig-specs wf/web::*userfig-fieldspecs*)
   core))

(defparameter *destination*
  "https://127.0.0.1/o/bafkreiab3ysddpms3i4hzfx2oyr42ysm6fmpdd2zm3qug73n4g3tz5zsge")

(test login-page "Login page functionality"
      (let* ((app (test-app-1
                   (lambda (env)
                     (is (equal *destination*
                                (gethash :clath-destination (webhax:session-from-env env))))
                     (let ((ningle:*context* (hu:hash (:session (hu:hash)))))
                       (is (equal "/sign-up/" (clath::destination-on-login)))
                       (let ((userfig:*new-user-p* (lambda (_) nil)))
                         (is (equal *destination* (clath::destination-on-login))))))))
             (res (funcall app *test-env-1*))
             (cookie (getf (second res) :set-cookie))
             (cookie (subseq cookie (search "lack." cookie) (search ";" cookie))))
        (is (not-empty cookie))
        (funcall app (test-env-2 cookie))))

