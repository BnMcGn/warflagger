
(in-package #:test-warflagger)

(def-suite test-auth
  :description "Tests for login/logout/sign-up"
  :in wf-tests)

(in-suite test-auth)

;;FIXME: Much of this testing should be done under webhax or somewhere like that, but the tests won't
;; be run there at the moment. Here is good enough for now, but consider transferring at some point.

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
                     (let ((ningle:*context* (hu:hash (:session (hu:hash (:clath-destination "x"))))))
                       (is (equal "/sign-up/" (clath::destination-on-login)))
                       (is (equal *destination* (webhax-user:login-destination)))
                       (let ((userfig:*new-user-p* (lambda (_) nil)))
                         (is (equal "x" (clath::destination-on-login))))))))
             (res (funcall app *test-env-1*))
             (cookie (getf (second res) :set-cookie))
             (cookie (subseq cookie (search "lack." cookie) (search ";" cookie))))
        (is (not-empty cookie))
        (funcall app (test-env-2 cookie))))

(test sign-up-page "Sign up page functionality"
  ;;Shim (check-authenticated)
  (let ((webhax:*session* (hu:hash (:username "fake")))
        (save-called nil))
    ;;Shim save-signed-up-user
    (proto:shadow-func webhax-user::save-signed-up-user
        (lambda (_) (declare (ignore _)) (setf save-called t))
      (let ((page (car (third (webhax-user:sign-up-page)))))
        (is-false (search "text-red-700" page))
        (is-true (search "Create" page)))
      (let* ((webhax:*key-web-input* (list (cons :screen-name "Mr. X")  (cons :email "X")))
             (page (car (third (webhax-user:sign-up-page)))))
        (is-true (search "text-red-700" page))
        (is-true (search "Create" page)))
      ;(is (print (webhax-user:sign-up-page)))
      (let* ((webhax:*key-web-input* (list (cons :screen-name "Mr. X")
                                           (cons :email "x@warflagger.net")))
             (res (webhax-user:sign-up-page)))
        (is-true save-called)
        (is (eq 302 (car res)))))))

