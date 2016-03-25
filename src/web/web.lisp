(in-package :wf/web)

(defvar *handler*)
(defvar *session-store*)

(define-default-layout (warflagger-main :wrapper #'webhax:page-base)
  (:prepend-parts
   (add-part :@css "/static/css/style.css"))
  (html-out
                                        ;;Header
    (:div :id "header_wrapper"
          (:div :id "account_bar"
                :@notifications :@account-info))
                                        ;;Main content
    (:div :id "left_side"
          :@site-search :@site-index :@side-content)
    (:div :id "content"
          :@messages :@main-content :@footnotes)
                                        ;;Footer
    (:div :id "footer" :@copyright)))

(define-default-parts warflagger-base
  (add-part :@css "/static/css/style.css")
  (add-part :@account-info "here")
  (add-part :@javascript #'ps-gadgets)
  (add-part :@javascript "/static/javascript/jquery/1.9.1/jquery.js"))

(def-thing
    'user
    (compose #'get-author-data #'get-local-user-id)
  #'get-author-representation
  :lister (list
           #'user-lister
           :sortkeys '(values id)
           :length (lambda (&rest params)
                     (get-count
                      (unexecuted
                        (apply #'user-lister params))))))

(def-db-thing
    'opinion
  'opinion
  #'print
  :keyfunc (lambda (id)
             (opinion-from-db-row (get-assoc-by-pkey 'opinion id)))
  :sortkeys '(target author datestamp excerpt rooturl))

(clsql:connect wf/text-extract::*db-connect-spec*
               :database-type :postgresql-socket3)

(wf/text-extract:initialize-indices)

(defvar *app* (make-instance 'ningle:<app>))

(setf (ningle:route *app* "/demo/")
      (quick-page "test"))


(setf (ningle:route *app* "/demo2/")
      (quick-page #'webhax::react
                  (with-output-to-string (*webhax-output*)
                    (html-out
                      (:div :id "things")
                      (:script
                       :type "text/javascript"
                       (str (test-js)))))))

(setf (ningle:route *app* "/target/*")
      (quick-page #'webhax::react #'target-components #'mood-lib
                  (lambda ()
                    (bind-validated-input
                        ((id (webhax-validate:ratify-wrapper :integer)))
                      (let ((url (get-rooturl-by-id id)))
                        (html-out
                          (:div :id "test")
                          (:script
                           :type "text/javascript"
                           (str
                            (ps
                              (var data
                                   (lisp-raw
                                    (target-data id)))
                              (var target-url (lisp url))
                              (var target-title (lisp (grab-title url)))
                              (render
                               (create-element target-root
                                            (create :text (@ data text)
                                                    :opinions
                                                    (@ data opinions)
                                                    :focus '(20)
                                                    :url target-url
                                                    :title target-title))
                            (chain document
                                   (get-element-by-id "test"))))))))))))




;;;Code below starts server. To restart, first stop server thusly:
;;;(clack:stop wf/web::*handler*)
;;;Then evaluate code below.

(setf *handler*
      (clack:clackup
       (clack-pretend:pretend-builder (:insert 2) ;clack.builder:builder
        (clack.middleware.clsql:<clack-middleware-clsql>
         :database-type :postgresql-socket3
         :connection-spec *db-connect-spec*)
        (clack.middleware.static:<clack-middleware-static>
         :path "/static/"
         :root #p"~/quicklisp/local-projects/warflagger/src/static/")
        :session
        ;(setf *session-store*
         ;     (make-instance
          ;     'clack.middleware.session:<clack-middleware-session>
           ;    :state
            ;   (make-instance
             ;   'clack.session.state.cookie:<clack-session-state-cookie>)))
        (:mount
         "/oid_connect"
         (clack-openid-connect:app
          "http://logintest.warflagger.com:5000/oid_connect/"))
        ;(clack-pretend::clack-middleware-pretend)
        ;(clack.middleware.openid:<clack-middleware-openid>)
        ;(json-call :login-p nil)
        *app*)
       :port 5000))
