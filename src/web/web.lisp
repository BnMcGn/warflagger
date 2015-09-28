(in-package :wf/web)

(defvar *handler*)
(defvar *session-store*)

(define-default-layout (warflagger-main :wrapper #'webhax:page-base)
  (:prepend-parts
   (add-part :@css "/static/css/style.css"))
  (html-out
                                        ;Header
    (:div :id "header_wrapper"
          (:div :id "account_bar"
                :@notifications :@account-info))
                                        ;Main content
    (:div :id "left_side"
          :@site-search :@site-index :@side-content)
    (:div :id "content"
          :@messages :@main-content :@footnotes)
                                        ;Footer
    (:div :id "footer" :@copyright)))

(define-default-parts warflagger-base
  (add-part :@css "/static/css/style.css")
  (add-part :@account-info "here"))

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

(defvar *app* (make-instance 'ningle:<app>))

(setf (ningle:route *app* "/demo/")
      (quick-page "test"))

(setf (ningle:route *app* "/demo2/")
      (quick-page "x"))

                                        ;Code below starts server. To restart, first stop server thusly:
                                        ;(clack:stop wf/web::*handler*)
                                        ;Then evaluate code below.

(setf *handler*
      (clack:clackup
       (clack.builder:builder
        (clack.middleware.clsql:<clack-middleware-clsql>
         :database-type :postgresql
         :connection-spec *db-connect-spec*)
        (clack.middleware.static:<clack-middleware-static>
         :path "/static/"
         :root #p"~/quicklisp/local-projects/warflagger/src/static/")
        (setf *session-store*
              (make-instance
               'clack.middleware.session:<clack-middleware-session>
               :state
               (make-instance
                'clack.session.state.cookie:<clack-session-state-cookie>)))
        (clack-pretend::clack-middleware-pretend)
        (clack.middleware.openid:<clack-middleware-openid>)
        *app*)
       :port 5000))
