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
  (add-part :@account-info #'account-bar)
  (add-part :@javascript #'ps-gadgets)
  (add-part :@javascript "/static/javascript/jquery/1.9.1/jquery.js"))

(def-thing
    'user
    (compose #'get-author-data #'get-local-user-id)
  #'author-representation-from-row
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
  (lambda (x)
    (with-output-to-string (s)
      (print x s)))
  :keyfunc (lambda (id)
             (opinion-from-db-row (get-assoc-by-pkey 'opinion id)))
  :sortkeys '(target author datestamp excerpt rooturl))

(def-thing
    'target
    (lambda (rootid)
      ;;FIXME: maybe should check that page is extracted/available
      (let ((url (get-rooturl-by-id rootid)))
        (list
         :id rootid
         :title (grab-title url)
         :text (grab-text url)
         :url url
         :warstats (warstats-for-target url))))
  (lambda (targdata)
    (concatenate 'string
                 (truncate-string (getf targdata :title) :length 30)
                 " - "
                 (truncate-string (getf targdata :text) :length 30)))
  :lister
  (list
   (wrap-with-paging-handler
    (lambda (&key order-by)
      (declare (ignore order-by))
      ;;Only have relevance for now
      (get-ranked-rootids)))
   :sortkeys '(relevance))
  :html-thing-link (lambda (id) (format nil "/target/~a" id)))

(clsql:connect wf/text-extract::*db-connect-spec*
               :database-type :postgresql-socket3)

(defun warflagger-user-info-bundle ()
  (cons
   (cons :user-url "")
   (webhax-user:user-info-bundle)))

(wf/text-extract:initialize-indices)

(defvar *app* (make-instance 'ningle:<app>))

(dependency-auto-watcher routes
  (setf (ningle:route *app* "/text-server/")
        (lambda (params)
          (list 200 '(:content-type "application/json")
                (list
                 (json:encode-json-to-string
                  (wf/text-extract:text-server
                   (cdr (assoc "url" params :test #'string=))))))))

  (setf (ningle:route *app* "/opinion/")
        (quick-page #'webhax::react #'webhax::redux #'opinion-components
                    #'opinion-form-page))

  (setf (ningle:route *app* "/target/*")
        (quick-page #'webhax::react #'target-components #'mood-lib
                    (lambda ()
                      (bind-validated-input
                          ((id :integer))
                        (let ((url (get-rooturl-by-id id)))
                          (multiple-value-bind (text opinions)
                              (target-data id)
                            (mount-component (target-root)
                              :text (lisp-raw text)
                              :opinions (lisp-raw opinions)
                              :url (lisp url)
                              :title (lisp (grab-title url))
                              :focus '(20))))))))

  ;;(setf (ningle:route *app* "/signup/") #'signup-page)

  (setf (ningle:route *app* "/demo/")
        (quick-page #'webhax::react #'webhax::webhax-ask
                    (add-part :@javascript #'webhax-widgets:ps-widgets)
                    #'demo-pages))

  ;;FIXME: Should be handled internally by webhax service middleware
  (setf (ningle:route *app* "/ask-data/*" :method :POST)
        (input-function-wrapper
         (lambda ()
           (bind-validated-input
               ((askid :overlength))
             (print (json:encode-json-alist-to-string
                     (webhax:call-ask-manager
                      askid :update webhax:*key-web-input*))
                    *webhax-output*)))
         :content-type "application/json"))

  (setf (ningle:route *app* "/")
        (lambda (params)
          (declare (ignore params))
          (clack-openid-connect::logged-in-page))))

;;;Code below starts server. To restart, first stop server thusly:
;;;(clack:stop wf/web::*handler*)
;;;Then evaluate code below.

(defparameter *userfig-fieldspecs*
  '(:test-value
    (:string
     :initial "a value")))

(clack-server-manager
 *handler*
 (clack-pretend:pretend-builder
  (:insert 4) ;clack.builder:builder
  (clack.middleware.clsql:<clack-middleware-clsql>
   :database-type :postgresql-socket3
   :connection-spec *db-connect-spec*)
  (clack.middleware.static:<clack-middleware-static>
   :path "/static/"
   :root #p"~/quicklisp/local-projects/warflagger/src/static/")
  :session
  (clack-openid-connect:component
   "http://logintest.warflagger.com:5000/oid_connect/")
  (webhax-user:webhax-user :userfig-specs *userfig-fieldspecs*)
  (html-thing-lister:thing-component)
  *app*)
 :port 5000)
