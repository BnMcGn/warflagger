(in-package :wf/web)

(defvar *handler*)
(defvar *session-store*)

(define-default-layout (warflagger-main :wrapper #'webhax:page-base)
  (:prepend-parts
   :@css-link "/static/css/style.css")
  (html-out
                                        ;;Header
    (:div :id "header_wrapper"
          (:div :id "account_bar"
                :@notifications :@account-info))
                                        ;;Main content
    (:div :id "left_side"
          :@site-search :@site-index :@side-content)
    (:div :id "content"
          :@messages :@inner :@footnotes)
                                        ;;Footer
    (:div :id "footer" :@copyright)))

(define-default-parts warflagger-base
  :@account-info #'account-bar
  :@javascript #'ps-gadgets
  :@javascript-link "/static/javascript/jquery/1.9.1/jquery.js"
  :@head #'favicon-links)

(clsql:connect wf/text-extract::*db-connect-spec*
               :database-type :postgresql-socket3)

(defun favicon-links ()
  (html-out
    (:link :rel "apple-touch-icon" :sizes "180x180"
           :href "/static/img/apple-touch-icon.png")
    (:link :rel "icon" :type "image/png" :sizes "32x32"
           :href "/static/img/favicon-32x32.png")
    (:link :rel "icon" :type "image/png" :sizes "16x16"
           :href "/static/img/favicon-16x16.png")
    (:link :rel "manifest" :href "/static/img/manifest.json")
    (:link :rel "mask-icon" :href "/static/img/safari-pinned-tab.svg"
           :color "#f46a25")
    (:link :rel "shortcut icon" :href "/static/img/favicon.ico")
    (:meta :name "msapplication-config" :content "/static/img/browserconfig.xml")
    (:meta :name "theme-color" :content "#ffffff")))

(defun warflagger-user-info-bundle ()
  (cons
   (cons :user-url "")
   (webhax-user:user-info-bundle)))

(wf/text-extract:initialize-indices)

(defvar *app* (make-instance 'ningle:<app>))

(dependency-auto-watcher routes
  (setf (ningle:route *app* "/text-server/")
        (lambda (params)
          (let ((url (cdr (assoc "url" params :test #'string=))))
            (list 200 '(:content-type "application/json")
                  (list
                   (json:encode-json-to-string
                    (if (opinion-exists-p url)
                        (warflagger:opinion-text-server url)
                        (wf/text-extract:text-server url))))))))

  (setf (ningle:route *app* "/opinion/")
        (quick-page (#'webhax:react-parts #'webhax:redux-parts
                                          #'opinion-components)
          (opinion-form-page)))

  (setf (ningle:route *app* "/opinion-post/" :method :POST)
        (lambda (x)
          (declare (ignore x))
          (opinion-post-response)))

  (setf (ningle:route *app* "/look-post/" :method :POST)
        (input-function-wrapper
         (lambda ()
           (bind-validated-input
               (&key
                (root :integer)
                (opinion :integer))
             (check-signed-up)
             (set-look (get-user-name) :rootid root
                                   :opinionid opinion)))
         :content-type "application/json"))

  (setf (ningle:route *app* "/target/*")
        (quick-page
            (#'webhax:react-parts
             #'target-components
             #'mood-lib
             :@side-content
             (lambda ()
               (bind-validated-input
                   ((id :integer))
                 (funcall
                  (html-thing-lister:connection-display-func
                   'target 'participants)
                  id))))
          (bind-validated-input
              ((id :integer))
            (let ((url (get-rooturl-by-id id)))
              (multiple-value-bind (text opinions looks)
                  (target-data id)
                (mount-component (target-root)
                  :text (lisp-raw text)
                  :opinions (lisp-raw opinions)
                  :url (lisp url)
                  :title (lisp (grab-title url))
                  :looks (lisp-raw looks)
                  :focus '()
                  ))))))

  (setf (ningle:route *app* "/faq/")
        (quick-page ()
            (markdown:markdown
             (asdf:system-relative-pathname 'warflagger "src/faq.md")
             :stream webhax:*webhax-output*)))

  (setf (ningle:route *app* "/flags/")
        (quick-page ()
          (loop
             for category in *flag-categories*
             for labels in *flag-labels*
             for sources in *flag-types-source*
             do
               (html-out
                 (:h2 (str category))
                 (loop
                    for lab in labels
                    for (labsym description) on sources by #'cddr
                    do
                      (progn
                        (htm (:h3 (str lab)))
                        (htm (:p (str description)))))))))

  ;;(setf (ningle:route *app* "/signup/") #'signup-page)

  (setf (ningle:route *app* "/demo/")
        (quick-page (#'webhax:react-parts
                     #'webhax::webhax-ask
                     :@javascript #'webhax-widgets:ps-widgets)
          (demo-pages)))

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
          (clath::logged-in-page))))

;;;Code below starts server. To restart, first stop server thusly:
;;;(clack:stop wf/web::*handler*)
;;;Then evaluate code below.

;;FIXME: Need to handle the user information that will be passed out in OpinML
;; exports. User needs to be able to specify a homepage, whether email address
;; should go out in the data.
(defparameter *userfig-fieldspecs*
  '(:test-value
    (:string
     :initial "a value")))

(clack-server-manager
 *handler*
 (clack-pretend:pretend-builder
  (:insert 3) ;clack.builder:builder
  (clack.middleware.clsql:<clack-middleware-clsql>
   :database-type :postgresql-socket3
   :connection-spec *db-connect-spec*)
  (clack.middleware.static:<clack-middleware-static>
   :path "/static/"
   :root #p"~/quicklisp/local-projects/warflagger/src/static/")
  :session
  (clath:component
   "http://logintest.warflagger.com:5000/clath/")
  (webhax-user:webhax-user :userfig-specs *userfig-fieldspecs*)
  (html-thing-lister:thing-component)
  *app*)
 :port 5000)

