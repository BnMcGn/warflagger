(in-package :wf/web)

(defvar *handler*)
(defvar *session-store*)

(define-default-layout (warflagger-main :wrapper #'webhax:page-base)
  (:prepend-parts
   :@css-link "/static/css/style.css"
   ;;:@javascript-link "/static/javascript/warflagger-bundle.js"
   :@javascript (wf-web-library))
  (html-out
                                        ;;Header
    (:div :id "header_wrapper"
          (:div :id "account_bar"
                :@account-info))
                                        ;;Main content
    (:div :id "left_side"
          :@site-index :@side-content)
    (:div :id "right_side"
          :@site-search :@notifications
          (:div :class "featurebox_side" :style "opacity: 0;" "_"))
    (:div :id "content"
          :@messages :@inner :@footnotes)
                                        ;;Footer
    (:div :id "footer" :@copyright)
    (str (tracking-code))))

(define-default-parts warflagger-base
  :@account-info #'account-bar
  :@javascript #'ps-gadgets
  :@javascript-link "/static/javascript/jquery/1.9.1/jquery.js"
  :@head #'favicon-links
  :@site-index
  (lambda ()
    (html-out
      (:div
       :class "featurebox_side"
       (:h3 "Index")
       (:div (:a :href "/" "Home"))
       (:div (:a :href "/things/things/opinion" "Recent Opinions"))
       (:div (:a :href "/grouped/" "Current Discussions"))
       (:div (:a :href "/opinion/" "Write an Opinion"))
       (:div (:a :href "/faq/" "FAQ"))
       (:div (:a :href "/introduction/" "Introduction"))))))

(defun clath:clath-page-wrapper (title body-func)
  (funcall
   (webhax-route:quick-page
       (:@title title)
     (princ (funcall body-func) *webhax-output*))))

(if-production
 (clsql:connect wf/local-settings:*db-connect-spec*
                :database-type wf/local-settings:*db-connect-type*)
 (clsql:connect wf/local-settings:*test-db-connect-spec*
                :database-type wf/local-settings:*db-connect-type*))

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

(setf (webhax-user::login-destination) "/home/")

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
                       :opinionid opinion))
           (print (json:encode-json-to-string "OK") *webhax-output*))
         :content-type "application/json"))

  ;;FIXME: Target page needs to handle URLs that don't have a rootid yet.
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
                  (html-thing-lister:connector-display-func
                   'target 'participants)
                  id))))
          (bind-validated-input
              ((id :integer))
            (let ((url (get-rooturl-by-id id)))
              (mount-component (target-loader)
                :url (lisp url)
                :rootid (lisp id)
                :title (lisp (grab-title url))
                :looks (lisp-raw (json:encode-json-to-string
                                  (get-looks (get-user-name) id)))
                :focus '()
                :username (lisp (webhax-user:get-user-name))
                )))))

  (setf (ningle:route *app* "/grouped/*")
        (quick-page
            (#'webhax:react-parts
             #'target-components
             #'mood-lib
             :@javascript #'grouped-components)
          (grouped-page)))

  (setf (ningle:route *app* "/faq/")
        (quick-page ()
          (html-out
            (:div :class "featurebox"
                  (named-text :faq)))))

  (setf (ningle:route *app* "/introduction/")
        (quick-page ()
          :@inner
          (large-logo)
          (html-out
            (:div :class "featurebox"
                  (named-text :introduction)))))

  (setf (ningle:route *app* "/bookmarklet/")
        (quick-page ()
          (html-out
            (:div :class "featurebox"
                  (named-text :bookmarklet)))))

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

  (unless-production
   (setf (ningle:route *app* "/flag-color/")
         #'flag-color-page))

  (setf (ningle:route *app* "/home/")
        (quick-page (#'webhax:react-parts #'warflagger-things)
          (user-home-page)))

  (setf (ningle:route *app* "/author/*")
        (quick-page (#'webhax:react-parts
                     #'warflagger-things
                     #'author-page-parts)))

  (unless-production
   (setf (ningle:route *app* "/demo/")
         (quick-page (#'webhax:react-parts
                      #'webhax::webhax-ask
                      :@javascript #'webhax-widgets:ps-widgets)
           (demo-pages))))

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
        (quick-page (#'webhax:react-parts
                     #'target-components
                     #'mood-lib
                     :@javascript #'grouped-components
                     #'main-page-parts))))

;;;Code below starts server. To restart, first stop server thusly:
;;;(clack:stop wf/web::*handler*)
;;;Then evaluate code below.

;;FIXME: Need to handle the user information that will be passed out in OpinML
;; exports. User needs to be able to specify a homepage, whether email address
;; should go out in the data.
(defparameter *userfig-fieldspecs* nil)

(if-production
 (defun run-server ()
   (clack-server-manager
    *handler*
    (lack:builder
     (:backtrace
      :output #p"/var/log/warflagger.err"
      :result-on-error `(500 (:content-type "text/plain") ("Internal Server Error")))
     (clack.middleware.clsql:<clack-middleware-clsql>
      :database-type :postgresql-socket3
      :connection-spec *db-connect-spec*)
     :session
     (clath:component
      *base-url*)
     (webhax-user:webhax-user :userfig-specs *userfig-fieldspecs*)
     (html-thing-lister:thing-component)
     *app*)
    ;:server :fcgi
    ;:use-thread nil
    :port 5005)
   (sb-thread:join-thread
    (find-if
     (lambda (th)
       (string= (sb-thread:thread-name th) "clack-handler-hunchentoot"))
     (sb-thread:list-all-threads))))
 (clack-server-manager
  *handler*
  (clack-pretend:pretend-builder
   (:insert 3) ;clack.builder:builder
   (clack.middleware.clsql:<clack-middleware-clsql>
    :database-type :postgresql-socket3
    :connection-spec *test-db-connect-spec*)
   (clack.middleware.static:<clack-middleware-static>
    :path "/static/"
    :root #p"~/quicklisp/local-projects/wf-static/")
   :session
   (clath:component
    "http://logintest.warflagger.com:5000/")
   (webhax-user:webhax-user :userfig-specs *userfig-fieldspecs*)
   (html-thing-lister:thing-component)
   *app*)
  :port 5000))



