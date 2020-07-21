(in-package :wf/web)

(defvar *handler*)
(defvar *session-store*)

(defparameter *warflagger-js-resources* "/static/javascript/warflagger-resources.js")

;;Compile resources

;;FIXME: How do we update? Won't run if recompiled
(defun write-warflagger-js-resources ()
  (write-js-resources
   (concatenate 'string wf/local-settings:*static-path* "javascript/warflagger-resources.js")
   'react:build
   'ps-gadgets
   'webhax-widgets:ps-widgets
   'ps-react-gadgets:ps-react-gadgets
   'webhax-ask
   'wf-web-library
   'mood-lib
   'displayables
   'opinion-components
   'grouped-components
   'titlebar-components
   'target-article
   'target-thread
   'target-summary
   'target-components
   'opinion-page
   'warflagger-things))

(define-default-layout (warflagger-main :wrapper #'webhax:page-base)
  (:prepend-parts
   :@head (html-out (:meta :charset "utf-8"))
   :@css-link "/static/css/style.css")
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

;;FIXME: react, react-dom should be loaded from the npm bundle.
(define-default-parts warflagger-base
  :@javascript-link "https://cdnjs.cloudflare.com/ajax/libs/babel-polyfill/6.26.0/polyfill.js"
  :@javascript-link "https://unpkg.com/react@16.12.0/umd/react.development.js"
  :@javascript-link "https://unpkg.com/react-dom@16.12.0/umd/react-dom.development.js"
  :@javascript (ps:ps (setf (ps:@ -react #:create-class) (require "create-react-class"))
                      (setf (ps:@ -react -d-o-m) (require "react-dom-factories")))
  :@javascript-link "/static/javascript/warflagger-bundle.js"
  :@javascript-link
  "https://cdnjs.cloudflare.com/ajax/libs/redux/4.0.0/redux.js"
  :@javascript-link
  "https://cdnjs.cloudflare.com/ajax/libs/react-redux/5.0.7/react-redux.js"

  :@account-info #'account-bar
  :@javascript-link "/static/javascript/jquery/1.9.1/jquery.js"
  :@javascript-link  "https://cdn.jsdelivr.net/npm/lodash@4/lodash.min.js"
  ;;FIXME: Should be able to bundle these with browserify. Can't.
  :@javascript-link "/static/node_modules/rangy/lib/rangy-core.js"
  :@javascript-link "/static/node_modules/rangy/lib/rangy-textrange.js"
  :@javascript-link *warflagger-js-resources*
  :@head #'favicon-links
  :@site-index
  (lambda ()
    (html-out
      (:div
       :class "featurebox_side"
       (:h3 "Index")
       (:div (:a :href "/" "Home"))
       (:div (:a :href "/introduction/" "Introduction"))
       (:div (:a :href "/things/things/opinion" "Recent Opinions"))
       (:div (:a :href "/grouped/" "Current Discussions"))
       (:div (:a :href "/opinion/" "Write an Opinion"))
       (:div (:a :href "/faq/" "FAQ"))
       (:div (:a :href "http://warblog.warflagger.net/" "WarBlog"))))))

(defun clath:clath-page-wrapper (title body-func)
  (funcall
   (webhax-route:quick-page
       (:@title title)
     (princ (funcall body-func) *webhax-output*))))

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

(proto:dependency-auto-watcher routes
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
        (quick-page ()
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
            (#'target-parts
             :@side-content
             (lambda ()
               (bind-validated-input
                   ((id :integer))
                 (funcall
                  (html-thing-lister:connector-display-func
                   'target 'participants)
                  id))))
          (bind-validated-input
              ((id :integer)
               &key
               (tmode :integer))
            (let ((url (get-rooturl-by-id id)))
              (mount-component (target-loader)
                :url (lisp url)
                :rootid (lisp id)
                :title (lisp (grab-title url))
                :looks (lisp (when (authenticated?)
                               (ps-gadgets:as-ps-data
                                (get-looks (get-user-name) id))))
                :focus '()
                :tmode (lisp tmode)
                :username (lisp (webhax-user:get-user-name))
                )))))

  (setf (ningle:route *app* "/new-target/")
        (quick-page
            (#'target-parts)
          (bind-validated-input
              (&key
               (url :url))
            (mount-component (new-target)
              :url (lisp url)
              :username (lisp (webhax-user:get-user-name))))))

  ;;FIXME: Think about taking over the /opinion/ URL for this
  (setf (ningle:route *app* "/opinion-page/*")
        (quick-page
            (#'target-parts)
          (bind-validated-input
              ((id :integer))
            (let* ((opin (opinion-by-id id))
                   (rooturl (get-rooturl-by-id (assoc-cdr :rooturl opin))))
              (mount-component (target-loader)
                :url (lisp rooturl)
                :rootid (lisp (assoc-cdr :rooturl opin))
                :title (lisp (grab-title rooturl))
                :looks (lisp (when (authenticated?)
                               (ps-gadgets:as-ps-data
                                (get-looks (get-user-name) (assoc-cdr :rooturl opin)))))
                :focus (lisp (list* 'list (tree-address id)))
                :username (lisp (webhax-user:get-user-name))
                :child opinion-page)))))

  (setf (ningle:route *app* "/grouped/*")
        (quick-page
            (#'target-parts)
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
        (quick-page ()
          (user-home-page)))

  (setf (ningle:route *app* "/author/*")
        (quick-page ()))

  (unless-production
   (setf (ningle:route *app* "/demo/")
         (quick-page ()
           (demo-pages))))

  (setf (ningle:route *app* "/private-call-cleanup-test-user/")
        (quick-page ()
          (cleanup-test-user)
          (html-out
            (:h1 "Done"))))

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

  ;;FIXME: bad URL should not cause this code to get stuck in the debugger. Should return error.
  (setf (ningle:route *app* "/target-seek/")
        (input-function-wrapper
         (lambda ()
           (bind-validated-input
               (&key
                ;;FIXME: :url not used to keep things from jamming.
                (url :string))
             (princ (json:encode-json-to-string (target-seek-server url))
                    *webhax-output*)))
         :content-type "application/json"
         :headers '("Access-Control-Allow-Origin" "*")))

  (setf (ningle:route *app* "/")
        (quick-page (#'target-parts
                     #'main-page-parts))))

;;FIXME: Need to handle the user information that will be passed out in OpinML
;; exports. User needs to be able to specify a homepage, whether email address
;; should go out in the data.
(defparameter *userfig-fieldspecs* nil)

;;;Code below starts server. To restart, first stop server thusly:
;;;(clack:stop wf/web::*handler*)
;;;Then evaluate code below.

(defun run-server ()
  (clack-server-manager
   *handler*
   (if-production
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
    (clack-pretend:pretend-builder
     (:insert 3) ;clack.builder:builder
     (clack.middleware.clsql:<clack-middleware-clsql>
      :database-type :postgresql-socket3
      :connection-spec *test-db-connect-spec*)
     (webhax:header-adder "/static" '("Access-Control-Allow-Origin" "*"))
     (clack.middleware.static:<clack-middleware-static>
      :path "/static/"
      :root #p"~/quicklisp/local-projects/wf-static/")
     :session
     (clath:component
      "http://logintest.warflagger.com:5000/")
     (webhax-user:webhax-user :userfig-specs *userfig-fieldspecs*)
     (:mount "/rest" (snooze:make-clack-app))
     (html-thing-lister:thing-component)
     *app*))
   :port (if-production 5005 5000))
  (when-production
   #+sbcl (sb-thread:join-thread
           (find-if
            (lambda (th)
              (string= (sb-thread:thread-name th) "clack-handler-hunchentoot"))
            (sb-thread:list-all-threads)))))

(when wf/local-settings:*auto-run*
  (unless-production
   ;;FIXME: we don't have write permission on production, so update through git instead.
   ;; This isn't quite the best way to do things.
   (write-warflagger-js-resources))
  ;;FIXME: isn't working for production.
  (if-production
   (clsql:connect wf/local-settings:*db-connect-spec*
                  :database-type wf/local-settings:*db-connect-type*)
   (clsql:connect wf/local-settings:*test-db-connect-spec*
                  :database-type wf/local-settings:*db-connect-type*))
  (run-server))
