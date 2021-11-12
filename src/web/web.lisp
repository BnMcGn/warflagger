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
   ;;FIXME: Shouldn't need to specify this
   (ps-lib-tool:get-code 'ps-lisp-library)
   (ps-lib-tool:get-code 'ps-gadgets)
   'webhax-widgets:ps-widgets
   (ps-lib-tool:get-code 'ps-react-gadgets)
   'webhax-ask
   (ps-lib-tool:get-code 'flaglib)
   ;'wf-web-library
   ;'mood-lib
   ;'displayables
   'opinion-components
   ;'grouped-components
   ;'titlebar-components
   ;'target-article
   ;'target-thread
   ;'target-summary
   ;'target-components
   ;'opinion-page
   ;'warflagger-things
  ;; 'radmin
   ))

(setf (cl-who:html-mode) :html5)
(define-default-layout (warflagger-main :wrapper #'webhax:page-base)
  (:prepend-parts
   :@head (html-out (:meta :charset "utf-8"))
   :@head (html-out (:meta :name "viewport"
                           :content "width=device-width, initial-scale=1, shrink-to-fit=no"))
   :@css-link "https://cdn.jsdelivr.net/npm/bootstrap@4.6.0/dist/css/bootstrap.min.css"
   :@css-link "/static/css/style.css"
   :@css-link "/static/css/target.css"
   :@css-link "/static/css/react-tabs.css")
  (html-out
    (:div :id "header_wrapper"
          (:div :id "account_bar"
                :@account-info))
    (:div
     :class "container-fluid"
     (:div
      :class "row"
      (:div :id "left_side" :class "col-sm-2 wf-sidebar-width"
            :@site-index :@side-content)
      (:div :class "col"
            :@messages :@inner :@footnotes)
      (:div :id "right_side" :class "col-sm-2 wf-sidebar-width"
            :@site-search :@notifications
            (:div :class "featurebox_side" :style "opacity: 0;" "_"))))
    (:div :id "footer" :class "jumbotron-fluid" :@copyright)))

#|
<nav class="navbar navbar-expand-sm bg-dark navbar-dark">
  <a class="navbar-brand" href="#">Navbar</a>
  <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#collapsibleNavbar">
    <span class="navbar-toggler-icon"></span>
  </button>
  <div class="collapse navbar-collapse" id="collapsibleNavbar">
    <ul class="navbar-nav">
      <li class="nav-item">
        <a class="nav-link" href="#">Link</a>
      </li>
      <li class="nav-item">
        <a class="nav-link" href="#">Link</a>
      </li>
      <li class="nav-item">
        <a class="nav-link" href="#">Link</a>
      </li>    
    </ul>
  </div>  
</nav>
|#

;;FIXME: react, react-dom should be loaded from the npm bundle.
(define-default-parts warflagger-base
  :@javascript-link "https://cdnjs.cloudflare.com/ajax/libs/babel-polyfill/6.26.0/polyfill.js"
  :@javascript-link "/static/javascript/local.js"
  :@javascript-link "/static/javascript/warflagger-bundle.js"
  :@javascript (ps:ps
                 (setf -react (require "react"))
                 (setf (ps:@ -react #:create-class) (require "create-react-class"))
                 (setf (ps:@ -react -d-o-m) (require "react-dom-factories"))
                 (setf -redux (require "redux"))
                 (setf -react-redux (require "react-redux")))

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
       (:div (:a :href "/opinions-recent/" "Recent Opinions"))
       (:div (:a :href "/grouped/" "Current Discussions"))
       (:div (:a :href "/opinion/" "Write an Opinion"))
       (:div (:a :href "/faq/" "FAQ"))
       (:div (:a :href "http://warblog.warflagger.net/" "WarBlog"))))))

(defun clath:clath-page-wrapper (title body-func)
  (funcall
   (webhax-route:quick-page
       (:@title title)
     (princ (funcall body-func) *webhax-output*))))

(defun clath:clath-login-page ()
  (clath:clath-page-wrapper
   "Login"
   (lambda ()
     (named-text :login-page)
     (clath:login-links))))

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

(setf (webhax-user::login-destination) "/user/")

(proto:dependency-auto-watcher routes
  (setf (ningle:route *app* "/text-server/")
        (lambda (params)
          (let ((url (cdr (assoc "url" params :test #'string=))))
            (list 200 '(:content-type "application/json")
                  (list
                   (json:encode-json-to-string
                    (text-server-dispatcher url)))))))

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
            (
             :@side-content
             (lambda ()
               (bind-validated-input
                   ((id :integer))
                 (target-participants-sidebar id))))
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

  ;;FIXME: temporary
  (setf (ningle:route *app* "/target2/*")
        (quick-page
            ()
          (bind-validated-input
              ((id :integer)
               &key
               (tmode :integer))
            (let ((url (get-rooturl-by-id id)))
              (mount-component (target-iloader)
                :rooturl (lisp url)
                :tmode (lisp tmode)
                :child target-root)))))


  ;;FIXME: redirect if url found
  (setf (ningle:route *app* "/new-target/")
        (quick-page
            ()
          (bind-validated-input
              (&key
               (url :url))
            (mount-component (new-target)
              :url (lisp url)
              :username (lisp (webhax-user:get-user-name))))))

  ;;FIXME: Think about taking over the /opinion/ URL for this
  (setf (ningle:route *app* "/opinion-page/*")
        (quick-page
            ()
          (bind-validated-input
              ((id :integer))
            (let* ((opin (opinion-by-id id))
                   (rooturl (assoc-cdr :rooturl opin)))
              (mount-component (target-loader)
                :url (lisp rooturl)
                :rootid (lisp (assoc-cdr :rootid opin))
                :title (lisp (grab-title rooturl))
                :looks (lisp (when (authenticated?)
                               (ps-gadgets:as-ps-data
                                (get-looks (get-user-name) (assoc-cdr :rootid opin)))))
                :focus (lisp (list* 'list (tree-address id)))
                :username (lisp (webhax-user:get-user-name))
                :child opinion-page)))))

  (setf (ningle:route *app* "/o/*")
        (quick-page
            ()
          (bind-validated-input
              ((iid :string))
            (handler-case
                (let* ((opin (opinion-by-id iid))
                       (rooturl (assoc-cdr :rooturl opin)))
                  (mount-component (target-loader)
                    :url (lisp rooturl)
                    :rootid (lisp (assoc-cdr :rootid opin))
                    :title (lisp (grab-title rooturl))
                    :looks (lisp (when (authenticated?)
                                   (ps-gadgets:as-ps-data
                                    (get-looks (get-user-name) (assoc-cdr :rootid opin)))))
                    :focus (lisp (list* 'list (tree-address (assoc-cdr :id opin))))
                    :username (lisp (webhax-user:get-user-name))
                    :child opinion-page))
              (warflagger:not-found (c)
                (declare (ignore c))
                (webhax-core:web-fail-404))))))

  (setf (ningle:route *app* "/o2/*")
        (quick-page
            ()
          (bind-validated-input
              ((iid :string))
            (handler-case
                (let* ((opin (opinion-by-id iid))
                       (rooturl (assoc-cdr :rooturl opin)))
                  (mount-component (target-iloader)
                    ;;:url (lisp rooturl)
                    :rooturl (lisp rooturl)
                    :title (lisp (grab-title rooturl))
                    :looks (lisp (when (authenticated?)
                                   (ps-gadgets:as-ps-data
                                    (get-looks (get-user-name) (assoc-cdr :rootid opin)))))
                    :focus (lisp (list* 'list (wf/ipfs::tree-address opin)))
                    :username (lisp (webhax-user:get-user-name))
                    :child opinion-page))
              (warflagger:not-found (c)
                (declare (ignore c))
                (webhax-core:web-fail-404))))))

  (setf (ningle:route *app* "/grouped/*")
        (quick-page
            ()
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

  (setf (ningle:route *app* "/user-manual/")
        (quick-page ()
          (html-out
            (:div :class "featurebox"
                  (named-text :user-manual)))))

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

  (setf (ningle:route *app* "/user/")
        (quick-page ()
          (user-home-page)))

  (setf (ningle:route *app* "/u/*")
        (quick-page (#'author-page-parts)))

  (setf (ningle:route *app* "/author/*")
        (quick-page (#'author-page-parts)))

  (unless-production
   (setf (ningle:route *app* "/demo/")
         (quick-page ()
           (demo-pages))))

  (unless-production
   (setf (ningle:route *app* "/radmin/")
         (quick-page ()
           (mount-component (admin-page)))))

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
        (quick-page (
                     #'main-page-parts))))

;;FIXME: Need to handle the user information that will be passed out in OpinML
;; exports. User needs to be able to specify a homepage, whether email address
;; should go out in the data.
(defparameter *userfig-fieldspecs* nil)

;;;Code below starts server. To restart, first stop server thusly:
;;;(clack:stop wf/web::*handler*)
;;;Then evaluate code below.
(defun run-test-server ()
  (clack-server-manager
   *handler*
   (terminate-thread-on-broken-pipe
     (clack-pretend:pretend-builder
        (:insert 0)
        (clsql-middleware :postgresql-socket3 *test-db-connect-spec*)
        (webhax:header-adder "/static" '("Access-Control-Allow-Origin" "*"))
        (:static
         :path "/static/"
         :root #p"~/quicklisp/local-projects/wf-static/")
        :session
        (claxy:middleware (list (list "/ipfs/" "http://localhost:8080/ipfs/")
                                (list "/ipns/" "http://localhost:8080/ipns/")))
        (claxy:middleware (list (list "/x/" "https://warflagger.net/")))
        (clath:component
         "https://logintest.warflagger.com:5000/")
        (webhax-user:webhax-user :userfig-specs *userfig-fieldspecs*)
        ;;(snooze:make-clack-middleware)
        *app*))
   :port 5000
   :ssl t
   :ssl-key-file wf/local-settings:*ssl-key-file*
   :ssl-cert-file wf/local-settings:*ssl-cert-file*
   :ssl-key-password wf/local-settings:*ssl-password*))

(defun run-production-server ()
  ;; Because we aren't running the block below on live server
  (clsql:connect wf/local-settings:*db-connect-spec*
                 :database-type wf/local-settings:*db-connect-type* :if-exists :old)
  (terminate-thread-on-broken-pipe
    (clack-server-manager
    *handler*
    (clack-pretend:pretend-builder
        (:insert 0 :errors-only t
         :logfile "/var/log/warflagger.requests")
        (:backtrace
         :output #p"/var/log/warflagger.err"
         :result-on-error `(500 (:content-type "text/plain") ("Internal Server Error")))
        (clsql-middleware :postgresql-socket3 *db-connect-spec*)
        :session
        (clath:component
         *base-url*)
        (webhax-user:webhax-user :userfig-specs *userfig-fieldspecs*)
        *app*)
    :port 5005))
  (bordeaux-threads:join-thread
   (find-if
    (lambda (th)
      (string= (bordeaux-threads:thread-name th) "clack-handler-hunchentoot"))
    (bordeaux-threads:all-threads))))

(when wf/local-settings:*auto-run*
  ;;FIXME: isn't working for production.
  (if-production
   (clsql:connect wf/local-settings:*db-connect-spec*
                  :database-type wf/local-settings:*db-connect-type*)
   (clsql:connect wf/local-settings:*test-db-connect-spec*
                  :database-type wf/local-settings:*db-connect-type* :if-exists :old))
  (unless-production
   ;;FIXME: we don't have write permission on production, so update through git instead.
   ;; This isn't quite the best way to do things.
   (write-warflagger-js-resources))
  (unless-production
   (clerk:start))
  (if-production (run-production-server) (run-test-server)))
