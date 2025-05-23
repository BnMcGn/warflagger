(in-package :wf/web)

(defvar *handler*)
(defvar *session-store*)

(defparameter *warflagger-js-resources* "/static/javascript/warflagger-resources.js")

;;Compile resources

(defun warflagger-user-info-bundle ()
  (cons
   (cons :user-url "")
   (webhax-user:user-info-bundle)))

;;(wf/text-extract:initialize-indices)

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
        (cljs-page ()
          (make-opinion-page)))

  (unless-production
   (setf (ningle:route *app* "/mock-make/*")
         (cljs-page ((title-part "WF: Opinion Maker Mockups"))
           (mock-make-page))))

  (setf (ningle:route *app* "/author-url-data/")
        (input-function-wrapper
         (lambda ()
           (when-let* ((user (get-user-name))
                       (aid (get-local-user-id user))
                       (author (get-author-representation aid)))
             (json:encode-json-plist (author-urls author) *webhax-output*)))
         :content-type "application/json"))

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

  (setf (ningle:route *app* "/target/*")
        (cljs-page
            (:@side-content
             (lambda ()
               (bind-validated-input
                   ((id (:or :integer :url)))
                 (when-let ((id (if (integerp id)
                                    id
                                    (and (rooturl-p id) (get-rooturl-id id)))))
                   (target-participants-sidebar id)))))
          (bind-validated-input
           ((id (:or :integer :url))
            &key
            (tmode :string))
           (let* ((url (if (integerp id) (get-rooturl-by-id id) id))
                  (touched (or (rooturl-p url) (wf/ipfs:ipfs-have-text-for-rooturl? url))))
             (mount-cljs-component ("target")
               :rooturl (lisp url)
               :touched-p (lisp (if touched 'true 'false))
               :refd (lisp (unless touched (cons 'list (refd-to url))))
               :tmode (lisp tmode))))))

  (setf (ningle:route *app* "/o/*")
        (cljs-page
           ()
         (bind-validated-input
          ((iid :string)
           &key
           (tmode :string))
          (handler-case
              (let* ((opin (opinion-by-id iid))
                     (rooturl (assoc-cdr :rooturl opin)))
                (mount-cljs-component ("opinion-page")
                  :rooturl (lisp rooturl)
                  :tmode (lisp tmode)
                  :focus (lisp (list* 'list (wf/ipfs::tree-address opin)))))
            (warflagger:not-found (c)
              (declare (ignore c))
              (webhax-core:web-fail-404))))))

  (setf (ningle:route *app* "/social-image-source/*")
        (plain-page ()
           (bind-validated-input
               ((target (:or :url (webhax-validate:predicate-test
                                   #'warflagger:iid-p "Not an OpinionID"))))
             (mount-cljs-component ("social-card")
               :target (lisp target)
               :rooturl (lisp
                         (if (iid-p target)
                             (assoc-cdr :rooturl (opinion-by-id target))
                             target))))))

  (setf (ningle:route *app* "/social-image/*")
        (input-function-wrapper
         (lambda ()
           (bind-validated-input
               ((target (:or :url (webhax-validate:predicate-test
                                   #'warflagger:iid-p "Not an OpinionID"))))
             (unless (target-exists-p target)
               (webhax-core:web-fail-500 "Target not found"))
             (let* ((api-url "http://screeenly.com/api/v1/fullsize")
                    (target (if (iid-p target) target (quri:url-encode target)))
                    (params (list
                             (cons "key" wf/local-settings:*screeenly-api-key*)
                             (cons "url" (gadgets:strcat
                                          "https://warflagger.net/social-image-source/"
                                          target))
                             (cons "height" "630")
                             (cons "width" "1200")
                             (cons "delay" "10")))
                    (results (json:decode-json (drakma:http-request
                                                api-url
                                                :method :post
                                                :parameters params
                                                :want-stream t))))
               (unless (assoc :base-64--raw results)
                 (webhax-core:web-fail-500 "Unable to generate card"))
               (coerce (cl-base64:base64-string-to-usb8-array
                        (gadgets:assoc-cdr :base-64--raw results))
                       '(vector (unsigned-byte 8))))))
         :content-type "image/png"
         :capture-html false))

  (setf (ningle:route *app* "/grouped/*")
        (cljs-page ((title-part "WF: Discussions"))
          (mount-cljs-component ("grouped"))))

  (setf (ningle:route *app* "/faq/")
        (cljs-page ((title-part "WF: Frequently Asked Questions"))
          (html-out
            (:div :class "featurebox"
                  (named-text :faq)))))

  (setf (ningle:route *app* "/introduction/")
        (cljs-page ((title-part "WF: Introduction"))
          :@inner
          (large-logo)
          (html-out
            (:div :class "featurebox"
                  (named-text :introduction)))))

  (setf (ningle:route *app* "/user-manual/")
        (cljs-page ((title-part "WF: User Manual"))
          (html-out
            (:div :class "featurebox"
                  (named-text :user-manual)))))

  (setf (ningle:route *app* "/bookmarklet/")
        (cljs-page ()
          (html-out
            (:div :class "featurebox"
                  (named-text :bookmarklet)))))

  (setf (ningle:route *app* "/flags/")
        (cljs-page ((title-part "WF: Available Flags"))
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
        (cljs-page ((title-part
                     (if (signed-up?)
                         (format nil "WF: User: ~a"
                                 (get-apparent-display-name
                                  (get-user-name)))
                         "WF: Not Logged In")))
          (user-home-page)))

  (setf (ningle:route *app* "/u/*")
        (cljs-page (#'author-page-parts)))

  (setf (ningle:route *app* "/author/*")
        (cljs-page (#'author-page-parts)))

  (unless-production
   (setf (ningle:route *app* "/demo/")
         (cljs-page ()
           (demo-pages))))

  (setf (ningle:route *app* "/private-call-cleanup-test-user/")
        (cljs-page ()
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
        (cljs-page (#'main-page-parts (title-part "WF: Join the Fray!")))))

;;FIXME: Need to handle the user information that will be passed out in OpinML
;; exports. User needs to be able to specify a homepage, whether email address
;; should go out in the data.
(defparameter *userfig-fieldspecs*
  '(advanced (:yesno :initial nil :editable t
              :description "Advanced posting options")))

(defun all-fieldspecs ()
  (concatenate 'list webhax-user::*userfig-for-user* *userfig-fieldspecs*))

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
        (:session
         :store
         (lack.session.store.dbi:make-dbi-store
          :connector (lambda () (apply #'dbi:connect
                                       wf/local-settings:*session-db-connect-spec*))))
        (claxy:middleware (list (list "/ipfs/" "http://localhost:8080/ipfs/")
                                (list "/ipns/" "http://localhost:8080/ipns/")))
        (clath:component
         wf/local-settings::*base-url*)
        (webhax-user:webhax-user :userfig-specs *userfig-fieldspecs*)
        *app*))
   :port 5000))


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


(defun web-restart ()
  (clack:stop *handler*)
  (if-production (run-production-server) (run-test-server))
  (shim-handle-response))

(when wf/local-settings:*auto-run*
  ;;FIXME: isn't working for production.
  (if-production
   (clsql:connect wf/local-settings:*db-connect-spec*
                  :database-type wf/local-settings:*db-connect-type*)
   (clsql:connect wf/local-settings:*test-db-connect-spec*
                  :database-type wf/local-settings:*db-connect-type* :if-exists :old))
  (unless-production
   (clerk:start))
  (if-production (run-production-server) (run-test-server))
  (shim-handle-response))
