(in-package :wf/web)


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

(define-parts cljs-base
  :@javascript-link "/static/javascript/local.js"
  :@javascript-link "/static/cljs-out/dev-main.js"
  :@account-info #'account-bar
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

