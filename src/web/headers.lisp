(in-package :wf/web)


;; CSS classes, tailwind style

(defun featurebox (more-css)
  (gadgets:strcat (or more-css "")
          " m-0 p-2.5 w-4/5 text-xs lineHeight-5 leading-relaxed text-black"))

(defun featurebox-side (more-css)
  (gadgets:strcat (featurebox more-css)
                  " ml-2.5 mr-2.5 mb-4"))

(defun sidebar-stuff (more-css)
  (gadgets:strcat (or more-css "")
          " pt-1 pl-2 pr-2 tracking-[0.015rem]"
          " heir-p:m-2.5 heir-p:mt-4 heir-p:mb-4"
          " heir-p:text-xs heir-p:lineHeight-4"
          " heir-h3:mb-2.5 heir-h3:mt-1.5 heir-h3:ml-neg2.5 heir-h3:mr-neg2.5"
          " heir-h3:p-1 heir-h3:text-sm heir-h3:text-white heir-h3:text-med";heir-h3:lineHeight-3.5 "
          " heir-h3:bg-black heir-h3:tracking-tighter"
          " heir-h4:mt-0 heir-h4:mb-0 heir-h4:ml-2.5"
          " heir-h4:text-sm heir-h4:lineHeight-3.5"))

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

(define-layout (cljs-layout :wrapper #'webhax:page-base)
  (:prepend-parts
   :@head (html-out (:meta :charset "utf-8"))
   :@head (html-out (:meta :name "viewport"
                           :content "width=device-width, initial-scale=1, shrink-to-fit=no"))
   :@css-link "/static/css/re-com/re-com.css"
   :@css-link "/static/css/re-com/material-design-iconic-font.min.css"
   :@css-link "/static/css/main.css")
  (html-out
    (:div :id "header_wrapper"
          (:div :id "account_bar"
                :@account-info))
    (:div
     :class "flex sm:flex-row flex-col"
     (:div :id "left_side" :class (lisp (sidebar-stuff "basis-44"))
           :@site-index :@side-content)
     (:div :class "grow"
           :@messages :@inner :@footnotes)
     (:div :id "right_side" :class (lisp (sidebar-stuff "basis-44"))
           :@site-search :@notifications
           (:div :class (lisp (featurebox-side nil)) :style "opacity: 0;" "_")))
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
  :@account-info #'tw-account-bar
  :@head #'favicon-links
  :@site-index
  (lambda ()
    (html-out
      (:div
       :class (lisp (featurebox-side nil))
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

(defmacro cljs-page ((&rest parts-and-templates) &body body)
  `(webhax-core:input-function-wrapper
    (lambda ()
      (webhax-metaplate:display-page
       #'cljs-layout
       #'cljs-base
       ,@parts-and-templates
       ,@(when body `(:@inner
                      (lambda ()
                        ,@body)))))))
