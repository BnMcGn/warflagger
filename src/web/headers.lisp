(in-package :wf/web)


(eval-always
 (defmacro cljs-page ((&rest parts-and-templates) &body body)
   `(webhax-core:input-function-wrapper
     (lambda ()
       (webhax-metaplate:display-page
        #'cljs-layout
        #'cljs-base
        ,@parts-and-templates
        ,@(when body `(:@inner
                       (lambda ()
                         ,@body))))))))

(eval-always
  (defmacro plain-page ((&rest parts-and-templates) &body body)
    `(webhax-core:input-function-wrapper
      (lambda ()
        (webhax-metaplate:display-page
         #'webhax:page-base
         #'cljs-base
         ,@parts-and-templates
         ,@(when body `(:@inner
                        (lambda ()
                          ,@body))))))))

;; CSS classes, tailwind style

(defun featurebox (more-css)
  (gadgets:strcat (or more-css "")
          " m-0 p-2.5 w-4/5 text-xs lineHeight-5 leading-relaxed text-black"))

(defun featurebox-side (more-css)
  (gadgets:strcat (featurebox more-css)
                  " sm:ml-2.5 sm:mr-2.5 mb-4 mx-0"))

(defun sidebar-stuff (more-css)
  (gadgets:strcat (or more-css "")
          " sm:basis-44"
          " pt-[2px] sm:pt-1 px-0 sm:pl-2 sm:pr-2 tracking-[0.015rem]"
          " heir-p:m-2.5 heir-p:mt-4 heir-p:mb-4"
          " heir-p:text-xs heir-p:lineHeight-4"
          " heir-h3:mb-2.5 heir-h3:mt-1.5 heir-h3:ml-neg2.5 heir-h3:mr-neg2.5"
          " heir-h3:p-1 heir-h3:text-sm heir-h3:text-white heir-h3:text-med";heir-h3:lineHeight-3.5 "
          " heir-h3:bg-black heir-h3:tracking-tighter"
          " heir-h4:mt-0 heir-h4:mb-0 heir-h4:ml-2.5"
          " heir-h4:text-sm heir-h4:lineHeight-3.5"))

(setf (cl-who:html-mode) :html5)

(define-layout (cljs-layout :wrapper #'webhax:page-base)
  (html-out
    (:div :id "header_wrapper"
          (:div :id "account_bar"
                :@account-info))
    (:div
     :class "flex sm:flex-row flex-col"
     (:div :id "left_side"
           :class (lisp (sidebar-stuff "mt-0 child:sm:mt-0 child:mb-[2px] "))
           :@site-index :@side-content)
     (:div :class "grow sm:w-min min-w-full sm:min-w-0"
           :@messages :@inner :@footnotes)
     (:div :id "right_side" :class (lisp (sidebar-stuff ""))
           :@site-search :@notifications
           (:div :class (lisp (featurebox-side nil)) :style "opacity: 0;" "_")))
    (:div :id "footer" :class "jumbotron-fluid" :@copyright)))

(defparameter *index*
  '(("/" "Home")
    ("/introduction/" "Introduction")
    ("/opinions-recent/" "Recent Opinions")
    ("/grouped/" "Current Discussions")
    ("/opinion/" "Write an Opinion")
    ("/faq/" "FAQ")
    ("http://warblog.warflagger.net/" "WarBlog")))

(defun get-all-user-visible-data ()
  (when (signed-up?)
    (hu:hash->plist
     (userfig::prep-user-data
      (userfig:get-user-visible-data (get-user-name) (all-fieldspecs))))))

(define-parts cljs-base
  :@head (lambda () (html-out (:meta :charset "utf-8")))
  :@head (lambda ()
           (html-out
             (:meta :name "viewport"
                    :content "width=device-width, initial-scale=1, shrink-to-fit=no")))
  :@css-link "/static/css/re-com/re-com.css"
  ;;FIXME: We have both fontawesome, for clath, and material design icons. Refactor
  :@css-link "/static/css/re-com/material-design-iconic-font.min.css"
  :@css-link "/static/css/main.css"
  :@css clath::*provider-container-css*
  :@javascript-link "/static/javascript/local.js"
  :@javascript-link "/static/cljs-out/main.js"
  ;;:@javascript-link "/static/cljs-out/dev/main_bundle.js"
  :@javascript
  (ps:ps
    (defparameter |userfig-data|
      (ps:lisp (list* 'ps:create (get-all-user-visible-data)))))
  :@account-info #'tw-account-bar
  :@head #'favicon-links
  :@site-index
  (lambda ()
    (html-out
      (:div
       :class (lisp (gadgets:strcat (featurebox-side nil) " hidden sm:block"))
       (:h3 "Index")
       (lisp
        (loop for (url label) in *index*
              do (htm (:div (:a :href url (str label)))))))
      (:div
       :class "sm:hidden flex bg-black justify-center"
       (:select
        :class "p-2 bg-black text-white"
        :|onChange| (ps:ps
                     (ps:chain
                      window location
                      (replace
                       (ps:@
                        (ps:getprop (ps:@ this options) (ps:@ this selected-index))
                        value))))
        (:option :value "" :selected "selected" "Index")
        (lisp
         (loop for (url label) in *index*
               do (htm (:option :value url (str label))))))))))

(defun clath:clath-page-wrapper (title body-func)
  (funcall
   (cljs-page
       (:@title title)
     (princ (funcall body-func) *webhax-output*))))

(defun clath:clath-login-page ()
  (clath:clath-page-wrapper
   "Login"
   (lambda ()
     (html-out
       (:div
        :class "border-solid border-8 rounded-lg m-2 p-2 border-blue-200 inline-block"
        (named-text :login-page)
        (:div :class "m-2"
              (str (clath:login-links))))))))

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

