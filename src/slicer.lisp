;;;; slicer.lisp

(in-package #:wf/admin)

(defparameter *document* nil)
(defparameter *url* nil)
(defparameter *text* nil)
(defparameter *tdat* nil)

(defun load-url (&optional (url *url*))
  ;;reset
  (setf *text* nil)
  (unless (equal url *url*)
    (setf *url* url))
  (setf *document* (lquery:$ (initialize (dexador:get url)))))

(defun load-text ()
  ;;For now. Might want edited text from ipfs?
  (setf *text* (wf/text-extract:grab-text *url*))
  (setf *tdat* (warflagger:create-textdata *text*)))

(defun raw-links ()
  (mapcar #'plump-sexp:serialize (gadgets:sequence->list (lquery:$ *document* "a"))))

(defun domain-keyword (url)
  (alexandria:make-keyword (gadgets:to-uppercase (warflagger:uri-domain url))))

(defun link-class (link)
  (fetch-keyword :class (fetch-keyword :a link)))

(defun useful-string-p (itm)
  (and
   (stringp itm)
   (not-empty itm)
   (not-empty (string-strip itm))))

;;FIXME: borrowed from warflagger
(defun clean-string-for-excerpt (str)
  (coerce
   (cl-utilities:collecting
     (let ((last-was-white nil))
       (dotimes (i (length str))
         (if (member (elt str i) *whitespace-characters*)
             (unless last-was-white
               (setf last-was-white t)
               (cl-utilities:collect #\ ))
             (progn
               (setf last-was-white nil)
               (cl-utilities:collect (elt str i)))))))
   'string))

(defun text-from-tag (tagsxp)
  (string-strip
   (clean-string-for-excerpt
    (apply #'strcat
           (mapcar (lambda (x)
                     (cond
                       ((stringp x) x)
                       ((fetch-keyword :div x) (text-from-tag x))
                       ((fetch-keyword :span x) (text-from-tag x))))
                   (cdr tagsxp))))))

(defun link-text (link)
  (text-from-tag link))

(defun link-href (link)
  (fetch-keyword :href (fetch-keyword :a link)))



(defun discard-link-p (link &optional (host *url*))
  (some (alexandria:curry #'discardable-link-class (domain-keyword host))
        (split-sequence:split-sequence #\space (link-class link))))

(defun link-excerpt-exists-p (link)
  "Checks link existence in current document"
  (warflagger:find-excerpt-position *tdat* (link-text link)))

(defgeneric usable-link-class (domain linkclass)
  (:documentation "Domain specific tests for usable body links."))

(defmethod usable-link-class ((domain (eql :globalnews.ca)) linkclass)

  )

(defgeneric discardable-link-class (domain linkclass))

(defmethod discardable-link-class ((domain (eql :globalnews.ca)) linkclass)
  (member
   linkclass
   '("c-logoCorus"
     "l-footer__copyrightLink" "c-logo--black" "c-headingLink__link" "c-posts__inner" "c-tags__link"
     "c-iconLink--error" "c-iconLink" "l-main__feedbackLink" "is-hidden"
     "c-byline__link" "c-topLabel__link" "l-notification__headline"
     "l-notification__link" "l-notification__video" "l-navbar__link" "l-navbar__labelLink"
     "c-link--xlarge" "c-social__link" "c-link--large" "c-link"
     "c-socialShare__link"
     "l-navbar__logoSocial" "c-nav__link" "l-navbar__logo" "l-header__choices" "l-header__live"
     "l-header__button--light" "l-header__secondary" "c-button--small" "c-button" "l-header__button"
     "l-header__subscribe" "c-logo--header" "c-logo" "l-header__skip")
          :test #'equal))

'("c-logoCorus"
  "l-footer__copyrightLink" "c-logo--black" "c-headingLink__link"
  "c-posts__inner" "c-tags__link" "c-iconLink--error" "c-iconLink"
  "l-main__feedbackLink" "is-hidden" "c-readmore__link" "c-details__link"
  "c-byline__link" "c-topLabel__link" "l-notification__headline"
  "l-notification__link" "l-notification__video" "l-navbar__link"
  "l-navbar__labelLink" "c-link--xlarge" "c-social__link" "c-link--large"
  "c-link" "" "c-socialShare__link" "l-navbar__logoSocial" "c-nav__link"
  "l-navbar__logo" "l-header__choices" "l-header__live"
  "l-header__button--light" "l-header__secondary" "c-button--small" "c-button"
  "l-header__button" "l-header__subscribe" "c-logo--header" "c-logo"
  "l-header__skip")

(defun gather-links-by-class (links)
  (hu:collecting-hash-table (:test #'equal :mode :append)
    (dolist (link links)
      (dolist (class (split-sequence:split-sequence #\space (link-class link)))
        (hu:collect
            class
          (list (link-text link) (link-href link)))))))

(defun link-postables (link)
  (list (link-href link) (link-text link)))

