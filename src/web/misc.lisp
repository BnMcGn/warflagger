(in-package :wf/web)


(defmethod json:encode-json ((object clsql-sys:wall-time) &optional stream)
  (write-char #\" stream)
  (clsql:format-time stream object :format :iso8601)
  ;;FIXME: quick hack to make this work on the live server (GMT). Javascript is touchy
  ;; about parsing dates.
  (write-string "+0000" stream)
  (write-char #\" stream))

(defmethod json:encode-json ((object local-time:timestamp) &optional stream)
  (write-char #\" stream)
  ;; FIXME: verify correct. Who uses local-time?
  (local-time:format-timestring
   stream object
   :format '((:year 4) #\- (:month 2) #\- (:day 2) #\T
             (:hour 2) #\: (:min 2) #\: (:sec 2) :gmt-offset-hhmm))
  (write-char #\" stream))

(defun large-logo ()
  (html-out
    (:img :src "/static/img/wf_logo_large.png"
          :alt "[WarFlagger: Because someone is wrong on the internet]"
          :style "display: block; margin-left: auto; margin-right: auto; margin-top: 3em; margin-bottom: 3em;")))

(define-parts main-page-parts
  :@css-link "/static/css/push_button.css"
  :@notifications
  (let ((info (warflagger-user-info-bundle)))
    (lambda ()
      (unless (signed-up?)
        (html-out
          (:div :class "featurebox_side"
                (:h3 "Account")
                (:a :href (assoc-cdr :login-url info)
                    :class "push_button pb_green" "Sign Up")
                (:a :href (assoc-cdr :login-url info) "Log In"))))))
  :@inner
  (lambda ()
    (large-logo)
    (unless (warflagger:get-local-user-id (get-user-name))
      ;;Local user won't have an entry in DB if hasn't posted yet.
      (html-out
        ;;FIXME: find a proper way to style/display notifications
        (:div :style "margin: 4px; background-color: lightgreen; text-align: center"
         "You don't seem to have posted on WarFlagger yet."
         (:a :href "/introduction/" "Click here")
         " for a guide to posting.")))
    (html-thing-lister:render-list-for-sidebar
     (list :lister-type :thing :thing 'target)
     :label "Active targets:"
     :class "featurebox"
     :summary-width 40
     :pagequantity 20)))

(defun wf-web-library ()
  (ps
    ;;FIXME: Duplicate of CL version
    (defun make-id-path (id)
      (if (< id 1000)
          "/0000/"
          (strcat "/" (chain id (to-string) (slice 0 -3)) "000/")))

    (defun make-warstats-url (id type)
      (strcat "/static/warstats"
              (make-id-path id)
              (chain id (to-string))
              "/"
              (getprop (lisp (ps-gadgets:alist->ps-object-code
                              (hu:plist->alist warflagger:*warstats-path-types*)))
               type)))

    (defun opinion-children (tree-address opinions)
      (let ((curr opinions))
        (dolist (id tree-address)
          (dotimes (i (@ curr length))
            (when (= id (getprop curr i 0 id))
              (setf curr (chain (@ curr i) (slice 1)))
              (break))))
        curr))

    (defun filter-opins-score (tree-addresses opinions warstats)
      (let ((res
             (collecting
                 (dolist (ta tree-addresses)
                   (let ((id (list-last ta)))
                     (when (< 2 (getprop warstats id 'effect))
                       (collect id)))))))
        (chain res
               (sort (lambda (a b) (- (getprop warstats a 'effect)
                                      (getprop warstats b 'effect)))))
        res))

    (defun filter-opins-controversial (tree-addresses opinions warstats)
      (let ((res
             (collecting
                 (dolist (ta tree-addresses)
                   (let ((id (list-last ta)))
                     (when (< 2 (getprop warstats id 'controversy))
                       (collect id)))))))
        (chain res
               (sort (lambda (a b) (- (getprop warstats a 'controversy)
                                      (getprop warstats b 'controversy)))))
        res))

    (defun filter-opins-question (tree-addresses opinions warstats)
      (let ((res
             (collecting
                 (dolist (ta tree-addresses)
                   (let* ((id (list-last ta))
                          (opin (getprop opinions id))
                          (stats (getprop warstats id)))
                     (when
                         ;;FIXME: This is a crude definition of a question. Reconsider
                         ;; if/when we implement "Accepted" flag. Also if directives
                         ;; are added.
                         (or (chain (list "needsEvidence" "raiseQuestion")
                                    (includes (@ opin flag 1)))
                             ;; Wrong axis is a stand in for being answered. For now.
                             (< 1 (@ stats x-wrong 0)))
                       (collect id)))))))
        (chain res
               (sort
                ;; Have arbitrarily decided to sort by combined controversy and effect
                (lambda (a b) (- (+ (getprop warstats a 'controversy)
                                    (getprop warstats a 'effect))
                                 (+ (getprop warstats b 'controversy)
                                    (getprop warstats b 'effect))))))))

    ))

(defun tracking-code ()
 "<!-- Start of StatCounter Code for Default Guide -->
<script type=\"text/javascript\">
var sc_project=11547060;
var sc_invisible=1;
var sc_security=\"13ae87ef\";
var scJsHost = ((\"https:\" == document.location.protocol) ?
\"https://secure.\" : \"http://www.\");
document.write(\"<sc\"+\"ript type='text/javascript' src='\" +
scJsHost+
\"statcounter.com/counter/counter.js'></\"+\"script>\");
</script>
<noscript><div class=\"statcounter\"><a title=\"Web Analytics\"
href=\"http://statcounter.com/\" target=\"_blank\"><img
class=\"statcounter\"
src=\"//c.statcounter.com/11547060/0/13ae87ef/1/\" alt=\"Web
Analytics\"></a></div></noscript>
<!-- End of StatCounter Code for Default Guide -->")
