(in-package :wf/web)

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
    #|(html-thing-lister:render-list-for-sidebar
     (list :lister-type :thing :thing 'target)
     :label "Active targets:"
     :class "featurebox"
     :summary-width 40
    :pagequantity 20)|#
    (grouped-page)
    ))

(define-ps-lib wf-web-library ()
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
              (getprop (lisp (ps-gadgets:as-ps-data
                              (hu:plist->alist warflagger:*warstats-path-types*)))
                       type)))

    (defun make-rootid-url (rid)
      (strcat "/target/" (chain rid (to-string))))

    (defun make-opinionid-url (opid)
      (strcat "/opinion-page/" (chain opid (to-string))))

    (defun make-missing-rootid-url (url)
      (strcat "/target/?newurl=" (encode-u-r-i-component url)))

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

    ;;
    ;;Utilies taken from opinion-form
    ;;

    ;;FIXME: Duplicate of lisp functions in excerpt.lisp
      ;;Would be nice to only implement once.
      (defun create-textdata (text)
        (let ((res (create :text text :whitespace (create)))
              (found nil))
          (dotimes (i (length text))
            (when (member (elt text i) *whitespace-characters*)
              (unless found (setf found i))
              (dolist (j (range found (1+ i)))
                (incf (getprop res 'whitespace j)))
              (setf found nil)))
          res))

      (defun contiguous-whitespace? (tdat index)
        (or (getprop tdat 'whitespace index) 0))

      (defun excerpt-here? (tdat excerpt index)
        (let ((exdat (create-textdata excerpt))
              (text (@ tdat text)))
          (loop with tind = index
             with eind = 0
             with tlen = (length text)
             with elen = (length excerpt)
             do (progn
                  (when (eq elen eind) (return-from excerpt-here? tind))
                  (when (eq tlen tind) (return-from excerpt-here? nil))
                  (let ((ewhite (contiguous-whitespace? exdat eind))
                        (twhite (contiguous-whitespace? tdat tind)))
                    (if (and (eq 0 ewhite) (eq 0 twhite)
                             (eq (elt excerpt eind) (elt text tind)))
                        (progn (incf tind) (incf eind))
                        (if (or (eq 0 ewhite) (eq 0 twhite))
                            (return-from excerpt-here? nil)
                            (progn (incf tind twhite)
                                   (incf eind ewhite)))))))))

      (defun find-excerpt-position (tdat excerpt &optional (offset 0))
        (dotimes (i (length (@ tdat text)))
          (let ((loc (excerpt-here? tdat excerpt i)))
            (when loc
              (if (< 0 offset)
                  (decf offset)
                  (return (list i (- loc i))))))))

      ;;End of duplicate functions

      (defun clean-string-for-excerpt (the-string)
        (collecting-string
          (let ((last-was-white nil))
            (dotimes (i (length the-string))
              (if (member (elt the-string i) *whitespace-characters*)
                  (unless last-was-white
                    (setf last-was-white t)
                    (collect #\ ))
                  (progn
                    (setf last-was-white nil)
                    (collect (elt the-string i))))))))

      (defun calculate-offset (tdat excerpt startloc)
        (if (not-empty excerpt)
            (let ((res 0))
              (dotimes (i startloc)
                (when (excerpt-here? tdat excerpt i)
                  (incf res)))
              res)
            nil))

      (defun get-location-excerpt (tdat start end)
        (let* ((excerpt (chain tdat text (slice start end)))
               (excerpt (clean-string-for-excerpt excerpt))
               (offset (calculate-offset tdat excerpt start)))
          (list excerpt offset)))

      (defun find-excerpt-start/end (tdat excerpt &optional (offset 0))
        (let ((pos (find-excerpt-position tdat excerpt offset)))
          (when pos
            (list (elt pos 0) (+ (elt pos 0) (elt pos 1))))))

      ;;
      ;; End opinion-form utilities
      ;;

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

;;Not the greatest thing to have on the live server, but we want to have the sign up code tested.
(defun cleanup-test-user ()
  (when wf/local-settings:*test-user-name*
    (when-let ((user (get-user-by-screenname wf/local-settings:*test-user-name*)))
      (userfig:remove-user user))))
