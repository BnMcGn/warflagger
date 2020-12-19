(in-package :wf/web)

(defvar *app* (make-instance 'ningle:<app>))

(defun large-logo ()
  (html-out
    (:img :src "/static/img/wf_logo_large.png"
          :alt "[WarFlagger: Because someone is wrong on the internet]"
          :style "display: block; margin-left: auto; margin-right: auto; margin-top: 3em; margin-bottom: 3em;")))

(defun blank-warstat-for-web ()
  (let* ((wstat (warflagger:generate-rooturl-warstats ""))
         (root (hu:plist->hash (gethash :root wstat))))
    (setf (gethash :effect root) 0)
    (setf (gethash :root wstat) root)
    wstat))

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
       (grouped-page)
    ))

(define-ps-lib wf-web-library ()
  (ps

    ;;;Paths and urls

    ;; Also in lisp
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
      (strcat "/new-target/?url=" (encode-u-r-i-component url)))

    (defun excerpt-reply-link (url excerpt)
      (let ((exstr (when excerpt (strcat "&excerpt=" (encode-u-r-i-component excerpt)))))
        (strcat
         "/opinion/?target="
         (encode-u-r-i-component url)
         exstr)))

    ;;; Tree tools

    ;;FIXME: is this same/similar to immediate-children-ids?
    (defun opinion-children (tree-address opinions)
      (let ((res (some (lambda (x)
                         (when (eql (getprop tree-address 0) (@ x 0 id)))
                         x)
                       opinions)))
        (if (< 1 (length tree-address))
            (opinion-children (array-cdr tree-address)
                              (array-cdr res))
            (array-cdr res))))

    ;;FIXME: votevalue might not remain
    (defun opinion-p (itm)
      (and (not (atom itm))
           (chain itm (has-own-property "votevalue"))))

    (defun focus-p (props?)
      (if (and (@ props? focus) (not-empty (@ props? focus)))
          (eql (list-last (@ props? tree-address) (list-last (@ props? focus))))
          t))

    (defun focus-parent-p (props?)
      (let ((fparent (getprop (chain (@ props? focus) (slice -2)) 0)))
        (when (eql (list-last (@ props? tree-address) fparent))
          (getprop (@ props? opinion-store) fparent)))
      (when
          (eql (list-last (@ props? tree-address)) fparent)))

    (defun immediate-children-ids (id opinstore)
      (if id
          (let ((len (@ (getprop opinstore id) 'tree-address length)))
            (collecting
              (dolist (itm (all-descendant-ids id opinstore))
                (when (eq (1+ len) (@ (getprop opinstore itm) 'tree-address length))
                  (collect itm)))))
          (collecting
            (do-keyvalue (k opin opinstore)
              (when (eq 1 (@ opin 'tree-address length))
                (collect (@ opin id)))))))

    (defun all-descendant-ids (id opinstore)
      (if id
          (let* ((trad (@ (getprop opinstore id) 'tree-address))
                 (len (@ trad length)))
            (collecting
              (do-keyvalue (k opin opinstore)
                (when (< len (@ opin 'tree-address length))
                  (when (array-equal trad (chain (@ opin 'tree-address) (slice 0 len)))
                    (collect (@ opin id)))))))
          (chain -object (keys opinstore))))

    ;;; Excerpt and text tools

    ;; Also in lisp
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

    ;; Also in lisp
    (defun contiguous-whitespace? (tdat index)
      (or (getprop tdat 'whitespace index) 0))

    ;; Also in lisp
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

    ;; Also in lisp
    (defun find-excerpt-position (tdat excerpt &optional (offset 0))
      (dotimes (i (length (@ tdat text)))
        (let ((loc (excerpt-here? tdat excerpt i)))
          (when loc
            (if (< 0 offset)
                (decf offset)
                (return (list i (- loc i))))))))

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

    (defun rebreak (text)
      (chain (collecting
               (dolist (string (chain text (split #\linefeed)))
                 (collect string)
                 (collect (psx (:br :key (unique-id))))))
             (slice 0 -1)))

    (defun %overlap-p (start1 end1 start2 end2)
      (not (or (> start1 end2) (> start2 end1))))

    ;;Find all the indices where excerpts start or stop.
    (defun excerpt-segment-points (opset end)
      "End is the length of the text."
      (chain
       (collecting-set
           (dolist (itm opset)
             (collect (@ itm text-position 0))
             (collect (+ (@ itm text-position 0) (@ itm text-position 1))))
         (collect 0)
         (collect (1+ end)))
       (sort (lambda (a b) (- a b)))))

    (defun has-excerpt-p (opin)
      (chain opin (has-own-property :excerpt)))

    (defun has-found-excerpt-p (opin)
      (and (has-excerpt-p opin)
           (not (equal null (@ opin 'text-position 0)))))

    ;;Misc tools
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
                                    (getprop warstats b 'effect))))))
        res))

    (let ((counter 0))
      (defun unique-id ()
        (incf counter)))

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
