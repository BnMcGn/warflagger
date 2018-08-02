(in-package :warflagger)

;;FIXME: duplicated in javascript: components.lisp. Update in both places.
(defun make-id-path (id)
  "Will put 1000 per dir for now"
  (if (< id 1000)
      "/0000/"
      (strcat "/"
              (let ((strid (princ-to-string id)))
                (subseq strid 0 (- (length strid) 3)))
              "000/")))

(defparameter *warstats-path-types*
  '(:badge "opinion-badge.svg"
    :opinion "opinion-data.json"
    :tree "tree.json"
    :warstats "warstats.json"
    :references "references.json"
    :questions "questions.json"
    :opinions "opinions.json"
    :author "author.json"
    :text "page.txt"))

(defun make-subpath (id type)
  (format nil "~a~a/~a"
          (make-id-path id) id (getf *warstats-path-types* type)))

(defun make-warstats-path (id type)
  (strcat wf/local-settings:*warstats-path* (make-subpath id type)))

(defun make-warstats-url (id type)
  (strcat "/static/warstats" (make-subpath id type)))



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


;;;;;;;;;;;;;;;;;;;;;;;
; SVG badge generator
;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *ring-cx* "21")
(defparameter *ring-cy* "21")
(defparameter *ring-r* "15.91549430918954")

;; https://stackoverflow.com/questions/46010186/mixed-case-tag-names-in-cl-who/
;; has a more generic solution


(eval-always
  (defmethod cl-who:convert-tag-to-string-list ((tag (eql :radialgradient))
                                         attr-list body body-fn)
    (nconc (cons "<radialGradient"
                 (let ((cl-who:*downcase-tokens-p* nil))
                   (cl-who:convert-attributes (recasify-attributes attr-list))))
           (list ">")
           (funcall body-fn body)
           (list "</radialGradient>")))

  (defmethod cl-who:convert-tag-to-string-list ((tag (eql :svg)) attr-list body body-fn)
    (nconc (cons "<svg"
                 (let ((cl-who:*downcase-tokens-p* nil))
                   (cl-who:convert-attributes (recasify-attributes attr-list))))
           (list ">")
           (funcall body-fn body)
           (list "</svg>")))

  (defun recasify-attributes (attr-list)
    "Get around cl-who trying to upcase everything or nothing."
    (mapcar
     (lambda (attr)
       (cons
        (if (find-if #'lower-case-p (symbol-name (car attr)))
            (string (car attr))
            (string-downcase (string (car attr))))
        (cdr attr)))
     attr-list)))

(defun draw-gradients (color)
  (html-out
    (:defs
        (:radialGradient
         :id "grad1"
         :fx "50%" :fy "30%" :r "1"
         (:stop :offset "0%" :stop-color color)
         (:stop
          :offset "100%"
          :stop-color (simple-rgb:xmlify-rgb
                       (simple-rgb:darken-rgb
                        (simple-rgb::parse color) :alpha 0.75))))
        (:radialGradient
         :id "grad2" :cx "21" :cy "10.5"
         :fx "21" :fy "10.5" :r "5.08"
         :|gradientTransform| "matrix(1.66,0.04,-0.03,1.25, -15.43,-0.099)"
         :|gradientUnits| "userSpaceOnUse"
         (:stop :offset "0%" :stop-color "#fff" :stop-opacity "1")
         (:stop :offset "100%" :stop-color "#fff" :stop-opacity "0")))))

(defun draw-ring (hole-func content-func)
  (html-out
    (:svg
     :xmlns "http://www.w3.org/2000/svg"
     :width "25" :height "25" :|viewBox| "0 0 42 42" :class "donut"
     (funcall hole-func)
     (:circle :class "donut-ring" :cx *ring-cx* :cy *ring-cy* :r *ring-r*
              :fill "none" :stroke "#525354" :stroke-width "2")
     (funcall content-func))))

(defun draw-segment (color offset length width)
  (html-out
    (:circle :class "donut-segment" :cx *ring-cx* :cy *ring-cy* :r *ring-r*
             :fill "none" :stroke color
             :stroke-dasharray
             (format nil "~a, ~a" (float length) (float (- 100 length)))
             :stroke-dashoffset (princ-to-string (float (+ 25 (- 100 offset))))
             :stroke-width (princ-to-string (float width)))))

(defun draw-hole (color)
  (html-out
    (draw-gradients color)
    (:circle :class "donut-hole" :cx *ring-cx* :cy *ring-cy* :r *ring-r*
             :fill "url(#grad1)")
    ;;spectral highlight
    (:ellipse :fill "url(#grad2)" :style "opacity:0.8;" :fill-opacity "1"
              :cx "19.5" :cy "14.0" :rx "8.68" :ry "8.36")))

(defun intervals (num)
  (unless (zerop num)
    (let ((len (/ 100 num)))
      (gadgets:collecting
          (dotimes (i num)
            (gadgets:collect (cons (* i len) len)))))))

(defun calculate-direction-from-axis (opinion warstats)
  (if
   (< (getf warstats :effect) (getf warstats :controversy))
   :contested
   (three-way (assoc-cdr :votevalue opinion) :negative :neutral :positive)))

(defun draw-opinion-badge (opinion reply-tree warstats)
  (let* ((this-stats (gethash (assoc-cdr :id opinion) warstats))
         (reply-count (getf this-stats :replies-immediate))
         (intervals (intervals reply-count))
         (reply-ids (mapcar #'car reply-tree))
         (max-effect (when reply-ids
                       (apply #'max (mapcar (lambda (id)
                                              (getf (gethash id warstats)
                                                    :effect))
                                            reply-ids))))
         (min-effect (when reply-ids
                       (apply #'min (mapcar (lambda (id)
                                              (getf (gethash id warstats)
                                                    :effect))
                                            reply-ids))))
         (max-controv (when reply-ids
                        (apply #'max (mapcar (lambda (id)
                                               (getf (gethash id warstats)
                                                     :controversy))
                                             reply-ids))))
         (min-controv (when reply-ids
                        (apply #'min (mapcar (lambda (id)
                                               (getf (gethash id warstats)
                                                     :controversy))
                                             reply-ids)))))
    ;;FIXME: These values are only calculated on opins that have replies. Some
    ;; Opins have more effect even if they don't have replies. Effect calculation
    ;; mechanism may need some rethinking.
    (labels ((scale-effect (num)
               (if (< min-effect max-effect)
                   (as-in-range 3 10
                                (relative-to-range min-effect max-effect num))
                   4))
             (scale-controv (num)
               (if (< min-controv max-controv)
                   (as-in-range 3 10
                                (relative-to-range min-controv max-controv num))
                   4)))
      (draw-ring
       (lambda ()
         (draw-hole
          (getf *flag-colors* (second (assoc-cdr :flag opinion)) "#fff")))
       (lambda ()
         (loop
            for (offset . length) in intervals
            for id in reply-ids
            for opinion = (opinion-by-id id)
            for warstat = (gethash id warstats)
            for direction = (calculate-direction-from-axis opinion warstat)
            do (draw-segment (getf *direction-colors* direction)
                             offset length
                             (if (eq direction :contested)
                                 (scale-controv (getf warstat :controversy))
                                 (scale-effect (getf warstat :effect))))))))))

(defun create-badges-for-rootid (rootid)
  (let* ((rooturl (get-rooturl-by-id rootid))
         (main-tree (opinion-tree-for-rooturl rooturl))
         (warstats (generate-rooturl-warstats rooturl :tree main-tree)))
    (labels ((proc (tree)
               (dolist (node tree)
                 (let ((fname (make-warstats-path (car node) :badge)))
                   (uiop/common-lisp:ensure-directories-exist fname)
                   (when (probe-file fname)
                     (delete-file fname))
                   (write-html-file
                       fname
                     (draw-opinion-badge (opinion-by-id (car node))
                                         (cdr node)
                                         warstats)))
                 (when (cdr node)
                   (proc (cdr node))))))
      (proc main-tree))))

(defun redraw-all-badges ()
  (dolist (id (sql-stuff:get-column 'rooturl 'id))
    (create-badges-for-rootid id)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Gather the opinion data
;;;;;;;;;;;;;;;;;;;;;;;;

(defun %opinion-data (opid text)
  (let ((data (opinion-by-id opid)))
    (if (assoc-cdr :excerpt data)
        (cons
         (list*
          :text-position
          (multiple-value-list
           (find-excerpt-position text (assoc-cdr :excerpt data)
                                  (or (assoc-cdr :excerptoffset data) 0))))
         data)
        data)))

(defun %fill-out-opinion-tree (tree text)
  (if (null tree)
      nil
      (let ((op (%opinion-data (caar tree) text)))
        (cons
         (cons
          op
          (%fill-out-opinion-tree (cdar tree)
                                  (create-textdata
                                   (aif (assoc :comment op) (cdr it) ""))))
         (%fill-out-opinion-tree (cdr tree) text)))))

(defun %warstats-pathdata-for-url (url)
  (multiple-value-bind (rid rtype)
      (get-target-id-from-url url)
    (when rid
      (values
       (list (case rtype
               (:rooturl rid)
               (:opinion (grab-one (liql rid 'opinion 'opinion.rooturl)))
               (otherwise (error "Unknown type")))
             :warstats)
       rid rtype))))

(defun %tidy-loaded-json (json)
  (hu:alist->hash
   (mapcar (lambda (x)
             (cons
              (let ((sym (string-unless-number (mkstr (car x)))))
                (if (numberp sym) sym (car x)))
              (hu:alist->plist (cdr x))))
           json)))

(defun request-warstats-for-url (url &optional (cache (make-hash-table)))
  "First check the in memory cache for warstats, then check the disk storage, then create minimal default stats. Cache is of the format generated by reference-list-for-rooturl. Returned warstats are just for the requested entity, not for the whole tree."
  (or
   (cl-hash-util:hget cache (list url :warstats))
   (multiple-value-bind (spec rid rtype) (%warstats-pathdata-for-url url)
     (when spec
       (let ((path (apply #'make-warstats-path spec)))
         (when (probe-file path)
           (with-open-file (fh path)
             (gethash (if (eq rtype :rooturl) :root rid)
                      (%tidy-loaded-json (json:decode-json fh))))))))
   ;;Don't need a full warstats dict to seed the tree.
   (list :effect 1 :controversy 0)))

;;FIXME: Don't yet have system in place for discussing headlines.
(defun get-headline-for-url (url)
  (multiple-value-bind (id type) (get-target-id-from-url url)
    (declare (ignore id))
    (collecting-hash-table (:mode :replace)
      (cond
        ((eq type :opinion)
         ;;FIXME: Could make default title from start of comment
         nil)
        ((eq type :rooturl)
         (when-let ((title (and (is-cached url)
                                (grab-title url :alternate nil :update nil))))
           (hu:collect :natural-title title)
           (hu:collect :title title)))
         ;;FIXME: Don't yet know what to do if reference isn't a target.
        (t nil)))))

(defun reference-list-for-rooturl (rooturl)
  (collecting-hash-table (:mode :replace)
    (let ((tree (opinion-tree-for-rooturl rooturl)))
      (labels ((proc (tree location)
                 (dolist (node tree)
                   (when (grab-column (liql (car node) 'reference.opinion))
                     (let* ((refopin (opinion-by-id (car node)))
                            (refurl (assoc-cdr :reference refopin)))
                       (hu:collect (assoc-cdr :id refopin)
                         (hu:plist->hash
                          (list
                           :reference refurl
                           :reference-domain (nth-value 2 (quri:parse-uri refurl))
                           :warflagger-link (make-wf-url-for-url refurl)
                           :tree-address (nreverse (cons (car node) location))
                           :refbot (system-generated-p (car node))
                           :refopinid (assoc-cdr :id refopin)
                           :refopinurl (assoc-cdr :url refopin)
                           :warstats (request-warstats-for-url refurl)
                           :warstats-src-url
                           (when-let ((spec (%warstats-pathdata-for-url refurl)))
                             (strcat wf/local-settings:*base-url*
                                     (apply #'make-warstats-url spec)))
                           :headline (get-headline-for-url refurl))))))
                   (when (cdr node)
                     (proc (cdr node) (cons (car node) location))))))
        (proc tree nil)))))

(defun question-opinion-p (opinid)
  (member (grab-one (liql opinid 'opinion 'opinion.flag))
          '("Negative RaiseQuestion" "Negative NeedsReference")))

(defun question-list-for-rooturl (rooturl warstats)
  (collecting
    (let ((tree (opinion-tree-for-rooturl rooturl)))
      (labels ((proc (tree location)
                 (dolist (node tree)
                   (when (question-opinion-p (car node))
                     (let ((opin (opinion-by-id (car node))))
                       (collect
                         (hu:plist->hash
                          (list
                           :tree-address (nreverse (cons (car node) location))
                           :questopinid (assoc-cdr :id opin)
                           :questopinurl (assoc-cdr :url opin)
                           :warstats (gethash (car node) warstats)))))))
                   (when (cdr tree)
                     (proc (cdr tree) (cons (car tree) location)))))
        (proc tree nil)))))

(defun %prep-for-json (warstats)
  (collecting-hash-table (:mode :replace)
    (gadgets:do-hash-table (k v warstats)
      (hu:collect k (hu:plist->hash v)))))

(defun %prep-references-for-json (references)
  ;;WARNING: Modifies the references table
  (do-hash-table (k ref references)
    (setf (gethash :warstats ref)
          (hu:plist->hash (gethash :warstats ref))))
  references)

(defun write-individual-references (references)
  (do-hash-table (id ref references)
    (let ((opinion (opinion-by-id (gethash :refopinid ref)))
          (statpath (make-warstats-path (gethash :refopinid ref) :opinion)))
      (setf (gethash :opinion ref)
            (hu:alist->hash opinion))
      (uiop/common-lisp:ensure-directories-exist statpath)
      (with-open-file (fh statpath
                         :direction :output :if-exists :supersede
                         :if-does-not-exist :create)
       (json:encode-json ref)))))

(defun write-all-rootid-warstats (rootid)
  (let* ((url (get-rooturl-by-id rootid))
         (text (progn
                 (unless (is-cached url)
                   (error "Don't have that page!"))
                 ;;FIXME: Suboptimal place, but somebody has to do it...
                 (make-rooturl-real rootid)
                 (grab-text url)))
         (references (reference-list-for-rooturl url))
         (url-keyed-references
          (collecting-hash-table (:mode :replace :test #'equal)
            (dolist (ref (hash-table-values references))
              (hu:collect (gethash :reference ref) ref))))
         (warstats
          (generate-rooturl-warstats url :reference-cache url-keyed-references)))
    (uiop/common-lisp:ensure-directories-exist (make-warstats-path rootid :opinions))
    (with-open-file (fh (make-warstats-path rootid :opinions)
                        :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
      (json:encode-json (%fill-out-opinion-tree
                         (opinion-tree-for-rooturl url) (create-textdata text))
                        fh))
    (with-open-file (fh (make-warstats-path rootid :warstats)
                        :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
      (json:encode-json (%prep-for-json warstats) fh))
    (with-open-file (fh (make-warstats-path rootid :references)
                        :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
      (%prep-references-for-json references) ;;WARNING: modifies!
      (json:encode-json references fh))
    (write-individual-references references) ;;WARNING: also modifies!
    (with-open-file (fh (make-warstats-path rootid :questions)
                        :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
      (json:encode-json (question-list-for-rooturl url warstats) fh))
    (with-open-file (fh (make-warstats-path rootid :text)
                        :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
      (write-string text fh))
    t))

(defun write-all-warstats ()
  (let ((rurls (all-rooturls))
        (updating nil))
    (dolist (rurl rurls)
      (unless (is-cached rurl)
        (update-page rurl)
        (setf updating t)))
    (when updating
      (sleep 2))
    (dolist (rurl rurls)
      (if (is-cached rurl)
          (write-all-rootid-warstats (get-rooturl-id rurl))
          (format
           t "Didn't process url: ~a~%Reason: ~a" rurl (grab-failed-message rurl))))))
