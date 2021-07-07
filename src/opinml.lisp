(in-package #:warflagger)

;;;
;;; OpinML
;;;


;;; Typedefs

(defun list-of-type-p (list type)
  (and (listp list)
       (every (alexandria:rcurry #'typep type) list)))

(deftype list-of-type (type)
  (let ((predicate (gadgets:symbolize (gensym "%LIST-OF-TYPE-"))))
    (setf (symbol-function predicate)
          #'(lambda (seq) (list-of-type-p seq type)))
    `(and list (satisfies ,predicate))))

(defun iid-p (item)
  (and (stringp item)
       (eq 59 (length item))
       (ppcre:scan *ipfs-hash-pattern* item)
       item))

(deftype iid () `(satisfies iid-p))

(defun opinion-p (item)
  (and (every #'consp item)
       (assoc :flag item)
       (assoc :target item)
       (assoc :url item)))

(deftype opinion () `(satisfies opinion-p))

(defun extended-opinion-p (item)
  (and (opinion-p item)
       (assoc :tree-address item)))

(deftype extended-opinion () `(satisfies extended-opinion-p))

(defun opinion-with-iid-p (opinion)
  (and (opinion-p opinion)
       (assoc :iid opinion)
       (iid-p (assoc-cdr :iid opinion))))

(deftype opinion-with-iid () `(satisfies opinion-with-iid-p))

(deftype recognized-flag () `(satisfies recognized-flag-p))

(defun opinion-store-p (item)
  (and (hash-table-p item)
       (every #'opinion-p (alexandria:hash-table-values item))))

(deftype opinion-store () `(satisfies opinion-store-p))

(defun iid-tree-address-p (item)
  (list-of-type-p item 'iid))

(deftype iid-tree-address () `(satisfies iid-tree-address-p))

(defun iid-opinion-tree-p (item)
  (and (listp item)
       (every #'iid-p (flatten item))))

(deftype iid-opinion-tree () `(satisfies iid-opinion-tree-p))

(defun hashtag-p (item)
  (and (stringp item)
       (eql #\# (elt item 0))
       (< (length item) *max-hashtag-length*)
       ;;FIXME: Alphanumeric for now. Should consider unicode chars?
       (every #'alphanumericp
              (sequence->list (subseq item 1)))))

(deftype hashtag () `(satisfies hashtag-p))

(defun url-p (item)
  (handler-case
      (progn (quri:parse-uri item)
             item)
    (error (e)
      (declare (ignore e))
      nil)))

(deftype uri () `(satisfies url-p))

(defun reference-opinion-p (opinion)
  (and (opinion-p opinion)
       (or (when-let ((ref (assoc :reference opinion)))
             (cdr ref))
           (when-let ((refs (assoc :references opinion)))
             (cdr refs)))))

;;;;;;;;;;;;;;;;;;;;
;; Save and load
;;;;;;;;;;;;;;;;;;;;


(defmacro with-inverted-case (&body body)
  (let ((case (gensym "case")))
    `(let ((,case (readtable-case *readtable*)))
       (unwind-protect (progn
                         (setf (readtable-case *readtable*) :invert)
                         ,@body)
         (setf (readtable-case *readtable*) ,case)))))

(defun serialize-opinion (opinion &key author created extended)
  "Create a version of an opinion that is suitable for saving to disk or IPFS in s-expression format. If EXTENDED is not set, no id is included because it is assumed to be a first save."
  (with-inverted-case
    (strcat
     (format nil ";;OpinML 0.0.1 :s-expression~%")
     (prin1-to-string
      (hu:hash->plist
       ;;FIXME: keys should be ordered
       (hu:collecting-hash-table (:mode :replace)
         (hu:collect :target (assoc-cdr :target opinion))
         (hu:collect :rooturl (assoc-cdr :rooturl opinion))
         (hu:collect :flag (assoc-cdr :flag opinion))
         (hu:collect :comment (assoc-cdr :comment opinion))
         (hu:collect :author (or author (assoc-cdr :author opinion)))
         (when-let ((comment (assoc-cdr :comment opinion)))
           (hu:collect :comment comment))
         (when-let ((votevalue (assoc-cdr :votevalue opinion)))
           (hu:collect :votevalue votevalue))
         (when-let ((excerpt (assoc-cdr :excerpt opinion)))
           (hu:collect :excerpt excerpt))
         (when-let ((excerpt-offset (assoc-cdr :excerpt-offset opinion)))
           (hu:collect :excerpt-offset excerpt-offset))
         (when-let ((reference (assoc-cdr :reference opinion)))
           (hu:collect :reference reference))
         (hu:collect :created
           (js-compatible-utcstamp (or created (cdr (assoc-or '(:created :datestamp) opinion)))))
         (when extended
           (hu:collect :tree-address (assoc-cdr :tree-address opinion))
           (hu:collect :iid (assoc-cdr :iid opinion))
           (when-let ((text-position (assoc-cdr :text-position opinion)))
             (hu:collect :text-position text-position))
           (when-let ((authorname (assoc-cdr :authorname opinion)))
             (hu:collect :authorname authorname))
           (when-let ((leading (assoc-cdr :leading opinion)))
             (hu:collect :leading leading))
           (when-let ((trailing (assoc-cdr :trailing opinion)))
             (hu:collect :trailing trailing))
           (when-let ((hashtags (assoc-cdr :hashtags opinion)))
             (hu:collect :hashtags hashtags))
           (when-let ((clean-comment (assoc-cdr :clean-comment opinion)))
             (hu:collect :clean-comment clean-comment))
           (when-let ((references (assoc-cdr :references opinion)))
             (hu:collect :references references))
           (when-let ((directives (assoc-cdr :directives opinion)))
             (hu:collect :directives directives)))))))))

;; Problems: datestamp. Might want to deal with :id or :url because maybe is a remote web opinion.
;; - how to serialize to string?
;; - indentation might be nice. has someone done that?

(defun ipfs-data-hash (data)
  (declare (type string data))
  (assoc-cdr
      :*hash
      (cl-json:decode-json-from-string
       (nth-value
        1
        (uiop:with-temporary-file (:pathname p :direction :output :stream s)
          (write data :stream s)
          (close s)
          (ipfs:add p :pin nil :only-hash t :cid-version 1))))))

(defun save-opinion-to-folder (opinion folder)
  (let* ((author (unless (assoc-cdr :author opinion)
                   (make-author-url (assoc-cdr :author-id opinion))))
         (datestamp (unless (assoc-cdr :datestamp opinion)
                      (get-universal-time)))
         (strop (serialize-opinion opinion :author author :datestamp datestamp))
         (iid (ipfs-data-hash strop)))
    (alexandria:write-string-into-file strop (make-pathname :directory folder :name iid))))

(defparameter *max-comment-length* 10000) ;; Too long. Could go much closer to twitter.
(defparameter *max-excerpt-length* 500) ;; Also too long
(defparameter *max-hashtag-length* 30)

(defun check-url (url)
  (cond
    ((< 2000 (length url)) (error "URL too long"))
    ((url-p url) url)
    (t (error "Not an URL"))))

(defun check-length (itm len)
  (if (< len (length itm))
      (error "Field too long")
      itm))

(defun check-hashtag (hashtag)
  (unless (hashtag-p hashtag)
    (error "Invalid hashtag")))

(defparameter *safe-opinion-symbols*
  (append
   '(:target :rooturl :flag :comment :author :votevalue :reference :datestamp)
   (gadgets:ordered-unique (alexandria:flatten (warflagger:known-flags)))))

(defun safe-symbol-p (namestr package)
  (and (member namestr *safe-opinion-symbols* :test #'string-equal)
       (eq package :keyword)))

(defun deserialize-opinion-from-stream (stream)
  ;;FIXME: Add over all length limit
  (let ((opinion (proto:limited-reader stream #'safe-symbol-p)))
    (mapc (lambda (key) (check-url (getf opinion key))) '(:target :rooturl :author))
    (when (getf opinion :reference)
      (check-url (getf opinion :reference)))
    (check-length (getf opinion :comment) *max-comment-length*)
    (when (getf opinion :excerpt)
      (check-length (getf opinion :excerpt) *max-excerpt-length*))
    (setf (getf opinion :created) (local-time:parse-timestring (getf opinion :created)))
    ;;FIXME: datestamp is deprecated
    (push (getf opinion :created) opinion)
    (push :datestamp opinion)
    (hu:plist->alist opinion)))

(defun make-experimental-opinion-url (oiid)
  (strcat *base-url* "o/" oiid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FIXME: This is temporary code, until converted from db storage
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun proc-opinion (opinion newtarget)
  (when newtarget
    (push (cons :target newtarget) opinion))
  (unless (assoc-cdr :author opinion)
    (push (cons :author (make-author-url (assoc-cdr :author-id opinion))) opinion))
  (let ((iid (ipfs-data-hash (serialize-opinion opinion))))
    (push (cons :url (make-experimental-opinion-url iid)) opinion)
    (push (cons :iid iid) opinion)
    (values iid opinion)))

(defparameter *id-iid* (make-hash-table))

(defun old-opinion-url (url)
  (or (search "opinion-page" url)
      (search "things/thing/opinion" url)))

(defun new-url-for-old (opinurl)
  (unless (old-opinion-url opinurl)
    (error "Not an opinion url!"))
  (make-experimental-opinion-url
   (gethash (parse-integer (last-car (split-sequence:split-sequence #\/ opinurl))) *id-iid*)))

(defun replace-urls-in-comment (text)
  (let ((res nil))
    (dolist (url (find-urls text))
      (when (old-opinion-url url)
        (let* ((curr (or res text))
               (indx (search url curr))
               (new (new-url-for-old url)))
          (unless indx (error "Something broken: didn't find found url!"))
          (setf res (concatenate
                     'string
                     (subseq curr 0 indx)
                     new
                     (subseq curr (+ indx (length url))))))))
    res))

(defun set-opinion-target (id url)
  (format t "SetTarget: ~a ~a~&" id url)
  (update-record 'opinion id (list (cons (colm 'target) url))))

(defun set-opinion-reference (id url)
  (format t "SetReference: ~a ~a~&" id url)
  (update-record 'reference id (list (cons (colm 'reference) url))))

(defun set-opinion-url (id url)
  (format nil "SetTarget: ~a ~a~&" id url)
  (update-record 'opinion id (list (cons (colm 'url) url))))

(defun set-opinion-comment (id comment)
  (format t "SetComment: ~a ~a~&" id comment)
  (update-record 'comment id (list (cons (colm 'comment) comment))))

(defun set-opinion-iid (id iid)
  (format nil "SetTarget: ~a ~a~&" id iid)
  (update-record 'opinion id (list (cons (colm 'iid) iid))))

(defun proc-db-opinion (opinion)
  (let ((id (assoc-cdr :id opinion)))
    (unless (equal (assoc-cdr :target opinion) (assoc-cdr :rooturl opinion))
      (when (old-opinion-url (assoc-cdr :target opinion))
        (let ((newtarg (new-url-for-old (assoc-cdr :target opinion))))
         (push (cons :target newtarg) opinion)
         (set-opinion-target id newtarg))))
    (unless (assoc-cdr :author opinion)
      (push (cons :author (make-author-url (assoc-cdr :author-id opinion))) opinion))
    (when-let* ((reference (assoc-cdr :reference opinion))
                (newref (and (old-opinion-url reference) (new-url-for-old reference))))
      (set-opinion-reference id newref)
      (push (cons :reference newref) opinion))
    (when-let* ((comment (assoc-cdr :comment opinion))
                (comment (replace-urls-in-comment comment)))
      (push (cons :comment comment) opinion)
      (set-opinion-comment id comment))
    (let ((iid (ipfs-data-hash (serialize-opinion opinion))))
      (set-opinion-url id (make-experimental-opinion-url iid))
      (set-opinion-iid id iid)
      (setf (gethash id *id-iid*) iid))))

(defun proc-opinions-in-db ()
  (let ((*id-iid* (make-hash-table)))
    (dolist (opid (get-column 'opinion 'id :order-by 'id))
      (proc-db-opinion (opinion-by-id opid)))))

(defun convert-opinions-to-iids (opinions)
  ;; need way to make target urls. Don't know if make-opinion-url is used anywhere
  (let ((oldnew (make-hash-table :test 'equal))
        (work opinions)
        (stor nil)
        (stop 0)
        (remaining nil))
    (loop
      do (progn
           (incf stop)
           (when remaining
             (format t "~a ~a" (length work) (length remaining))
             (setf work remaining) (setf remaining nil))
           (dolist (op work)
             (cond
               ((equal (assoc-cdr :target op) (assoc-cdr :rooturl op))
                (multiple-value-bind (iid opinion) (proc-opinion op nil)
                  (setf (gethash (assoc-cdr :url op) oldnew) (make-experimental-opinion-url iid))
                  (push opinion stor)))
                ((gethash (assoc-cdr :target op) oldnew)
                 (multiple-value-bind (iid opinion)
                     (proc-opinion op (gethash (assoc-cdr :target op) oldnew))
                   (setf (gethash (assoc-cdr :url op) oldnew) (make-experimental-opinion-url iid))
                   (push opinion stor)))
                ((not (gethash (assoc-cdr :target op) oldnew))
                 (push op remaining)))))
        until (> stop 1000)
      while remaining)
    (values (nreverse stor) oldnew)))

(defun iid-opinions-for-rooturl (rooturl)
  (convert-opinions-to-iids (mapcar #'opinion-by-id (opinion-ids-for-rooturl rooturl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end temporary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *ipfs-hash-pattern* (ppcre:create-scanner "baf[a-z0-9]{56}"))
(defun get-ipfs-hash-from-url (string)
  (ppcre:scan-to-strings *ipfs-hash-pattern* string))

;;FIXME: Just returns canonical text. Reconsider when we have edited available.
(defun opinion-text (opin)
  (let* ((opin (if (typep opin 'iid) (opinion-by-id opin) opin))
         (res (gadgets:assoc-or '(:clean-comment :comment) opin)))
    (when res
      (cdr res))))

;;; Comment parser stuff
(defun handle-hash (input)
  (let ((curr (funcall input)))
    (if (sequence-starts-with curr "#(")
        (handle-directive input)

        ;;FIXME: Should hashtags be alphanumeric only?
        (if (or (null *last-char*) (member *last-char* *whitespace-characters*))
            (let* ((ind (first-match-index (lambda (x) (member x *whitespace-characters*))
                                           (sequence->list curr)))
                   (tag (if ind (subseq curr 0 ind) curr)))
              (push tag *found-hashtags*)
              (if ind
                  (funcall input ind)
                  (funcall input (length curr)))
              tag)
            (progn
              (funcall input 1)
              (elt curr 0))))))

;;FIXME: doesn't allow nested parentheses. Might need some day.
;;FIXME: doesn't have an escape mechanism
(defun handle-directive (input)
  (let* ((curr (funcall input))
         (ind (position #\) curr))
         (correct-start (member *last-char* '(#\Newline nil)))
         (correct-end (and correct-start ind
                           (or (eq (length curr) (1+ ind))
                               (eq #\Newline (elt curr ind)))))
         (directive (and ind correct-start correct-end
                         (parse-directive
                          (string-trim *whitespace-characters* (subseq curr 0 (1+ ind)))))))
    (if directive
        (progn
          (funcall input (1+ ind))
          (push directive *found-directives*)
          "")
        (progn
          (funcall input 1)
          (elt curr 0)))))

(defparameter *md-link-pattern* (ppcre:create-scanner "^\\[(.+)\\]\\((.+?)\\)"))

(defun handle-square-bracket (input)
  (let ((curr (funcall input)))
    (multiple-value-bind (start end starts ends) (ppcre:scan *md-link-pattern* curr)
      (let ((text (subseq curr (elt starts 0) (elt ends 0)))
            (link (subseq curr (elt starts 1) (elt ends 1))))
        (if (or (null start)
                (not (url-p link))
                (not (eq start 0)))
            ;;Fail!
            (progn (funcall input 1) #\[)
            (progn
              (push (list text link) *found-references*)
              (funcall input end)
              ;;Not sure that this is right: Front end will need to reparse.
              (subseq curr start end)))))))

(defparameter *found-hashtags* nil)
(defparameter *found-directives* nil)
(defparameter *found-references* nil)
(defparameter *last-char* nil)
(defparameter *comment-read-table*
  (hu:hash (#\[ #'handle-square-bracket)
           (#\# #'handle-hash)))

(defun find-last-char (stor)
  (when stor
    (typecase (car stor)
      (standard-char (car stor))
      (sequence (if (not-empty (car stor))
                    (nelt (car stor) 0)
                    (find-last-char (cdr stor)))))))

(defun parse-comment-field (comment)
 "
- #() if alone on line. Should we strictly stay at start of line?
- #tag whitespace before and after
- markdown style links
- no other markdown for now. Or...
- should return structure with:
  - raw text.
  - clean text
  - hashtags found
  - refs found
  - directives found
  - indicate if only directives were found "
  (let* ((counter (dos-to-unix comment))
         (counter 0)
         (tracker (lambda (&optional (advance 0))
                    (prog1 (subseq comment counter)
                      (incf counter advance))))
         (*last-char* nil)
         (*found-hashtags* nil)
         (*found-directives* nil)
         (*found-references* nil)
         (stor nil))
    (loop
      do
         (if-let (handler (gethash (elt (funcall tracker) 0) *comment-read-table*))
           (push (funcall handler tracker) stor)
           ;;Default handler
           (push (elt (funcall tracker 1) 0) stor))
      do (setf *last-char* (find-last-char stor))
      while (not-empty (funcall tracker)))
    (hash-table-alist
     (hu:hash
      (:clean-comment
       (let ((res (apply #'strcat (nreverse stor))))
         ;;FIXME: This doesn't handle comments that consist only of hashtags
         ;;Nil if comment has nothing remaining but whitespace
         (unless (every (alexandria:rcurry #'member *whitespace-characters*) res)
           res)))
      (:hashtags (ordered-unique (nreverse *found-hashtags*)))
      (:references (nreverse *found-references*))
      (:directives (ordered-unique (nreverse *found-directives*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directives
;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun parse-directive (dstring)
  (let ((data (safe-read:safe-read (make-string-input-stream (string-trim '(#\#) dstring)))))
    (when-let ((sym (first-match (curry #'string-equal (car data)) *known-directives*)))
      ;;FIXME: Should send warnings to user on fail.
      (tryit (apply (symbol-function sym) (cdr data))))))

(defparameter *known-directives* '(vote-value target-text target-title no-cascade suggest-target-text
                                   suggest-target-title))

;;Note: these functions don't do anything to interpret the directives. They are just here as a parsing
;; aid.
(defun vote-value (value)
  (declare (type integer value))
  (when (< -2 value 2)
    (list 'vote-value value)))

(defun target-text ()
  (list 'target-text))

(defun target-title ()
  (list 'target-title))

(defun suggest-target-text ()
  (list 'suggest-target-text))

(defun suggest-target-title ()
  (list 'suggest-target-title))

(defun no-cascade ()
  (list 'no-cascade))
