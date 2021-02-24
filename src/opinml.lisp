(in-package #:warflagger)

;;;
;;; OpinML questions:
#|

- What do we need to generate opinml?
  - A filename/location. Could be under username. Beware of filesystem hacks!
  - An URL. Need to generate a unique url per opinion. Does not actually need to correspond to filename.
  - Can we use fragments? put multiple opins in a file? Why? keep simple.
  - Want to know if target is rooturl. Would be nice to store this in opinion.
   - at what point do we need this information?
   - interface should try to supply it.
   - might not be able to do much without.

|#

(defmacro with-inverted-case (&body body)
  (let ((case (gensym "case")))
    `(let ((,case (readtable-case *readtable*)))
       (unwind-protect (progn
                         (setf (readtable-case *readtable*) :invert)
                         ,@body)
         (setf (readtable-case *readtable*) ,case)))))

(defun serialize-opinion (opinion &key author datestamp)
  "Create a basic version of an opinion that is suitable for saving to disk or IPFS. No ID is included because this is assumed to be a first save."
  (with-inverted-case
    (prin1-to-string
     (hu:hash->plist
      ;;FIXME: keys should be ordered
      (hu:collecting-hash-table (:mode :replace)
        (hu:collect :target (assoc-cdr :target opinion))
        (hu:collect :rooturl (assoc-cdr :rooturl opinion))
        (hu:collect :flag (assoc-cdr :flag opinion))
        (hu:collect :comment (assoc-cdr :comment opinion))
        (hu:collect :author (or author (assoc-cdr :author opinion)))
        (when-let ((votevalue (assoc-cdr :votevalue opinion)))
          (hu:collect :votevalue votevalue))
        (when-let ((excerpt (assoc-cdr :excerpt opinion)))
          (hu:collect :excerpt excerpt))
        (when-let ((excerpt-offset (assoc-cdr :excerpt-offset opinion)))
          (hu:collect :excerpt-offset excerpt-offset))
        (when-let ((reference (assoc-cdr :reference opinion)))
          (hu:collect :reference reference))
        (hu:collect :datestamp
          (js-compatible-utcstamp (or datestamp (assoc-cdr :datestamp opinion)))))))))

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

;;; Comment parser stuff
(defun handle-hash (input)
  "Note: will often have a leading whitespace passed in"
  (let ((curr (funcall input)))
    (if (or (sequence-starts-with curr "\n#(") (sequence-starts-with curr "#("))
        (handle-directive input)
        ;;FIXME: Should hashtags be alphanumeric only?
        (let* ((ind (first-match-index (lambda (x) (member x *whitespace-characters*))
                                       (sequence->list (subseq curr 1))))
               (tag (string-trim *whitespace-characters*
                                 (if ind (subseq curr 0 ind)
                                     curr))))
          (push tag *found-hashtags*)
          (if ind
              (funcall input ind)
              (funcall input (length curr)))))))

;;FIXME: doesn't allow nested parentheses. Might need some day.
;;FIXME: doesn't have an escape mechanism
(defun handle-directive (input)
  (let* ((curr (funcall input))
         (ind (position #\) curr)))
    (if (and ind (or (eq (length curr) ind)
                     (eq #\Newline (elt curr (1+ ind)))))
        ;;we have a valid directive
        (progn
          (funcall input ind)
          (push (string-trim *whitespace-characters* (subseq curr 0 ind)) *found-directives*)
          ""))))

(defparameter *md-link-pattern* (ppcre:create-scanner "^\\[(.+)\\]\\((.+?)\\)"))

(defun handle-square-bracket (input)
  (let ((curr (funcall input)))
    (multiple-value-bind (start end text link) (ppcre:scan *md-link-pattern* curr)
      (if (or (null start)
              (not (url-p link))
              (not (eq start 0)))
          ;;Fail!
          (progn (funcall input 1) #\[)
          (progn
            (push (list text link) *found-references*)
            (funcall input end)
            ;;Not sure that this is right: Front end will need to reparse.
            (subseq curr start end))))))

(defun handle-whitespace (input)
  (let ((curr (funcall input)))
    (if (eq (elt curr 1) #\#)
        (handle-hash input)
        (funcall input 1))))

(defparameter *found-hashtags* nil)
(defparameter *found-directives* nil)
(defparameter *found-references* nil)
(defparameter *comment-read-table*
  (hu:hash (#\[ #'handle-square-bracket)
           (#\Newline #'handle-whitespace)
           (#\  #'handle-whitespace)
           (#\Tab #'handle-whitespace)))

(defun parse-comment-field (comment)
  ;;FIXME: convert all line endings to #\Newline
  (let* ((counter 0)
         (clen (length comment))
         (sol-last nil)
         (tracker (lambda (&optional (advance 0))
                    (prog1 (subseq comment counter)
                      (incf counter advance))))
         (*found-hashtags* nil)
         (*found-directives* nil)
         (*found-references* nil)
         (stor nil))
    ;;Special case: hash as first character
    (when (eq #\# (elt comment 0))
      (push (handle-hash tracker) stor))
    (loop
      do
         (if-let (handler (gethash (elt (funcall tracker) 0) *comment-read-table*))
           (push (funcall handler tracker) stor)
           ;;Default handler
           (push (elt (funcall tracker 1) 0) stor))
      while (not-empty (funcall tracker)))
    ;; for now
    (nreverse stor))
#|
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
  - indicate if only directives were found

|#
  )

(defparameter *max-comment-length* 10000) ;; Too long. Could go much closer to twitter.
(defparameter *max-excerpt-length* 500) ;; Also too long

(defun check-url (url)
  (cond
    ((< 2000 (length url)) (error "URL too long"))
    ((url-p url) url)
    (t (error "Not an URL"))))

(defun check-length (itm len)
  (if (< len (length itm))
      (error "Field too long")
      itm))

(defun deserialize-opinion-from-stream (stream)
  ;;FIXME: UNSAFE! Can't use read this way!
  ;;FIXME: Add over all length limit
  (let ((opinion (read stream)))
    (mapc (lambda (key) (check-url (getf opinion key))) '(:target :rooturl :author))
    (when (getf opinion :reference)
      (check-url (getf opinion :reference)))
    (check-length (getf opinion :comment) *max-comment-length*)
    (when (getf opinion :excerpt)
      (check-length (getf opinion :excerpt) *max-excerpt-length*))
    (setf (getf opinion :datestamp) (local-time:parse-timestring (getf opinion :datestamp)))
    (hu:plist->alist opinion)))

;;;;;
;; What do we need to know about opinions/ rooturl?
;; - what flags have been applied to target by whom?
;; - what opinions count for conversation?
;; - displayableness of opinion
;;   - summary only?
;;   - does it have a comment (once directives are subtracted)?
;; - outgoing references
;; - do we have all opinions?
;; - what opinions apply to the text?
;; - what opinions apply to the title?
;; - how to handle opinions down the tree? Say a disagree to a side type of flag?
;;   - maybe a separate category for guesses... Could make quite a difference to a discredited flag if
;;     someone reputable reinforces it.
;;   - different algos may treat differently.
;; - some stuff from the existing summarizer?
;; - rooturl text info should be included.
;; - Three opinion trees
;; - qualifies as a question or list of things?
;; - direction stuff is objective, right?
;; - replies total/immediate might be subjective... or should we not? Count of hidden. 
