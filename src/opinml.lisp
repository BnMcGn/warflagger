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

(defun parse-comment-field (comment)

#|
- #() if alone on line. Should we strictly stay at start of line?
- #tag whitespace before and after
- markdown style links
- no other markdown for now. Or...
- should return structure with:
  - raw text.
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
