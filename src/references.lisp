(in-package :warflagger)


;;;
;; Find-urls is a port of code from https://github.com/clips/pattern
;;;

(defparameter *re-head-punctuation* "\"'{(>")
(defparameter *re-tail-punctuation* "\"'.,;)}")
(defparameter *re-url-domains* '("com" "net" "org" "edu" "de" "uk"))
(defparameter *re-url-head*
  (format nil "[~a|\\[|\\s]" (string-join #\| (sequence->list *re-head-punctuation*))))
(defparameter *re-url-tail*
  (format nil "[~a|\\]]*[\\s|\\<]"
          (string-join #\| (sequence->list *re-tail-punctuation*))))
(defparameter *re-url1* (ppcre:create-scanner (strcat "(https?://.*?)" *re-url-tail*)))
(defparameter *re-url2* (ppcre:create-scanner
                         (strcat *re-url-head* "(www\\..*?\\..*?)" *re-url-tail*)))
(defparameter *re-url3*
  (ppcre:create-scanner
   (strcat *re-url-head* "([\\w|-]*?\\.(" (string-join #\| *re-url-domains*) "))"
           *re-url-tail*)))

(defun find-urls (string)
  (let ((string (string-join "  " (split-sequence:split-sequence #\  string)))
        (res nil))
    (nsubstitute #\. (code-char #x2024) string)
    (dolist (regex (list *re-url1* *re-url2* *re-url3*))
      (ppcre:do-matches-as-strings (match regex string)
        (push (car (split-sequence-on-subseq '("\">" "'>") match)) res)))
    (nreverse res)))



;;;
;; Not all references are equal: there are those found in the reference field of an
;; opinion. There are references found in the text of an article or an opinion. The
;; current policy is to harvest these and store them as generated opinions attached
;; to the article or opinion.
;; Another distinction: references that users have directly attached to the target,
;; vs. references attached further down the discussion tree. We could also be
;; interested in references to references, perhaps when trying to identify the general
;; topic of an article.
;;;

(defparameter *system-author-id* nil)
(defparameter *system-author-user* "wf_refbot")
(defparameter *system-author-display* "WF:RefBot")

(defun get-refbot-id ()
  (setf *system-author-id*
        (or *system-author-id*
            (get-local-user-id *system-author-user*)
            (insert-new-author
             :wf-user *system-author-user*
             :display-name *system-author-display*))))

(defun system-generated-p (opinid)
  (when-let ((opin (opinion-from-id opinid)))
    (eq (get-refbot-id) (assoc-cdr :author-id opin))))

(defun get-references-to (url)
  "Returns IDs of opinions that contain a reference to URL."
  (grab-column (liql url 'reference.reference 'reference.opinion 'opinion)))

(defun get-generated-references-from (url)
  (let ((rid (get-refbot-id)))
    (grab-column
      (liql url 'opinion.target rid 'opinion.author 'opinion 'reference.opinion))))

(defun get-references-from (url)
  "Returns IDs of opinions that are references made by the URL in one way or another."
  (let ((res (get-generated-references-from url)))
    (multiple-value-bind (id type) (get-target-id-from-url url)
      (if (and (eq :opinion type)
               (grab-column (liql id 'reference.opinion)))
          (cons id res) ;; URL is itself a reference opinion.
          res))))

(defun insert-generated-reference-opinion (target reference excerpt)
  (save-opinion-from-user
   `((:target . ,target)
     (:votevalue . 0)
     (:datestamp . ,(clsql:get-time))
     ,@(when excerpt
             (list (cons :excerpt excerpt)))
     (:comment . "Autogenerated by RefBot from link in the target text")
     (:reference . ,reference)
     (:flag . (:custodial :blank)))
   *system-author-id*))

(defun %absolutify (url found-url)
  "Given a relative link in a document, it should be found on the same server as the document."
  (let ((parsed (puri:parse-uri found-url)))
    (if (puri:uri-host parsed)
        found-url
        (puri:render-uri (puri:merge-uris parsed url) nil))))

(defun %extract-links-from-target (url)
  (let ((text))
    (cond
      ((is-cached url)
       (if-let ((links (grab-links url)))
         (return-from %extract-links-from-target links)
         (setf text (grab-text url))))
      ((opinion-exists-p url)
       (setf text (assoc-cdr :comment (opinion-exists-p url))))
      (t (error "Target not found as RootURL or as opinion")))
    (mapcar (lambda (x) (list x nil)) (find-urls text))))

(defun extract-links-from-target (url)
  (mapcar (lambda (x) (cons (%absolutify url (car x)) (cdr x)))
          (%extract-links-from-target url)))

(defun reference-redundant-p (ref1 ref2)
  "Ref1 is the existing reference. Ref2 is the addition being considered. Will treat as redundant either if the excerpt (second itm) matches or if ref2 doesn't have an excerpt."
  (and (equal (car ref1) (car ref2))
       (or (equal (second ref1) (second ref2))
           (not (second ref2)))))

(defun %format-reference (opinid)
  (let ((opin (opinion-from-id opinid)))
    (list (assoc-cdr :reference opin) (assoc-cdr :excerpt opin))))

(defun find-new-references (url)
  (let ((existing
          (collecting-hash-table (:test #'equal)
            (dolist (opid (get-references-from url))
              (let ((ref (%format-reference opid)))
                (collect (car ref) ref)))))
        (stor (make-hash-table :test #'equal)))
    (flatten-1
     (nreverse
      (hash-table-values
       (collecting-hash-table (:existing stor)
         (dolist (ref (extract-links-from-target url))
           (unless (find-if (rcurry #'reference-redundant-p ref)
                            (cat (gethash (car ref) existing)
                                 (gethash (car ref) stor)))
             (collect (car ref) ref)))))))))

(defun save-new-references (url)
  (loop for (link excerpt) in (find-new-references url)
     do (insert-generated-reference-opinion url link excerpt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Cluster detection tools
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;FIXME: Discussion clustering is very basic for now, simply considering outgoing
;; references as the source of info. We can also use: word clustering, participants,
;; hashtags. 

(defun discussion-root-p (rootid)
  "For now, we consider a RootURL to be a discussion root if it has opinions on it and if one of those opinions predate any references to it."
  (let* ((rooturl (get-rooturl-by-id rootid))
         (first-opin
          (take-one
           (select (colm 'opinion 'datestamp)
                   :from (tabl 'opinion)
                   :where (sql-= (colm 'target) rooturl)
                   :order-by (colm 'datestamp)
                   :ascending t))))
    (when first-opin
      (let ((first-ref
             (take-one
              (select (colm 'opinion 'datestamp)
                      :from (list (tabl 'opinion) (tabl 'reference))
                      :where (sql-and
                              (sql-= (colm 'reference 'reference) rooturl)
                              (sql-= (colm 'reference 'opinion) (colm 'opinion 'id)))
                      :order-by (colm 'datestamp)
                      :ascending t))))
        (if first-ref
            (when (time< first-opin first-ref)
              t)
            t)))))

;;FIXME: Should also return references to any child of URL?
(defun get-rootids-of-references-to (url)
  (collecting-set ()
    (dolist (refid (get-references-to url))
      (when-let*
          ((opin (opinion-from-id refid))
           (rootid (assoc-cdr :rooturl opin)))
        (collect rootid)))))

(defun get-rootids-referred-to-in-tree (url)
  (let ((ids
         (collecting-set ()
           (dolist (id (flatten (opinion-tree-for-target url)))
             (collect id))
           (dolist (id (get-references-from url))
             (collect id)))))
    (collecting
        (dolist (id ids)
          (let ((opin (opinion-from-id id)))
            (when-let*
                ((refurl (assoc-cdr :reference opin))
                 (rootp (rooturl-p refurl)))
              (collect (get-rooturl-id refurl))))))))

;;FIXME: For now, we are not attaching incoming refs to the discussion.
;;FIXME: This is not tail recursive. Could hit some limits.
(defun discussion-tree-for-root (discroot discroots)
  "Given a discussion root rootURL ID, and a list of all such ids, builds a tree of the rootURLs in the discussion. Returns a list of other discroots that get touched in the discussion as the second value."
  (let ((found (make-hash-table))
        (otherdisc nil))
    (setf (gethash discroot found) t)
    (labels ((proc (curr)
               (collecting
                   (dolist (id (get-rootids-referred-to-in-tree
                                (get-rooturl-by-id curr)))
                     (unless (gethash id found)
                       (setf (gethash id found) t)
                       (collect
                           (if (member id discroots)
                               (progn (push id otherdisc)
                                      (list id))
                               (list* id (proc id)))))))))
      (values (list* discroot (proc discroot)) otherdisc))))

(defun cluster-discussions ()
  (let* ((rooturls (all-rooturls))
         (rootids (mapcar #'get-rooturl-id rooturls))
         (discroots (remove-if-not #'discussion-root-p rootids)))
    (collecting
        (dolist (dr discroots)
          (collect dr)
          (collect (multiple-value-list (discussion-tree-for-root dr discroots)))))))

