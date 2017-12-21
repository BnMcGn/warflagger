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
;; In addition, there are references that users have directly attached to the target,
;; as well as references attached further down the discussion tree. We could also be
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

(defun get-references-to (url)
  "Returns IDs of opinions that contain a reference to URL."
  (grab-column (liql url 'reference.reference 'reference.opinion 'opinion)))

(defun get-generated-references-from (url)
  (let ((rid (get-refbot-id)))
    (grab-column
      (liql url 'reference.reference 'reference.opinion 'opinion.id rid 'opinion.author))))

(defun get-references-from (url)
  "Returns IDs of opinions that are referenced by the URL in one way or another."
  (let ((res (get-generated-references-from url)))
    (multiple-value-bind (id type) (get-target-id-from-url url)
      (if (and (eq :opinion type)
               (grab-one (liql id 'reference.opinion)))
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

(defun extract-links-from-target (url)
  (let ((text))
    (cond
      ((is-cached url)
       (if-let ((links (grab-links url)))
         (return-from extract-links-from-target links)
         (setf text (grab-text url))))
      ((opinion-exists-p url)
       (setf text (assoc-cdr :comment (opinion-exists-p url))))
      (t (error "Target not found as RootURL or as opinion")))
    (mapcar (lambda (x) (list x nil)) (find-urls text))))

(defun reference-redundant-p (ref1 ref2)
  "Ref1 is the existing reference. Ref2 is the addition being considered. Will treat as redundant either if the excerpt (second itm) matches or if ref2 doesn't have an excerpt."
  (and (equal (car ref1) (car ref2))
       (or (equal (second ref1) (second ref2))
           (not (second ref2)))))

(defun find-new-references (url)
  (let ((existing
          (collecting-hash-table (:test #'equal)
            (dolist (ref (get-references-from url))
              (collect (car ref) ref))))
        (stor (make-hash-table :test #'equal)))
    (flatten-1
     (hash-table-values
      (collecting-hash-table (:existing stor)
        (dolist (ref (extract-links-from-target url))
          (unless (first-match (rcurry #'reference-redundant-p ref)
                               (cat (gethash (car ref) existing)
                                    (gethash (car ref) stor)))
            (collect (car ref) ref))))))))

(defun save-new-references (url)
  (loop for (link excerpt) in (find-new-references url)
     do (insert-generated-reference-opinion url link excerpt)))
