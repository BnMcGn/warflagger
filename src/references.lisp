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
  (grab-column (liql url 'reference.reference 'opinion)))

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



(defun store-references (url urls)
  "URL is the source of references, urls is a list of things to which it makes reference. We will check for the existence of references before inserting to avoid duplicates."
  (let ((existing (get-references-from url)))
    (dolist (u urls)
      (when (not (member u existing :test #'equal))
        (insert-records :into 'reference2
                        :attributes (colms :urlfrom :urlto)
                        :values (list url u))))))
