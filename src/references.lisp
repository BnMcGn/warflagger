(in-package :warflagger)


;;;
;; Find-urls based on code borrowed from https://github.com/clips/pattern
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

