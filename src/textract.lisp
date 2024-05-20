(in-package :warflagger)

(defun tt-extract (page)
  (let* ((pobj (plump:parse page))
         (title (readability::get-article-title pobj))
         (article (readability::grab-article pobj))
         (simple-page (plump:serialize article nil))
         (text (readability::inner-text article)))
    (values title text article)))




