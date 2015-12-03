(in-package :wf/web)


(defmethod json:encode-json ((object clsql-sys:wall-time) &optional stream)
  (write-char #\" stream)
  (clsql:format-time stream object :format :iso8601)
  (write-char #\" stream))

(defun test-js ()
  (ps
    (define-react-class thing
        (cl-react:psx
         (:a :href "http://www.google.com"
             (:span :class "text-green" "Click heare!" ))))
    (chain |ReactDOM|
           (render (create-element thing)
                   (chain document (get-element-by-id "things"))))))


(defun %opinion-data (opid text)
  (let ((data (opinion-from-id opid)))
    (if (assoc-cdr :excerpt data)
        (cons
         (list*
          :text-position
          (multiple-value-list
           (find-excerpt-position text (assoc-cdr :excerpt data)
                                  (or (assoc-cdr :excerptoffset data) 0))))
         data)
        data)))

(defun target-data (id)
  (let* ((url (get-rooturl-by-id id))
         (text
           (progn
             (unless (is-cached url)
               (error "Don't have that page!"))
             (collecting-string
               (do-file-by-line (ln (grab-text url))
                 (collect (strcat ln #\Newline))))))
         (opins
           (mapleaves (rcurry #'%opinion-data text)
                      (opinion-tree-for-rooturl url))))
    (json:encode-json-to-string `((:text . ,text) (:opinions . ,opins)))))

