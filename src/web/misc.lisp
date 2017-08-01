(in-package :wf/web)


(defmethod json:encode-json ((object clsql-sys:wall-time) &optional stream)
  (write-char #\" stream)
  (clsql:format-time stream object :format :iso8601)
  (write-char #\" stream))

(defmethod json:encode-json ((object local-time:timestamp) &optional stream)
  (write-char #\" stream)
  (local-time:format-timestring stream object)
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

(defun target-data (id)
  (let* ((url (get-rooturl-by-id id))
         (text
           (progn
             (unless (is-cached url)
               (error "Don't have that page!"))
             ;;FIXME: Suboptimal place, but somebody has to do it...
             (make-rooturl-real id)
             (grab-text url)))
         (opins
           (%fill-out-opinion-tree
            (opinion-tree-for-rooturl url) (create-textdata text)))
         (looks (get-looks (get-user-name) id)))
    (values
     (json:encode-json-to-string text)
     (json:encode-json-to-string opins)
     (json:encode-json-to-string looks))))

(defun signup-page ())
'(defun signup-page ()
  (let ((username (get-user-name)))
    (unless username
      (error "User is not signed in at signup-page"))
    (if (new-user-p username)
        ))
  (unless
      (get-user-name)
    (error "User is not signed in at signup-page"))
  (if ))
