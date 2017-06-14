(in-package :wf/web)


(def-thing
    'user
    (compose #'get-author-data #'get-local-user-id)
  #'author-representation-from-row
  :lister (list
           #'user-lister
           :sortkeys '(values id)
           :length (lambda (&rest params)
                     (get-count
                      (unexecuted
                        (apply #'user-lister params))))))

(def-db-thing
    'opinion
  'opinion
  (lambda (x)
    (with-output-to-string (s)
      (print x s)))
  :keyfunc (lambda (id)
             (opinion-from-db-row (get-assoc-by-pkey 'opinion id)))
  :sortkeys '(target author datestamp excerpt rooturl))

(def-thing
    'target
    (lambda (rootid)
      ;;FIXME: maybe should check that page is extracted/available
      (let ((url (get-rooturl-by-id rootid)))
        (list
         :id rootid
         :title (grab-title url)
         :text (grab-text url)
         :url url
         :warstats (warstats-for-target url))))
  (lambda (targdata)
    (concatenate 'string
                 (truncate-string (getf targdata :title) :length 30)
                 " - "
                 (truncate-string (getf targdata :text) :length 30)))
  :lister
  (list
   (wrap-with-paging-handler
    (lambda (&key order-by)
      (declare (ignore order-by))
      ;;Only have relevance for now
      (get-ranked-rootids)))
   :sortkeys '(relevance))
  :html-thing-link (lambda (id) (format nil "/target/~a" id)))
