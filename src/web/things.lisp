(in-package :wf/web)

;;; Definitions for the thing-lister

(define-parts warflagger-things
  :@javascript
  (titlebar-components)
  :@javascript
  (ps
    (def-component opinion-line
        (psx (:div
              (:vote-value :key 1 :opinion (prop opinion))
              (:flag-name :key 2 :opinion (prop opinion))
              (:date-stamp :key 3 :opinion (prop opinion))
              (:author-long :key 4 :opinion (prop opinion))
                                        ;(:target-short :target)
              (:comment-summary :key 5 :opinion (prop opinion) :trimto 40))))))

(setf html-thing-lister:*html-thing-user-parts* nil)
(push #'warflagger-things html-thing-lister:*html-thing-user-parts*)
(push #'webhax:react-parts html-thing-lister:*html-thing-user-parts*)

(defun display-opinion-line (opinion)
  (let ((line-id (gadgets:mkstr (gensym "mount-opinion-"))))
    (webhax-core:html-out-str
      (:div :id line-id)
      (mount-component (opinion-line :mount-id (lisp line-id))
        :opinion (lisp-raw (json:encode-json-alist-to-string opinion))))))

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
  #'display-opinion-line
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

(def-thing-connector
    'opinion
    'opinion
  (lambda (&rest x)
    (mapcar #'car (opinion-tree-for-target
                   (assoc-cdr :url (opinion-from-id (car x)))))))
