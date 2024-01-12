(in-package #:wf/web)

(defun format-flags ()
  (cl-utilities:collecting
      (cl-utilities:collect (list nil "--Choose a Flag--"))
    (loop for cat in *flag-categories*
       for flags in *flag-labels*
       do (dolist (flag flags)
            (let ((flagstring (format nil "~a: ~a" cat flag)))
              (cl-utilities:collect (list flagstring flagstring)))))))

(defun format-flag-descriptions ()
  (cl-hash-util:collecting-hash-table (:mode :replace)
    (loop for cat in *flag-categories*
       for descs in *flag-types-source*
       for flags in *flag-labels*
       do (loop for (k desc) on descs by #'cddr
             for flag in flags
             do (cl-hash-util:collect (format nil "~a: ~a" cat flag) desc)))))

;;FIXME: form side of spec not in use any more
(defparameter *opinion-form-specs*
  (gadgets:mapcan-by-2
   (lambda (k v) (list k (webhax-validate:normalize-fieldspec-body v)))
   `(:target
     ((:or (:url :notnull) ,(webhax-validate:predicate-test #'warflagger:iid-p "Not an OpinionID"))
      :description "Target URL")
     :excerpt
     (:string)
     :excerpt-offset
     (:integer :description "Excerpt Offset")
     :flag
     ((:pickone :options ,(format-flags) :notnull)
      ;;FIXME: pickone-long was a bare symbol. What should it be?
            :widget "pickoneLong")
     :reference (:url :description "Reference URL")
     :comment (:string :widget :textentry))))

;;FIXME: Needs to update automatically from any updates to the userfig, as per
;; userfig settings. Also, need default homepage url + provision for override.
(defun create-author-spec-from-current-user ()
  (list
   :wf-user (get-user-name)
   :screen-name (userfig:userfig-value 'screen-name)
   ;;:display-name (get-openid-display-name)
   ;;:homepage (make-user-url (get-openid-display-name))
   ))

(proto:watch-for-recompile
  (defun opinion-post-response ()
    (check-signed-up)
    (multiple-value-bind (values sig)
        (webhax-validate:validate-batch
         *key-web-input*
         *opinion-form-specs*)
      (when sig
        (unless (is-author-initialized (get-user-name))
          (apply #'initialize-author (create-author-spec-from-current-user)))
        (gadgets:hashval! (val :flag values)
          (warflagger::flag-to-lisp
           (apply #'gadgets:strcat (split-sequence:split-sequence #\: val))))
        (save-opinion (hu:hash->alist values) (get-user-name) :post #'after-save-opinion))
      (list 200 '(:content-type "text/json")
            (list (webhax-validate:batch-response-json values sig))))))

(defun after-save-opinion (opinion)
  ;;FIXME: this is not working
  (save-new-references (assoc-cdr :url opinion))
  (wf/ipfs:ipfs-write-rooturl-data (assoc-cdr :rooturl opinion))
  (wf/ipfs::update-ipns)
  (warflagger::write-grouped-data-file)
  ;;FIXME: pre-ipfs stuff that should go away
  (write-all-rootid-warstats (assoc-cdr :rootid opinion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New opinion form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-opinion-page ()
  (check-signed-up)
  (bind-validated-input
      (&key
       (target (:or :url (webhax-validate:predicate-test #'warflagger:iid-p "Not an OpinionID"))) 
       (excerpt :string)
       (offset :unsigned-integer)
       (target-text :boolean)
       (target-title :boolean)
       (suggest-target-text :boolean)
       (suggest-target-title :boolean))
      ;(unless (or (ratify:url-p target) (warflagger:iid-p target))
      ;  (webhax-core:web-fail-400 "Target must be URL or IID"))
    (mount-cljs-component ("make-opinion")
      :target (lisp target)
      :excerpt (lisp excerpt)
      :offset (lisp offset)
      :target-text (lisp target-text)
      :target-title (lisp target-title)
      :suggest-target-text (lisp suggest-target-text)
      :suggest-target-title (lisp suggest-target-title))))


(defun mock-make-page ()
  (bind-validated-input
   ((section :string))
   (mount-cljs-component ("mock-make")
                         :section (lisp section))))
