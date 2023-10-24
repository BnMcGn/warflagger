
(in-package :wf/ipfs)

;;;
;;; ipfs.lisp
;;;
;;; Tools for interacting with the local IPFS instance.
;;;
;;;

;;; Save opinions and warstats to IPFS
;;; General equivalent to db.lisp


(defun ipfs-directory-exists-p (path)
  (let ((stat (ipfs:files-stat path)))
    (ignore-errors (equal "directory" (assoc-cdr "Type" stat :test #'equal)))))

(defun ipfs-file-exists-p (path)
  (let ((stat (ipfs:files-stat path)))
    (ignore-errors (equal "file" (assoc-cdr "Type" stat :test #'equal)))))

(defun ipfs-ensure-directory-exists (path)
  (or (ipfs-directory-exists-p path)
      (ipfs:files-mkdir path :parents t)))

(defun initialize-warstat-dirs ()
  (ipfs-ensure-directory-exists "/rooturls")
  (ipfs-ensure-directory-exists "/opinions")
  ;;FIXME
  (ipfs-ensure-directory-exists "/subjective")
  (ipfs-ensure-directory-exists "/subjective/default"))

;;FIXME: MFS for now. We might transition to OrbitDB.
(defun save-extended-opinion (opinion &key overwrite)
  (block nil
    (let* ((dir (strcat "/opinions/" (assoc-cdr :iid opinion) "/"))
           (file (strcat dir "opinion.data")))
     (when (and (not overwrite) (ipfs-file-exists-p file))
       (return))
     (ipfs-ensure-directory-exists dir)
     (ipfs:with-files-write (s (strcat dir "opinion.data") :create t :truncate t)
       (princ (warflagger:serialize-opinion opinion :extended t) s)))))

(defun serialize-warstat (warstat)
  (warflagger:with-inverted-case
    (prin1-to-string
     (cl-utilities:collecting
       (gadgets:do-window ((k v) warstat :size 2 :step 2)
         (cond ((eq :tree-freshness k)
                (cl-utilities:collect k)
                ;;Warstat for empty tree has nil tree freshness
                (cl-utilities:collect (if v (warflagger:js-compatible-utcstamp v) nil)))
               (t
                (cl-utilities:collect k)
                (cl-utilities:collect v))))))))

(defun add-author-to-opinion (opinion)
  (if (assoc-cdr :author opinion)
      opinion
      (list* (cons :author (warflagger:make-author-url (assoc-cdr :author-id opinion)))
             ;;FIXME: Should be independent of db
             (cons :authorname (warflagger:get-author-representation
                                (warflagger:find-author-id (assoc-cdr :author-id opinion))))
             opinion)))

(defun objective-data-for-rooturl (rooturl)
  (wf/ipfs:objective-data-for-opinions
   (mapcar #'add-author-to-opinion
           (mapcar #'warflagger:opinion-by-id
                   ;;FIXME: We will need an index opinions->rooturl for ipfs. This relies on DB.
                   (warflagger:opinion-ids-for-rooturl rooturl)))))

(defun ipfs-make-rooturl-data (rooturl)
  (let ((res (objective-data-for-rooturl rooturl)))
    (list* :results ;;could use a better name
           (warflagger:execute-score-script res :rooturl (or (getf res :rooturl) rooturl))
           res)))

(defun ipfs-write-rooturl-data (rooturl)
  (initialize-warstat-dirs)
  (hu:with-keys (:opinion-store :opinion-tree :score-script :results)
      (hu:plist->hash (ipfs-make-rooturl-data rooturl))
    (let ((warflagger:*opinion-store* opinion-store)
          (references (cl-utilities:collecting
                        (do-hash-table
                            (iid opin opinion-store)
                          (when (warflagger:reference-opinion-p opin)
                            (cl-utilities:collect iid)))))
          ;;FIXME: Relies on local db. Should eventually be based on OrbitDB or whatever
          ;; gets used long term.
          (refd (warflagger:discussion-refd-to rooturl))
          ;;Because IPFS daemon unescapes the escaped url in the path. Since we are doing a one-
          ;; way transform of the rooturl for id purposes only, this shouldn't harm.
          (rootpath (ipfs-rooturl-path rooturl "")))
      ;;Rooturl stuff
      (ipfs-ensure-directory-exists rootpath)
      (ipfs:with-files-write (s (strcat rootpath "opinion-tree.data") :create t :truncate t)
        (print opinion-tree s))
      (ipfs:with-files-write (s (strcat rootpath "score-script.data") :create t :truncate t)
        (let ((*package* (find-package :warflagger)))
          (warflagger:with-inverted-case
            (print score-script s))))
      (ipfs:with-files-write (s (strcat rootpath "references.data") :create t :truncate t)
        (warflagger:with-inverted-case
         (print (list :references references
                      :refd refd) s)))
      (ipfs:with-files-write (s (strcat rootpath "warstats.data") :create t :truncate t)
        (princ (serialize-warstat
                (hu:hash->plist
                 (warflagger::add-question-info
                  (warflagger::warstats-from-scsc-results (gethash rooturl results))
                  (gethash rooturl results)))) s))
      (ipfs:with-files-write (s (strcat rootpath "text.data") :create t :truncate t)
        (princ (serialize-warstat
                (alexandria:hash-table-plist
                 (warflagger::text-info-from-scsc-results results rooturl))) s))
      (ipfs:with-files-write (s (strcat rootpath "title.data") :create t :truncate t)
        (princ (serialize-warstat
                (alexandria:hash-table-plist
                 (warflagger::title-info-from-scsc-results results :rooturl rooturl))) s))
      ;;FIXME: Handle incoming references
      ;;Opinion stuff
      (gadgets:do-hash-table (iid opinion opinion-store)
        ;;FIXME: optimize the writings!
        (save-extended-opinion opinion :overwrite t)
        (let ((oppath (strcat "/opinions/" iid "/")))
          (ipfs:with-files-write (s (strcat oppath "warstats.data") :create t :truncate t)
            (princ (serialize-warstat
                    (hu:hash->plist
                     (warflagger::add-question-info
                      (warflagger::warstats-from-scsc-results (gethash iid results))
                      (gethash iid results) opinion))) s))
          (ipfs:with-files-write (s (strcat oppath "title.data") :create t :truncate t)
            (princ (serialize-warstat
                    (alexandria:hash-table-plist
                     (warflagger::title-info-from-scsc-results results :iid iid)))
                   s)))))))

(defun ipfs-rooturl-path (rooturl &optional more)
  (strcat "/rooturls/" (substitute #\* #\% (quri:url-encode rooturl))
          (when more "/")
          more))

(defun ipfs-rooturl-exists-p (rooturl)
  (ipfs-directory-exists-p (ipfs-rooturl-path rooturl)))

(defun ipfs-write-all-rooturl-data ()
  (dolist (rurl (append (warflagger:all-rooturls) (warflagger:all-proper-references)))
    (ipfs-write-rooturl-data rurl))
  (update-ipns))

;;;;;;;;;;;;;;;;
;; Grouped stuff
;;;;;;;;;;;;;;;;

(defun tidy-grouped-item (itm)
  (let ((allowed-keys '(:rowtype :display-depth :url :title :title-key :iid :comment :refparent
                                 :reference :reference-domain :tree-address :refbot :refopiniid
                                 :refiid)))
    (hu:collecting-hash-table (:mode :replace)
     (dolist (k allowed-keys)
       (when (gadgets:key-in-hash? k itm)
         (hu:collect k (gethash k itm)))))))

(defun serialize-grouped (grouped)
  (warflagger:with-inverted-case
   (prin1-to-string
    (gadgets:mapcan-by-2
     (lambda (k v)
       (case k
         (:group-opinions (list k v))
         (:group-rooturls (list k v))
         (:keywords (list k (hu:hash->plist v)))
         (:groups (list k (proto:mapleaves
                           (lambda (leaf)
                             (if (hash-table-p leaf)
                                 (hu:hash->plist (tidy-grouped-item leaf))
                               leaf))
                           v)))))
     grouped))))

(defun ipfs-write-grouped-data ()
  (initialize-warstat-dirs)
  ;;FIXME: load-static-grouped-list is a short term strategy
  (let* ((data (warflagger::prep-data-for-grouped-ipfs (warflagger::load-static-grouped-list)))
         (data (serialize-grouped data)))
    (ipfs:with-files-write (s "/subjective/default/grouped.data" :create t :truncate t)
     (princ data s))))

;;FIXME: This is ignorant. We are going to clear out everything. Probably wrong, but let's find out
(defun reset-pins ()
  (dolist (pin (ipfs:pin-ls :type "recursive"))
    (ipfs:pin-rm (car pin)))
  (ipfs:pin-add (assoc-cdr "Hash" (ipfs:files-stat "/") :test #'equal))
  (dolist (item (flatten (files-directories)))
    (ipfs:pin-add item)
    (ipfs:pin-add (assoc-cdr "Hash" (ipfs:files-stat item) :test #'equal))))

(defun update-ipns ()
  (let* ((stat (ipfs:files-stat "/"))
         (roothash (assoc-cdr "Hash" stat :test #'equal))
         ;;FIXME: IPNS is dog slow. Can we do without it?
         (namestat (ipfs:name-publish (strcat "/ipfs/" roothash))))
    (assoc-cdr "Name" namestat :test #'equal)))

(defun files-directories (&optional (root "/"))
  "Get a tree of the directories in the files repository. Each directory is represented by its hash."
  (let*
      ((stat (ipfs:files-stat root))
       (hash (ignore-errors (assoc-cdr "Hash" stat :test #'equal)))
       (type (ignore-errors (assoc-cdr "Type" stat :test #'equal))))
    (unless (equal type "directory")
      (error "Root directory not found"))
    (labels ((proc (dirhash)
               (cl-utilities:collecting
                 (let* ((res (ipfs:ls dirhash))
                        (items (and res (ignore-errors (assoc-cdr "Links" res :test #'equal)))))
                   (when res
                     (cl-utilities:collect dirhash))
                   (dolist (item items)
                     (when (eq 1 (ignore-errors (assoc-cdr "Type" item :test #'equal)))
                       (let ((hash (ignore-errors (assoc-cdr "Hash" item :test #'equal))))
                         (cl-utilities:collect (proc hash)))))))))
      (proc hash))))

(clerk:job "update ipns" every 12.hours (update-ipns))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPFS read
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ipfs-warstats (item)
  (if (warflagger:iid-p item)
      (ipfs-warstats-for-opinion item)
      (ipfs-warstats-for-rooturl item)))

(defun ipfs-warstats-for-rooturl (rooturl)
  "Load warstats for a rooturl from IPFS"
  (when (ipfs-rooturl-exists-p rooturl)
    (warflagger:deserialize-warstat (ipfs:files-read (ipfs-rooturl-path rooturl "warstats.data")))))

(defun ipfs-warstats-for-opinion (iid)
  nil)


