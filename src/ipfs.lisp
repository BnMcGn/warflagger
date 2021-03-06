
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
  (ipfs-ensure-directory-exists "/opinions"))

;;FIXME: MFS for now. We might transition to OrbitDB.
(defun save-extended-opinion (opinion &key overwrite)
  (block nil
    (let* ((dir (strcat "/opinions/" (assoc-cdr :iid opinion) "/"))
           (file (strcat dir "opinion.data")))
     (when (and (not overwrite) (ipfs-file-exists-p file))
       (return))
     (ipfs-ensure-directory-exists dir)
     (ipfs:with-files-write (s (strcat dir "opinion.data") :create t)
       (princ (warflagger:serialize-opinion opinion :extended t) s)))))

(defun save-warstat-sets (location key warstats text title)
  (ipfs:with-files-write (s (strcat location "warstats.data") :create t)
    (when-let ((data (gethash key warstats)))
      (print (hu:hash->plist data) s)))
  ;;FIXME: text and title will be wanting some additions
  (ipfs:with-files-write (s (strcat location "text.data") :create t)
    (when-let ((data (gethash key text)))
      (print (hu:hash->plist data) s)))
  (ipfs:with-files-write (s (strcat location "title.data") :create t)
    (when-let ((data (gethash key title)))
      (print (hu:hash->plist data) s))))

(defun add-author-to-opinion (opinion)
  (if (assoc-cdr :author opinion)
      opinion
      (cons (cons :author (warflagger:make-author-url (assoc-cdr :author-id opinion))) opinion)))

(defun ipfs-write-rooturl-data (rooturl)
  (initialize-warstat-dirs)
  ;;FIXME: We will need an index opinions->rooturl for ipfs. This relies on DB.
  (let* ((opinions (warflagger:opinion-ids-for-rooturl rooturl))
         (opinions (mapcar #'warflagger:opinion-by-id opinions))
         (opinions (mapcar #'add-author-to-opinion opinions))
         (references (remove-if-not #'warflagger:reference-opinion-p opinions))
         (references (mapcar (lambda (x) (assoc-cdr :iid x)) references))
         (rootpath (strcat "/rooturls/" (quri:url-encode rooturl) "/")))
    (hu:with-keys (:opinion-store :opinion-tree :score-script :rooturl)
        (hu:plist->hash (wf/ipfs:objective-data-for-opinions opinions))
      (multiple-value-bind (warstats text title)
          (warflagger:execute-score-script score-script rooturl opinion-store)
        ;;Rooturl stuff
        (ipfs-ensure-directory-exists rootpath)
        (ipfs:with-files-write (s (strcat rootpath "opinion-tree.data") :create t)
          (print opinion-tree s))
        (ipfs:with-files-write (s (strcat rootpath "score-script.data") :create t)
          (print score-script s))
        (ipfs:with-files-write (s (strcat rootpath "references.data") :create t)
          (print (list :references references) s))
        (save-warstat-sets rootpath rooturl warstats text title)
        ;;FIXME: Handle incoming references
        ;;Opinion stuff
        (gadgets:do-hash-table (iid opinion opinion-store)
          (save-extended-opinion opinion)
          (save-warstat-sets (strcat "/opinions/" iid "/") iid warstats text title))))))

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
    (assoc-cdr "Name" namestat)))

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
