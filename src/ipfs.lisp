
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
    (princ (hu:hash->plist (gethash key warstats)) s))
  ;;FIXME: text and title will be wanting some additions
  (ipfs:with-files-write (s (strcat location "text.data") :create t)
    (princ (hu:hash->plist (gethash key text)) s))
  (ipfs:with-files-write (s (strcat location "title.data") :create t)
    (princ (hu:hash->plist (gethash key title)) s)))

(defun ipfs-write-rooturl-data (rooturl)
  (initialize-warstat-dirs)
  ;;FIXME: We will need an index opinions->rooturl for ipfs. This relies on DB.
  (let* ((opinions (warflagger:opinion-ids-for-rooturl rooturl))
         (opinions (mapcar #'warflagger:opinion-by-id opinions))
         (rootpath (strcat "/rooturls/" (quri:url-encode rooturl) "/")))
    (hu:with-keys (:opinion-store :opinion-tree :score-script :rooturl)
        (hu:plist->hash (wf/ipfs:objective-data-for-opinions opinions))
      (multiple-value-bind (warstats text title)
          (warflagger:execute-score-script score-script rooturl opinion-store)
        ;;Rooturl stuff
        (ipfs-ensure-directory-exists rootpath)
        (ipfs:with-files-write (s (strcat rootpath "opinion-tree.data") :create t)
          (princ opinion-tree s))
        (ipfs:with-files-write (s (strcat rootpath "score-script.data") :create t)
          (princ score-script s))
        (save-warstat-sets rootpath rooturl warstats text title)
        ;;Opinion stuff
        (gadgets:do-hash-table (iid opinion opinion-store)
          (save-extended-opinion opinion)
          (save-warstat-sets (strcat "/opinions/" iid "/") iid warstats text title))))))

;; Bit of a problem: objective-data wants a list of opinml files. We don't have that.
;; How do we get started without?

;; Returns: plist: :opinion-store :opinion-tree :score-script :rooturl

(warflagger:execute-score-script )

;; Returns: values: warstats textwarstats titlewarstats
