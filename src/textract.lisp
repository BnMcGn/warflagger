(in-package :warflagger)

(defun tt-extract (page)
  (let* ((pobj (plump:parse page))
         (title (readability::get-article-title pobj))
         (article (readability::grab-article pobj))
         (simple-page (plump:serialize article nil))
         (text (readability::inner-text article)))
    (values title text article)))

(defun tt-get-page-from-archive (url)
  ;;Try common crawl first
  (let* ((errors nil)
         (captures (crawly:url-search url :limit 1 :source :common-crawl))
         (warc (when captures (crawly:get-archive-from-capture :common-crawl (car captures))))
         (page (when warc (crawly:get-record-for-url warc url))))
    (if page
        (return-from tt-get-page-from-archive (values nil page))
      (push
       (cond ((and captures warc) "Common Crawl: unable to extract page from WARC")
             (captures "Common Crawl: unable to fetch WARC for URL")
             (t "Common Crawl: URL not found"))
       errors))
    (let ((captures (crawly:url-search url :limit 1 :source :internet-archive))
          (page (when captures (crawly:get-archive-from-capture :internet-archive (car captures)))))
      (if page (return-from tt-get-page-from-archive (values (nreverse errors) page))
          (push
           (cond ((captures "Internet Archive: unable to fetch page")
                  (t "Internet Archive: URL not found")))
           errors))
      (values (nreverse errors) page))))

(defun join-errors (&rest erratum)
  (funcall
   #'gadgets:string-join
   #\Newline
   (cl-utilities:collecting
     (dolist (err erratum)
       (if (stringp err)
           (cl-utilities:collect err)
           (mapcar #'cl-utilities:collect err))))))

(defun tt-update-page-data (url)
  (multiple-value-bind (errors page)
      (tt-get-page-from-archive url)
    (if page
        (multiple-value-bind (title text) (tt-extract page)
          (if text
              (progn
                (wf/ipfs:ipfs-delete-original-failure url)
                (wf/ipfs:ipfs-write-original-title title)
                (wf/ipfs:ipfs-write-original-text text))
              (wf/ipfs:ipfs-write-original-failure
               (join-errors errors "Warflagger: Couldn't extract text from page"))))
        (wf/ipfs:ipfs-write-original-failure (join-errors errors)))))

(defun tt-is-cached (url)
  (or (wf/ipfs:ipfs-file-exists-p (wf/ipfs:ipfs-rooturl-path url "original-text.txt"))
      (wf/ipfs:ipfs-file-exists-p (wf/ipfs:ipfs-rooturl-path url "original-title.txt"))))

(defun tt-has-failure (url)
  (wf/ipfs:ipfs-file-exists-p (wf/ipfs:ipfs-rooturl-path url "original-failure.txt")))

(defun text-server (url)
  "This function, served as JSON, is the text server. Url is the address of the desired text"
  (hu:collecting-hash-table (:mode :replace)
    (labels ((get-text ()
               (if (wf/ipfs:ipfs-file-exists-p (wf/ipfs:ipfs-rooturl-path url "original-text.txt"))
                   (wf/ipfs:ipfs-original-text url)
                   ""))
             (get-title ()
               (if (wf/ipfs:ipfs-file-exists-p (wf/ipfs:ipfs-rooturl-path url "original-title.txt"))
                   (wf/ipfs:ipfs-original-title url)
                   "")))
      (cond
        ((or (null url) (eq 0 (length url)))
         (hu:collect :text "")
         (hu:collect :title "")
         (hu:collect :status "failure")
         (hu:collect :message "No URL provided"))
        ((not (ratify:url-p url))
         (hu:collect :text "")
         (hu:collect :title "")
         (hu:collect :status "failure")
         (hu:collect :message "Not a valid URL"))
        ((tt-has-failure url)
         (hu:collect :text (get-text))
         (hu:collect :title (get-title))
         (hu:collect :status "failure")
         (hu:collect :message (wf/ipfs:ipfs-original-failure url)))
        ((tt-is-cached url)
         (hu:collect :text (get-text))
         (hu:collect :title (get-title))
         (hu:collect :status "success")
         (hu:collect :message ""))
        (t
         ;;Can't detect pending now. Change that if needed?
         ;;(unless (is-pending url) (update-page url))
         (tt-update-page-data url)
         (hu:collect :text (get-text))
         (hu:collect :title (get-title))
         (hu:collect :status "wait")
         (hu:collect :message "Loading page text..."))))))
