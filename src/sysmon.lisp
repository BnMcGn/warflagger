(in-package :warflagger)

;;;
;;; sysmon.lisp
;;;
;;; Tools for watching the live server
;;;
;;;


;; Services

;; IPFS
;; - should check a remote node or two...

(defun is-ipfs-online? ()
  (let* ((stats (ipfs:diag-sys))
         (net (proto:assoc-cdr2 :net stats :test #'string-equal)))
    (proto:assoc-cdr2 :online net :test #'string-equal)))

;; Web

(defun is-nginx-running? ()
  (search "nginx: master" (uiop:run-program "ps aux" :output :string)))

;; Postgresql

(defun is-postgresql-online? ()
  (sql-stuff:exists (sql-stuff:unexecuted (warflagger:author-lister))))

;; Other stati

;; active sessions

(defun how-many-active-sessions? ()
  (let ((conn (apply #'dbi:connect wf/local-settings:*session-db-connect-spec*)))
    (unwind-protect
         (let ((query (dbi:prepare conn
                                   "SELECT * FROM sessions")))
           (length (dbi:fetch-all (dbi:execute query))))
         (dbi:disconnect conn))))

;; new posts

(defun how-many-posts-today? ()
  (get-count
   (unexecuted
     (merge-query
      (clsql:select
       (colm 'id)
       :from (tabl 'opinion))
      (recent-mixin 'datestamp "1 days")))))

;; unprepped texts

(defun reportable-page-urls ()
  ;;FIXME: remove duplications
  (let* ((urlist (concatenate 'list (warflagger:all-rooturls)
                              (warflagger:all-proper-references)))
         (urlist (remove-if #'warflagger:iid-p urlist))
         (urlist (remove-if
                  (alexandria:rcurry
                   #'gadgets:sequence-starts-with "http://warflagger.net/")
                  urlist))
         (urlist (remove-if
                  (alexandria:rcurry
                   #'gadgets:sequence-starts-with "https://warflagger.net/")
                  urlist)))
    urlist))

(defun url-of-concern? (url)
  (alexandria:if-let ((tinfo (gadgets:tryit* (wf/ipfs:data-not-found)
                               (wf/ipfs:ipfs-text-info-for-rooturl url))))
    (if (eql :initial (gethash :text-source tinfo)) t nil)
    t))

(defun last-30-days? (ctime)
  (let ((ago30 (- (get-universal-time) (gadgets:encode-time-delta 0 0 0 30))))
    (> (clsql-helper:clsql-date/times->utime ctime) ago30)))

(defun how-many-unopened-root-targets-30-days? ()
  (let* ((urls (remove-if-not #'url-of-concern? (reportable-page-urls)))
         (urls (remove-if-not (lambda (url)
                                (alexandria:when-let ((iid (earliest-mention url)))
                                  (last-30-days?
                                   (assoc-cdr
                                    :datestamp
                                    (opinion-by-id iid)))))
                              urls)))
    (length urls)))

;; new users

(defun this-month? (ltime)
  (let ((now (local-time:now)))
    (and (eql (local-time:timestamp-month now) (local-time:timestamp-month ltime))
         (eql (local-time:timestamp-year now) (local-time:timestamp-year ltime)))))

(defun how-many-signups-this-month? ()
  (let ((ulist (userfig:map-users
                (lambda (u settings)
                  (declare (ignore u))
                  (let ((udate
                          (and settings (gethash 'webhax-user:signed-up settings))))
                    (and udate (this-month? udate)))))))
    (length (remove-if-not #'identity ulist))))

;; recent server errors
;; - probably do this from client activated script

