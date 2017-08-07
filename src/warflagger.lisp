;;;; warflagger.lisp

(in-package #:warflagger)

;;; "warflagger" goes here. Hacks and glory await!


(defun offline-connect ()
  (clsql:connect *db-connect-spec* :database-type *db-connect-type*))

;    def target_text(self, op):
;        t = self.urls.get(op['target'], None)
;        if t:
;            return t.get('comment', None)
;        else:
;            return grab_text(op['target'])

;;;FIXME: Needs to attempt to load externals and test.
;;;FIXME: Should error if url not found anywhere?
(defun is-location-opinml? (url)
  (or (exists (select (colm 'id) :from (tabl 'opinion)
		      :where (sql-= (colm 'url) url)))
      (not (and (is-cached url) (old-page-available url)))))

