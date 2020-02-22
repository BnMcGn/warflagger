(in-package :cl-user)
(defpackage test-textract
    (:use :cl :prove :wf/text-extract :gadgets))
(in-package :test-textract)

(defparameter *test-page-data*
  (concatenate
   'string
   "file://"
   (directory-namestring (asdf:system-relative-pathname 'wf/text-extract ""))
   "src/textract/test-data.html"))

(plan 2)

(with-temporary-directory (:pathname pname)
  (let ((*cache-path* pname)
        (*bynum* nil)
        (*byurl* nil))

    (isnt (is-cached *test-page-data*))
    (isnt (is-fresh *test-page-data*))

    ))

(finalize)

#|
(plan 11)

(ok (getf (getf *fields* :email) :viewable))
(ok (functionp (getf (getf *fields* :email) :compiled-validator)))

(initialize-user *uname* *fields*)

(let ((udata (get-user-data *uname* *fields*)))
  (is 4 (gethash '(:system :watch-level) udata))
  (is nil (gethash '(:email) udata)))

(let ((udata (get-user-visible-data *uname* *fields*)))
  (is-values (gethash '(:system :watch-level) udata) '(nil nil))
  (ok (not (gethash '(:email) udata))))

(let ((indata (make-hash-table)))
  (setf (gethash :email indata) "asdfasdf")
  (is-error (update-from-user *uname* *fields* indata) 'simple-error)
  (setf (gethash :email indata) "asdf@asd.f")
  (ok (update-from-user *uname* *fields* indata))
  (setf (hu:hget/extend indata '(:system :watch-level)) 0)
  (is-error (update-from-user *uname* *fields* indata) 'simple-error))

(let ((udata (get-user-data *uname* *fields*)))
  (is (gethash '(:email) udata) "asdf@asd.f")
  (is 4 (gethash '(:system :watch-level) udata)))

(finalize)
|#

