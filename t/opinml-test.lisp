(in-package #:test-warflagger)

(defparameter *sample-comment*
  "This is a #test of hashtags.

This is an [URL](http://no.where.com:0016500/things#and?more=things&even=more)asdf)x
 #(bad-directive)
#(more-bad)
#(unknown-directive might have contents)
#(vote-value 0)")

(defparameter *opinion-store* nil)
(defparameter *opinion-tree* nil)
(defparameter *score-script* nil)
(defparameter *subjective* nil)
(defparameter *rooturl-text* nil)

(def-suite test-opinml
  :description "WF tests for reading opinml"
  :in wf-tests)

(in-suite test-opinml)

(defun init-test-opinml ()
  ;;Rob the content from the file, then hijack the url.
  (unless (gethash *target* *byurl*)
    (update-page *testurl*)
    (let ((id (gethash *testurl* *byurl*)))
      (setf (gethash id *bynum*) (list *target*))
      (setf (gethash *target* *byurl*) id)
      (remhash *testurl* *byurl*)))

  (let* ((opin-path (asdf:system-relative-pathname 'warflagger "t/opinions/"))
         (rtext (try-awhile
                 (lambda () (gadgets:tryit (wf/text:grab-text *target*)))
                 :sleep 0.5
                 :wait 8.0))
         ;;FIXME: Should handle the illegal token better, not just drop the opinion
         (data (handler-bind ((proto:illegal-token #'wf/ipfs:skip-opinion))
                 (wf/ipfs::objective-data-for-dir opin-path)))
         (more-data (warflagger:execute-score-script data)))
    (setf *opinion-store* (getf data :opinion-store))
    (setf *opinion-tree* (getf data :opinion-tree))
    (setf *score-script* (getf data :score-script))
    (setf *rooturl-text* rtext)
    (setf *subjective* more-data)))

(test opinion-with-excerpt "Excerpt opinion"
 (let* ((iid "bafkreiab3ysddpms3i4hzfx2oyr42ysm6fmpdd2zm3qug73n4g3tz5zsge")
        (opinion (gethash iid *opinion-store*))
        (text-position (assoc-cdr :text-position opinion)))
   (is (string-equal
        (subseq *rooturl-text* (car text-position) (+ (car text-position) (second text-position)))
        (assoc-cdr :excerpt opinion)))))

(test score-script-results "Score script results for target"
  (let* ((rootres (gethash *target* *subjective*))
         (warstats (warflagger::warstats-from-scsc-results rootres))
         (title-info (warflagger::title-info-from-scsc-results *subjective* :rooturl *target*))
         (text-info (warflagger::text-info-from-scsc-results *subjective* *target*)))
    (is (member "bafkreiexeatmiyguvk6nk7jfmj7auwdcmvi4jdbrj3vompefc6avppfvbi"
                (gethash :alternatives rootres) :test #'equal))
    (is (member "bafkreicbieru4ms6ixsgggbxhhgdo3f5lhnu7tbhzaxs632ihuszsyvl34"
                (gethash :alternatives rootres) :test #'equal))
    (is (member "#Test" (alexandria:hash-table-keys (gethash :hashtags rootres)) :test #'equal))
    (is (typep (gethash :tree-freshness rootres) 'local-time:timestamp))
    (is (< 0 (nth-value 1 (warflagger:ballot-box-totals (gethash :ballot-box rootres)))))
    (is (< 0 (nth-value 3 (warflagger:ballot-box-totals (gethash :title-ballot-box rootres)))))
    (is (member "#Test" (gethash :hashtags warstats) :test #'equal))
    (is (equal "Saample Web Pge" (gethash :title title-info)))
    (is (equal "bafkreicbieru4ms6ixsgggbxhhgdo3f5lhnu7tbhzaxs632ihuszsyvl34"
               (gethash :title-source title-info)))
    (is (equal "This is the secret real text of the page" (gethash :text text-info)))
    (is (equal "bafkreiexeatmiyguvk6nk7jfmj7auwdcmvi4jdbrj3vompefc6avppfvbi"
               (gethash :text-source text-info)))))

(test opinion-on-opinion "Effects cascade"
  (let* ((rootres (gethash *target* *subjective*))
         (target "bafkreiab3ysddpms3i4hzfx2oyr42ysm6fmpdd2zm3qug73n4g3tz5zsge")
         (targetres (gethash target *subjective*))
         (rwarstats (warflagger::warstats-from-scsc-results rootres))
         (twarstats (warflagger::warstats-from-scsc-results targetres)))
    (is (gethash :negative-inflammatory twarstats))
    (is (not (gethash :negative-inflammatory rwarstats)))))

(test opinion-with-bad-excerpt "Excerpt not found"
  (let* ((iid "bafkreihv55u2tcj2m7dcxlr4twcqhhkafix56mrlfermuwz4tizsw7x7zy")
         (opinion (gethash iid *opinion-store*)))
    (is (assoc-cdr :text-position opinion))
    (is (not (car (assoc-cdr :text-position opinion))))
    (is (not (second (assoc-cdr :text-position opinion))))))
;;FIXME: Not implemented: excerpt that points at different version of text
;;FIXME: Slight errors in excerpt, such as whitespace variations, should be handled.

;;FIXME: loading of nonexistent flag is not currently implemented
;(test opinion-nonexistent-flag "Unrecognized flag"
;  (let* ((iid "bafkreicwk5rilkvzvkvq4pjhfqksehtekggksl6u5wavvo3tdswrpsmnmu"))))

;;TODO: test all of the different flags
;;TODO: test different forms of adversity in votes

;;TODO: test square brackets that are not a markdown link

;;TODO: test multiple references in comment body

(test write-read "serialize-opinion"
  (let* ((iid "bafkreiab3ysddpms3i4hzfx2oyr42ysm6fmpdd2zm3qug73n4g3tz5zsge")
         (opinion (gethash iid *opinion-store*))
         (opinion (remove-if (lambda (x) (eq (car x) :url)) opinion))
         (opstring (warflagger:serialize-opinion opinion))
         (opinion2 (warflagger::deserialize-opinion-from-stream (make-string-input-stream opstring))))
    (is (equal (assoc-cdr :excerpt opinion) (assoc-cdr :excerpt opinion2)))
    (is (equal (assoc-cdr :author opinion) (assoc-cdr :author opinion2)))
    (is (equal (assoc-cdr :comment opinion) (assoc-cdr :comment opinion2)))))
