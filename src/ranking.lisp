(in-package :warflagger)

;;;
;;; ranking.lisp
;;;
;;; Various ranking algorithms and stuff
;;;
;;;

;;Opinion tree for rooturl

(defparameter *age-limit* (encode-time-delta 0 0 0 30))

(defun ratio-of-recentness-factor (ratio)
  "An arbitrary formula to reduce the hotness of a large, older discussion
that is winding down."
  (- 1 (* 0.01 (expt 1.55 (* 100 (- ratio 0.9))))))

(defun calculate-age-ranking (dates)
  ;;FIXME: Should ensure no future dates
  (let* ((total (length dates))
         (utime (get-universal-time))
         (recent
          (collecting
              (dolist (d dates)
                (let ((diff (+ *age-limit*
                               (- (clsql-helper:clsql-date/times->utime d)
                                  utime))))
                  (when (< 0 diff)
                    (collect diff))))))
         (rec-fact
          (ratio-of-recentness-factor (if (zerop (length recent))
                                          0
                                          (/ total (length recent))))))
    (* rec-fact
       (apply #'+ recent))))

(defun number-of-posts-factor (number)
  (or (cdr (first-match (lambda (x)
                          (> (car x) number))
                        '((1 . 0) (2 . 1) (10 . 4) (100 . 6) (200 . 8))))
      10))

(defun calculate-simple-ranking (rootid)
  ;;Can we do anything if rooturl hasn't been traced yet? Should do it?
  (if (rooturl-real-p rootid)
      (let* (;;(rooturl (get-rooturl-by-id rootid))
             (flat-opins
              (select (colm 'id) (colm 'datestamp)
                      :from (tabl 'opinion)
                      :where (sql-= (colm 'rooturl) rootid)
                      :order-by (colm 'datestamp)))
             ;;(opin-tree (opinion-tree-for-rooturl rooturl))
             (quantity-factor (number-of-posts-factor (length flat-opins)))
             (freshness-factor
              (calculate-age-ranking (mapcar #'second flat-opins))))
        (+ quantity-factor freshness-factor))
      0))

(defun get-ranked-rootids ()
  (let* ((ids (get-column 'rooturl 'id))
         (scores (mapcar #'calculate-simple-ranking ids)))
    (mapcar #'car
            (sort (remove-if-not (lambda (x) (< 0 (cdr x))) (pairlis ids scores))
                  #'> :key #'cdr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First attempt at opinion evaluation code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;FIXME: Implement me

(defun author-respect (authid)
  "Not yet implemented: how seriously we should take a given author. Factors:
  - Established member:
   - Has been around for some length of time.
   - Has contributed unique, meaningful content (not a bot)
   - Optionally, is a known real person vouched for by multiple other members.
   - If pseudonymous and non-local, digitally signs posts
  - Well behaved:
   - Has not attempted to introduce spam, illegal or deeply offensive material
   - Doesn't misuse the serious flags
   - Isn't excessively factional
   - Treats opponents decently
Some of these factors will obviously affect the respect points more than others."
  (declare (ignorable authid))
  1)

;;;;;;;;;;;;;;;;;;;
;; Axes:
;;  a tool for collating flaggage on a target.

;; A target will, based on opinions posted on it, be scored along a few different
;; axes. When the target is itself an opinion, this score will help to decide how
;; much effect the opinion will have on its target.

;; The axes are:

;; supported
;; Flags: Like Agree Funny (when positive: Interesting Anecdotal)

;; dissed
;; Flags: Dislike Disagree Inflammatory (when negative: Interesting Anecdotal)

;; wrong
;; Flags: AlreadyAnswered LogicalFallacy (when negative EyeWitness AmQualified
;;   SecondHand Evidence) Retraction

;; right/verified
;; Flags: (when positive: EyeWitness AmQualified SecondHand Evidence)

;; problematic
;; Flags: Inflammatory LanguageWarning Disturbing OutOfBounds FlagAbuse Disclosure?
;;   IncorrectFlag?

;; unverified
;; Flags: NeedsReference (when negative: RaiseQuestion)

;; irrelevant
;; Flags: Redundant OutOfDate IncorrectFlag? Offtopic Arcane AlreadyAnswered?

;; referenced - references indicate that this target is significant. Not
;; guaranteed to be positive.

;; quantity - How many people have looked at the target? The flags/looks ratio is
;; an indication of importance. Something with 10 flags in a niche topic is more
;; significant than 20 flags where 10x the people have viewed it.

;; Some of these axes are opposites. It's important to sum up opposite sentiments
;; rather than just cancelling them out, because they can have various causes:
;; - One portion of the target may be accurate, another statement within it may be
;; in error. This could indicate a faulty chain of reasoning.
;; - The target may represent a polarizing statement in a divisive fight. This sort
;; of statement should be carefully considered, not discarded.
;;

;;FIXME: Handling of NeedsReference and RaiseQuestion flags
;; These flags need some custom handling. Take, for example, a NeedsReference flag
;; with a -1 VoteValue. If someone adds a reference, say with the Evidence flag,
;; to the NeedsReference flag, it should have a VoteValue of -1 indicating a
;; neutralization of the need for a reference. Assuming that the reference is
;; good it should be effectively applied to the target as a +1 Evidence flag in
;; place of the NeedsReference flag.

;; Adding a +1 Evidence flag to the -1 NeedsReference flag will be taken as support
;; for the latter flag. You're saying that you are presenting additional reasons
;; that the grandparent target needs evidence. Thus your +1 Evidence flag
;; translates into a -1 Evidence to the toplevel target.

;; This inversion over the NeedsReference and RaiseQuestion flags will probably
;; cause some confusion to users. There should be a UI cue or two that will
;; help with clarity.


;;These functions return a factor which indicates the category of effect that an
;; opinion will have, based on its flag type. This factor will be combined with
;; things like the score of the opinion and reputation of the author to come up
;; with the effect that the opinion will have on its target.

(defun filter-supported (opinion)
  (let ((vv (assoc-cdr :votevalue opinion))
        (flag (third (assoc :flag opinion))))
    (cond
      ((member flag '(:like :agree :funny)) 1)
      ((and (member flag '(:interesting :anecdotal))
            (< 0 vv))
       1)
      (t 0))))

(defun filter-dissed (opinion)
  (let ((vv (assoc-cdr :votevalue opinion))
        (flag (third (assoc :flag opinion))))
    (cond
      ((member flag '(:dislike :disagree)) 1)
      ((eq flag :inflammatory) (if (> 0 vv) 0.5 0))
      ((and (member flag '(:interesting :anecdotal))
            (< 0 vv))
       1)
      (t 0))))

(defun filter-wrong (opinion)
  (let ((vv (assoc-cdr :votevalue opinion))
        (flag (third (assoc :flag opinion))))
    (cond
      ((member flag '(:already-answered :logical-fallacy)) 1)
      ((and (member flag '(:eyewitness :am-qualified :second-hand :evidence))
            (> 0 vv))
       1)
      (t 0))))

(defun filter-right (opinion)
  (let ((vv (assoc-cdr :votevalue opinion))
        (flag (third (assoc :flag opinion))))
    (cond
      ((and (member flag '(:eye-witness :am-qualified :second-hand :evidence))
            (< 0 vv))
       1)
      (t 0))))

(defun filter-problematic (opinion)
  (let ((flag (third (assoc :flag opinion))))
    (cond
      ((member flag '(:incorrect-flag :language-warning :disclosure)) 1)
      ((eq flag :Inflammatory) 0.5) ;because half goes under dissed
      ((member flag '(:flag-abuse :disturbing)) 4)
      ((eq flag :out-of-bounds) 8)
      (t 0))))

(defun filter-unverified (opinion)
  (let ((vv (assoc-cdr :votevalue opinion))
        (flag (third (assoc :flag opinion))))
    (cond
      ((eq flag :needs-reference) 1)
      ((and (eq flag :raise-question)
            (> 0 vv))
       1)
      (t 0))))

(defun filter-irrelevant (opinion)
  (let ((flag (third (assoc :flag opinion))))
    (cond
      ((member flag
               '(:redundant :out-of-date :incorrect-flag :already-answered)) 1)
      ((member flag '(:off-topic :arcane)) 0.5)
      (t 0))))

;;FIXME: No check for multiple flags from one author
;;FIXME: Need faction detection.
(defun collect-axis-sum (filter optree)
  (let ((effect 0)
        (controversy 0))
    (dolist (op optree)
      (let* ((opinion (opinion-from-id (car op)))
             (factor (funcall filter opinion)))
        (when (< 0 factor)
          (multiple-value-bind (e c) (opinion-effect op :opinion opinion)
            (incf effect (* factor e))
            (incf controversy (* factor c))))))
    (list effect controversy)))

(defun calculate-opinion-interest (axis-data)
  "Tries to determine how much interest is being shown in a target based on how many people have replied to it vs. how many have looked at it. An opinion with few replies may be of low interest in a well-trodden area or it may be significant in a niche field. We would like to detect the difference. This is harder than it looks. Should we count the immediate replies to the target, or the whole tree of replies below it? Should we count the number of opinions or the number of participants?"
  (declare (ignore axis-data))
  ;;FIXME: Not implemented
  ;;(/ (getf axis-data :looks) (getf axis-data :replies-total))
  1)

(defun calculate-opinion-effect (axis-data opinion)
  (labels ((getx (key)
             (car (getf axis-data key))))
    (let* ((likedness (- (getx :x-supported) (getx :x-dissed)))
           (rightness (- (getx :x-right) (getx :x-wrong)))
           (worries (- 0 (getx :x-problematic)
                       (getx :x-unverified)
                       (getx :x-irrelevant)))
           (initial (opinion-initial-effect :opinion opinion))
           (raw-score (+ initial likedness (* 2 rightness) worries)))
      (if (< 0 raw-score)
          (* raw-score (calculate-opinion-interest axis-data))
          0))))

(defun calculate-opinion-controversy (axis-data)
  (labels ((getx (key)
             (car (getf axis-data key))))
    (let ((positive (+ (getx :x-supported) (getx :x-right)))
          (negative (+ (getx :x-wrong) (getx :x-dissed) (getx :x-problematic)
                       (getx :x-irrelevant))))
      ;; Pick a number, any number... see how it works.
      (+ (getx :x-unverified) (* 0.1 positive negative)))))

;;FIXME: There should be some form of time calculation on effect. Opinions that are
;; new shouldn't have the same effect as well seasoned opinions. Otherwise people
;; can troll by spiking a previously settled target. Also, there should be a
;; warning mechanism to indicate that a target's rating is going to swing. People
;; should be given a chance to respond when a swing is about to occur.

(defun opinion-initial-effect (&key id opinion)
  "Set up the initial effect, or score, that an opinion will have before replies have been factored in."
  (let ((opinion (or opinion (opinion-from-id id))))
    ;;FIXME: author-respect maybe should vary for different flags?
    (author-respect (assoc-cdr :author-id opinion))))

;;FIXME: Opinion-effect is recurrent. Eventually a discussion will be larger than
;; the stack limit.
(defun %opinion-effect (optree &key opinion)
  (let* ((opinion (or opinion (opinion-from-id (car optree))))
         (axdat (opinion-axis-data optree))
         (effect (calculate-opinion-effect axdat opinion))
         (controv (calculate-opinion-controversy axdat)))
    (when (hash-table-p *opinion-effect-cache*)
      (setf (gethash (car optree) *opinion-effect-cache*)
            (list* :effect effect :controversy controv axdat)))
    (values effect controv)))

(defparameter *opinion-effect-cache* nil)
(defun opinion-effect (optree &key opinion)
  (if (hash-table-p *opinion-effect-cache*)
      (if-let ((dat (gethash (car optree) *opinion-effect-cache*)))
        (values (getf dat :effect) (getf dat :controversy))
        (%opinion-effect optree :opinion opinion))))

(defun opinion-axis-data (optree)
  (list
   :x-supported (collect-axis-sum #'filter-supported (cdr optree))
   :x-dissed (collect-axis-sum #'filter-dissed (cdr optree))
   :x-wrong (collect-axis-sum #'filter-wrong (cdr optree))
   :x-right (collect-axis-sum #'filter-right (cdr optree))
   :x-problematic (collect-axis-sum #'filter-problematic (cdr optree))
   :x-unverified (collect-axis-sum #'filter-unverified (cdr optree))
   :x-irrelevant (collect-axis-sum #'filter-irrelevant (cdr optree))
   :replies-total (length (flatten (cdr optree)))
   :replies-immediate (length (cdr optree))
   :looks (when (car optree) ;nil if root isn't an opinion.
            (length (get-opinion-looks (car optree))))
   ;;FIXME: Implement the factoring-in of references.
   :referenced 0))

(defun generate-rooturl-warstats (rooturl &key tree)
  (let* ((tree (or tree (opinion-tree-for-rooturl rooturl)))
         (*opinion-effect-cache* (make-hash-table))
         (root-ax (opinion-axis-data (list* nil tree))))
    (setf (gethash :root *opinion-effect-cache*) root-ax)
    *opinion-effect-cache*))
