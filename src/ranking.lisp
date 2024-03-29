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
that is winding down. Drops hotness if less than 10% of opinions are new."
  (min 1 (* ratio (+ 10 ratio))))

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
                                          (/ (length recent) total)))))
    (* rec-fact
       (apply #'+ recent))))

(defun number-of-posts-factor (number)
  (or (cdr (find-if (lambda (x)
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
;; Opinion direction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Opinion direction is a simple indication of whether an opinion is "for" or
;; "against" its target. It is an approximation, mostly for display purposes.
;; It is a binary indicator, avoiding the subtlety of numeric measures like effect
;; and controversy.
;;
;; - This version of direction is entirely calculated from the opinion itself,
;; we aren't counting in response to the opinion or questions of faction.
;; - There is an earlier measure called direction that factored in response. It will
;; hopefully disappear some day.

(defun opinion-direction (opin)
  (let ((vv (assoc-cdr :votevalue opin))
        (flag (second (assoc-cdr :flag opin))))
    (cond
      ((eq 0 vv)
       :neutral)
      ((member flag '(:incorrect-flag :language-warning :redundant :out-of-date
                      :offtopic :arcane :same-thing))
       :neutral)
      ((< 0 vv)
       :pro)
      (t :con))))

(defun opinion-chain-direction (parent-dir child-dir)
  "Attempt to discern whether an item is pro or con something farther up the tree based on relationship to parent opinion. Revert to neutral if either item is ambiguous."
  (cond
    ((eq parent-dir :neutral) :neutral)
    ((eq child-dir :neutral) :neutral)
    ((eq child-dir :con)
     (if (eq parent-dir :con) :pro :con))
    ((eq child-dir :pro)
     (if (eq parent-dir :con) :con :pro))
    (t (error "Shouldn't have reached this"))))

(defun generate-direction-data (tree stor)
  (labels ((proc (tree pardir)
             (dolist (branch tree)
               (let* ((opin (opinion-by-id (car branch)))
                      (opdir (opinion-direction opin))
                      (oprootdir (if pardir
                                     (opinion-chain-direction pardir opdir)
                                     opdir)))
                 (setf (getf (gethash (car branch) stor)
                             :direction)
                       opdir)
                 (setf (getf (gethash (car branch) stor)
                             :direction-on-root)
                       oprootdir)
                 (proc (cdr branch) oprootdir)))))
    (proc tree nil)))

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
;; Flags: NeedsEvidence (when negative: RaiseQuestion)

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

;;FIXME: Handling of NeedsEvidence and RaiseQuestion flags
;; These flags need some custom handling. Take, for example, a NeedsEvidence flag
;; with a -1 VoteValue. If someone adds a reference, say with the Evidence flag,
;; to the NeedsEvidence flag, it should have a VoteValue of -1 indicating a
;; neutralization of the need for a reference. Assuming that the reference is
;; good it should be effectively applied to the target as a +1 Evidence flag in
;; place of the NeedsEvidence flag.

;; Adding a +1 Evidence flag to the -1 NeedsEvidence flag will be taken as support
;; for the latter flag. You're saying that you are presenting additional reasons
;; that the grandparent target needs evidence. Thus your +1 Evidence flag
;; translates into a -1 Evidence to the toplevel target.

;; This inversion over the NeedsEvidence and RaiseQuestion flags will probably
;; cause some confusion to users. There should be a UI cue or two that will
;; help with clarity.


;;These functions return a factor which indicates the category of effect that an
;; opinion will have, based on its flag type. This factor will be combined with
;; things like the score of the opinion and reputation of the author to come up
;; with the effect that the opinion will have on its target.

(defun all-flags ()
  (remove-if-not #'keywordp (alexandria:flatten *flag-types-source*)))

(defun filter-supported (opinion)
  (let ((vv (assoc-cdr :votevalue opinion))
        (flag (third (assoc :flag opinion))))
    (cond
      ((member flag '(:like :agree :funny)) 1)
      ((and (member flag '(:interesting :anecdotal))
            (< 0 vv))
       1)
      ((and (not (member flag (all-flags)))
            (< 0 vv)) 1)
      (t 0))))

(defun filter-dissed (opinion)
  (let ((vv (assoc-cdr :votevalue opinion))
        (flag (third (assoc :flag opinion))))
    (cond
      ((member flag '(:dislike :disagree)) 1)
      ((eq flag :inflammatory) (if (> 0 vv) 0.5 0))
      ((and (member flag '(:interesting :anecdotal))
            (> 0 vv))
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
      ((eq flag :spam) 2)
      ((eq flag :inflammatory) 0.5) ;because half goes under dissed
      ((member flag '(:flag-abuse :disturbing)) 4)
      ((eq flag :out-of-bounds) 8)
      (t 0))))

(defun filter-unverified (opinion)
  (let ((vv (assoc-cdr :votevalue opinion))
        (flag (third (assoc :flag opinion))))
    (cond
      ((eq flag :needs-evidence) 1)
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
;;FIXME: Want responsibility tracing. Each opinion is reponsible for what percent
;; of the outcome. Need a rework to support this.
(defun collect-axis-sum (filter optree)
  (let ((effect 0)
        (controversy 0))
    (dolist (op optree)
      (let* ((opinion (opinion-by-id (car op)))
             (factor (funcall filter opinion)))
        ;;FIXME: This is inefficient, but makes sure all opinions in tree get
        ;; processed. Find better way.
        (when t ;(< 0 factor)
          (multiple-value-bind (e c) (opinion-effect op :opinion opinion)
            (incf effect (* factor e))
            (incf controversy (* factor c))))))
    (list effect controversy)))

(defun calculate-axdat-interest (axis-data)
  "Tries to determine how much interest is being shown in a target based on how many people have replied to it vs. how many have looked at it. An opinion with few replies may be of low interest in a well-trodden area or it may be significant in a niche field. We would like to detect the difference. This is harder than it looks. Should we count the immediate replies to the target, or the whole tree of replies below it? Should we count the number of opinions or the number of participants?"
  (declare (ignore axis-data))
  ;;FIXME: Not implemented
  ;;(/ (getf axis-data :looks) (getf axis-data :replies-total))
  1)

(defun calculate-axdat-effect (axis-data opinion)
  (labels ((getx (key)
             (car (getf axis-data key))))
    (let* ((likedness (- (getx :x-supported) (getx :x-dissed)))
           (rightness (- (getx :x-right) (getx :x-wrong)))
           (worries (- 0 (getx :x-problematic)
                       (getx :x-unverified)
                       (getx :x-irrelevant)))
           (initial (if opinion
                        (opinion-initial-effect :opinion opinion)
                        ;;FIXME: This constant should be 1. documented 2. elsewhere
                        ;;represents rooturl initial effect
                        1))
           (raw-score (+ initial likedness (* 2 rightness) worries
                         (getf axis-data :referenced-effect))))
      (if (< 0 raw-score)
          (* raw-score (calculate-axdat-interest axis-data))
          0))))

(defun calculate-axdat-controversy (axis-data)
  (labels ((getx (key)
             (car (getf axis-data key))))
    (let* ((positive (+ (getx :x-supported) (getx :x-right)
                       (getf axis-data :referenced-effect)))
           (negative (+ (getx :x-wrong) (getx :x-dissed) (getx :x-problematic)
                        (getx :x-irrelevant)))
           (larger (max positive negative)))
      ;; Pick a number, any number... see how it works.
      (+ (getx :x-unverified)
         (if (< 0 larger)
             (* 1.2
                (relative-to-range 0 (max positive negative) (min positive negative))
                (+ positive negative))
             0)))))

;;FIXME: There should be some form of time calculation on effect. Opinions that are
;; new shouldn't have the same effect as well seasoned opinions. Otherwise people
;; can troll by spiking a previously settled target. Also, there should be a
;; warning mechanism to indicate that a target's rating is going to swing. People
;; should be given a chance to respond when a swing is about to occur.

(defun opinion-initial-effect (&key id opinion)
  "Set up the initial effect, or score, that an opinion will have before replies have been factored in."
  (let ((opinion (or opinion (opinion-by-id id))))
    ;;FIXME: author-respect maybe should vary for different flags?
    (author-respect (assoc-cdr :author-id opinion))))

;;FIXME: Opinion-effect is recurrent. Eventually a discussion will be larger than
;; the stack limit.
(defun %opinion-effect (optree &key opinion)
  (let* ((opinion (or opinion (opinion-by-id (car optree))))
         (axdat (opinion-axis-data optree))
         (effect (calculate-axdat-effect axdat opinion))
         (controv (calculate-axdat-controversy axdat)))
    ;;FIXME: Can't have both reference and referenced becaese it creates a loop. Maybe re-enable when
    ;; User influence is factored in. Should prevent the problem.
    ;;     (ref-effects (calculate-reference-effects
    ;;                   effect controv
    ;;                   (getf axdat :reference-effect-main)
    ;;                   (getf axdat :reference-controversy-main))))
    ;;(setf effect (+ effect (car ref-effects) (getf axdat :reference-effect-extra)))
    ;;(setf controv
    ;;      (+ controv (second ref-effects) (getf axdat :reference-controversy-extra)))
    (when (hash-table-p *opinion-effect-cache*)
      (setf (gethash (car optree) *opinion-effect-cache*)
            (list* :effect effect :controversy controv axdat)))
    (values effect controv)))

(defparameter *opinion-effect-cache* nil)
(defun opinion-effect (optree &key opinion)
  "Opinion keyword is a lookup cache. No special meaning."
  ;;FIXME: Why if? Some form of typo?
  (if (hash-table-p *opinion-effect-cache*)
      (if-let ((dat (gethash (car optree) *opinion-effect-cache*)))
        (values (getf dat :effect) (getf dat :controversy))
        (%opinion-effect optree :opinion opinion))))

(defun calculate-reference-effects (effect controv ref-effect ref-controv)
  "References will obviously have their own opinions, ranking, controversy and effect. We can't just take the reputation of the reference and import it into the current conversation. The referrer may be abusing the reference in any number of creative ways. The first two parameters are the values of the reference. The second two are from the discussion that resulted from the reference being posted. We are attempting to come up with a reasonable value that reflects the quality of the original reference with its applicability to the current discussion. Much wildly arbitrary guesswork follows."
  (list (* (or effect 0) (or ref-effect 0)) (* (or controv 0) (or ref-controv 0))))

;;FIXME: Should return nil on no opinions?
(defun tree-freshness (optree)
  "Find the most recent date in the replies."
  (when-let ((dates (mapcar (lambda (id) (assoc-cdr :datestamp (opinion-by-id id)))
                            (remove-if-not #'integerp (flatten optree)))))
    (apply #'clsql:time-max dates)))

(defun referenced-effect (refopinids)
  "The total approval effect of incoming reference opinions. All effects are assumed to be positive because the opins are using the reference as a resource."
  (reduce #'+
          (mapcar (lambda (id)
                    (getf (request-warstats
                           (assoc-cdr :iid (opinion-by-id id)))
                          :effect))
                  refopinids)))

(defun reference-data (optree)
  ;;When we are getting reference data for a rooturl, car will be nil
  (let ((opinion (and (car optree) (opinion-by-id (car optree))))
        (effect 0)
        (controv 0)
        (meffect 0)
        (mcontrov 0))
    (when-let* ((link (assoc-cdr :reference opinion))
                (refdat (hu:hget *reference-list* (list link :warstats))))
      (setf meffect (getf refdat :effect)
            mcontrov (getf refdat :controversy)))
    (dolist (tree (cdr optree))
      (let ((replyid (car tree)))
        (when (system-generated-p replyid)
          (let* ((results (multiple-value-list (opinion-effect tree)))
                 (refdat
                  (hu:hget *reference-list*
                           (list (assoc-cdr :reference (opinion-by-id (car tree)))
                                 :warstats)))
                 (reffects (calculate-reference-effects
                            (car results) (second results)
                            (getf refdat :effect) (getf refdat :controversy))))
            (incf effect (car reffects)) (incf controv (second reffects))))))
    ;;Main: from the reference slot
    ;;Extra: from comment body
    (let ((referenced (and opinion (get-references-to (assoc-cdr :url opinion)))))
      (list
       :reference-effect-main meffect
       :reference-controversy-main mcontrov
       :reference-effect-extra effect
       :reference-controversy-extra controv
       ;; referenced for rootURL is filled in elsewhere
       :referenced referenced
       :referenced-effect (referenced-effect referenced)))))

(defun opinion-axis-data (optree)
  (list*
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
   :tree-freshness (tree-freshness optree)
   (reference-data optree)))

(defvar *reference-list*)

(defun generate-rooturl-warstats (rooturl &key tree reference-cache)
  (let* ((tree (or tree (opinion-tree-for-rooturl rooturl)))
         (*opinion-effect-cache* (make-hash-table))
         (*reference-list* (or reference-cache
                               (reference-list-for-rooturl rooturl)))
         (root-ax (opinion-axis-data (list* nil tree))))
    (setf (getf root-ax :referenced) (get-references-to rooturl))
    (setf (getf root-ax :referenced-effect)
          (referenced-effect (getf root-ax :referenced)))
    (setf (getf root-ax :effect) (calculate-axdat-effect root-ax nil))
    (setf (getf root-ax :controversy) (calculate-axdat-controversy root-ax))
    (setf (gethash :root *opinion-effect-cache*) root-ax)
    (generate-direction-data tree *opinion-effect-cache*)
    ;;FIXME: Side effect!! Need a better way to cache results. This is just a hack for now.
    (setf (gethash rooturl *warstat-store*) root-ax)
    (gadgets:do-hash-table (id stats *opinion-effect-cache*)
      (unless (eq id :root)
        (let ((opinion (opinion-by-id id)))
          (setf (gethash (assoc-cdr :url opinion) *warstat-store*) stats))))
    *opinion-effect-cache*))
