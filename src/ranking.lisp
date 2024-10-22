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

;;FIXME: unused
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First attempt at opinion evaluation code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;FIXME: Not used anywhere. Left for documentation purposes

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


;;FIXME: There should be some form of time calculation on effect. Opinions that are
;; new shouldn't have the same effect as well seasoned opinions. Otherwise people
;; can troll by spiking a previously settled target. Also, there should be a
;; warning mechanism to indicate that a target's rating is going to swing. People
;; should be given a chance to respond when a swing is about to occur.

