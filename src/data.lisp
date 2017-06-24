(in-package :warflagger)

;FIXME: This should be stored in a format readable by both lisp and python.

(defparameter *flag-types-source*

;;; Negative:
  '((:spam
     "For the obvious use on any comments that contain out of place marketing. It can also be used on URLs that are referred to by spam emails. Don't use it for offtopic posts. There is a flag for that."

     :inflammatory
     "For content that is inflammatory or offensive in tone, 
especially if you feel that its point could have been made more gently."

     :disagree
     "A generic way to say \"I think you are wrong\"."

     :dislike
     "The most generic downvote. This one should not be taken too seriously. Some people will use it for decluttering and other sorting tasks."

     :obscene
     "For off-color or coarse language."

     :disturbing
     "Warning that a link is pornographic, disturbing, or otherwise not safe for work."

     :already-answered
     "If you feel that a point has been answered well in another place, use this flag's reference field to point to the existing answer, rather than restating it
in place. Argument can continue at the site of the existing answer. Using this
tag will help prevent redundant discussions all over the place."

     :logical-fallacy
     "Use this to point out a logical fallacy. It's good form to explain the
problem in your comment. Note that there are individual flags for some of the
more common logical fallacies. Try to use them where appropriate instead of this
one."

     :ad-hominem
     "Identifies cases where a personal attack is being used to avoid an opponent's
point."

     :from-authority
     "When a person is presenting a source as authorative without questioning its
statements. Also when a source is being dismissed because it is not a recognized
authority."

     :needs-reference
     "Any time that a vague claim has been made, such as \"Studies indicate...\".
This is your way of saying \"Put up or shut up\" to an opponent. It is a way
to challenge unquestioned assumptions. Note that the VoteValue field of this
flag can be used to change it's level of hostility. Using a negative value indicates that an opponent's point will not be allowed to stand without further evidence. It says, \"There's no way that I am going to believe you until I have seen your sources and gone over them with a fine tooth comb\". Placing a zero in this field, on the other hand, indicates a polite request for more information."

     :raise-question
     "When you feel that a point has not been covered. For example, when a reporter
has failed to explore the background and associations of a source. When an
angle that would derail an argument has been avoided. With the VoteValue set to
neutral (0) this flag can indicate simple curiousity; this can be the voice of
the child asking \"Why?\" or a launch into \"I wonder if...\" type of speculation.
Applied to research articles it is a way to denote areas of possible future
exploration.")

;;; Positive:

    (:funny
     "For the evident use. Note: Some people don't feel that the upvoting of Funny
 comments adds to a discussion, therefore they may regard a positive Funny vote as a proxy for Offtopic."

     :agree
     "Generic agreement: When you think that something is right."

     :like
     "Even more generic approval: Use this for things that are a matter of taste more than a matter of fact or opinion."

     :interesting
     "Use this when you wish to indicate that something is worth noting, even if you aren't ready to agree with it. Interesting flags used with a negative vote value will still raise the profile of a target. It can be used to point out negative examples.")

;;; SpecialClaims

    (:eye-witness
     "You are declaring yourself to be a firsthand witness to a statement. You may set your VoteValue to register confirmation or contradiction of the
statement."

     :am-qualified
     "A generic claim that your opinion deserves special weight, for example, if
you are a professional in a field under discussion. You should not
expect your opinion to be automatically accepted because of this flag. In fact,
it serves as an invitation for the community to verify your qualifications."

     :second-hand
     "This is meant for situations where you have spoken to a witness of an event. If a NeedsReference or RaiseQuestion flag has been raised on a point and you are able to interview a source first hand knowledge to resolve the question, then you can use this flag to indicate that you have done so."

     :dislosure
     "If you wish to let people know that you have a possible conflict of interest that could inform your perspective. In general, this should not be too
important. Everyone should assume that any poster may have undisclosed
biases and should evaluate statements on their own merit. Disclosure is
therefore left as an option for the individual.")

;;; Custodial:

    (:redundant
     "This flag is for decluttering. It won't have a negative effect on other people's scores. It only means that it's content has been better stated elsewhere. It's polite to put a link to your prefered statement in the Reference
field."

     :out-of-date
     "Some comments are going to need pruning as time progresses. When appropriate, add a link to updated statements using the Reference field."

     :retraction
     "This flag is only valid for use against your own posts. Use it to acknowledge
that you were wrong or out of line about something. This is a little stronger
than using PointTaken, which might indicate partial agreement. It can also be
used for cleaning up accidental posts."

     :correction
     "Use this on spelling and grammatical errors. WARNING: some reader systems may use this flag to automatically update other people's posts. It is therefore possible to use this flag to tamper with the meaning of them. This is not
the purpose of this flag. It is meant for innocuous changes. Subversive use may
result in the censure of the community!"

     :update
     "Add information to an opinion, for example, additional reference fields or
excerpt fields."

     :incorrect-flag
     "Use this when someone has misused a flag or votevalue, and has evidently done
so out of ignorance. This warns reader systems to ignore comments that might
throw the discussion tree out of whack."

     :flag-abuse
     "A very serious claim. We can expect that in partisan wrath, people will try to discredit others by misusing certain flags, eg. by labelling an honest opinion as Spam or using the Correction flag to change another person's words. This flag is a yell out that someone is fighting dirty. It's a call for the community to investigate."

     :offtopic
     "When you feel that a comment doesn't belong in a discussion. Different
 people have different levels of tolerance for offtopic discussion, and will set
 their readers to allow or ignore it."

     :arcane
     "Use this flag when you check out of a conversation because it is
 deteriorating into name calling or getting trapped in details."

     :same-thing
     "This flag ties two URLs with identical or similar content together. It
 merges the discussions under them so that points don't have to be restated in
 multiple places all over the internet.")))

(defparameter *flag-categories*
 '("Negative" "Positive" "SpecialClaims" "Custodial"))

(defparameter *flag-labels*
  '(("Spam" "Inflammatory" "Disagree" "Dislike" "Obscene" "Disturbing"
     "AlreadyAnswered" "LogicalFallacy" "AdHominem" "FromAuthority"
     "NeedsReference" "RaiseQuestion")
    ("Funny" "Agree" "Like" "Interesting")
    ("EyeWitness" "AmQualified" "SecondHand" "Disclosure")
    ("Redundant" "OutOfDate" "Retraction" "Correction" "Update" "IncorrectFlag"
     "FlagAbuse" "Offtopic" "Arcane" "SameThing")))

;;; for each type: whether respectively, negative, neutral, positive vote values
;;; are allowed for an opinion type. Last field is the default
;;; [True, True, False, 1] allows neg and neutral values with neutral as the
;;; default

;;FIXME: out of date
(defparameter *vote-ranges*
  '((t nil nil 0) (t nil nil 0) (t nil nil 0) (t nil nil 0)
    (t nil nil 0) (t nil nil 0) (t t nil 0) (t nil nil 0)
    (t nil nil 0) (t nil nil 0) (t t nil 0) (t t nil 0)
    (nil nil t 2) (nil nil t 2) (nil t t 2) (nil t t 2)
    (nil nil t 2) (nil nil t 2) (t t t 1) (t t t 1)
    (t t t 1) (t t t 1) (t t nil 1) (nil t nil 1)
    (nil t nil 1) (nil t nil 1) (nil t nil 1) (nil t nil 1)
    (t t nil 0) (t t nil 0) (t t nil 1) (t t nil 1)
    (nil t nil 1)))

