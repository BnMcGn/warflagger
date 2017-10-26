## Welcome to WarFlagger



## General

### What is WarFlagger?

WarFlagger is an attempt to tame acrimonious debate on the internet. It takes inspiration from user moderated sites such as slashdot, stackexchange and reddit.

### What does WarFlagger have to offer that other sites do not?

WarFlagger is built on an open, distributed (or distributable) debate language called [OpinML][1]. OpinML, a species of RDF, has an annotation feature that allows users to point exactly to portions of a text that are causing agreement or contention. WarFlagger can use this information to highlight source texts with appropriate indications of emotional intensity. Opinml also has a flag field modeled after slashdot's comment system that allows users to specify the nature of their objections and approvals. On top of all this, it has a user hackable moderation and meta-moderation system.

[1]:  https://github.com/BnMcGn/warflagger/wiki/OpinML

The [Other Sites](https://github.com/BnMcGn/warflagger/wiki/Other-Sites) section of the WarFlagger Wiki has a comparison with other forum sites.

### Why are distributed, open standard comments important?

OpinML is as open as the internet. You may choose to host your opinml comments on your own server as a feed, host them on WarFlagger or an alternate service, publish them - digitally signed - on freenet or torrents, email them to friends, or keep them private.
Our intent is to create a system where all parties feel that their points of view can be fairly represented, each with all of it best arguments - and their rebuttals - presented clearly.
If a discussion is not completely open, you can't be sure that all points of view have been adequately represented. Someone may be censoring things. WarFlagger intends to remove this excuse: if you have the evidence, present it! 

### How do I know that WarFlagger isn't censoring stuff?

We don't promise not to. There are things that we certainly don't want to host. In exchange, we give you the raw material of the discussion, the ability to host your own content, and the source code to our software.

We may be evil, but you can migrate.

### What if people post intolerant/irreverent/unkind things on WarFlagger?

Our default filters (will be) set up to remove unnecessarily rude or abusive comments. However, we intend to tune our filters to find "voices in the wilderness". If one individual asks a hard question, we want to give him a voice even if 100 people downvote him. Having our cherished beliefs questioned often feels like unkindness, and it has often been the case that charges of intolerance or irreverence have been used to silence legitimate questions.

## Usage

### What flags are available in OpinML?

They can be found [here](/flag_descriptions/) with descriptions.

### Why are there so many flags? Do I have to learn what they are all for?

If in doubt, you are fine to just use the Like and Dislike flags.

### What is the reference field for?

Like target, reference is meant to hold an URL. Unlike target, it is optional. Use it to connect something to a target link. 

Examples: 

- If you wish to flag an opinion as **Redundant**, it's a good practice to link to the post that the opinion duplicates.
- When replying to a **NeedsReference** request, you can put an answering URL in the reference field, especially if you are leaving the comment field blank.
- The reference field is mandatory when using the **SameThing** flag to merge two discussions.

## Future

### Can OpinML be used for scientific peer review and research?

Possibly. This would require investigation by people familiar with the needs of the scientific community.

### Can WarFlagger be used in place of the legal system for trying cases and evaluating evidence?

While it's goals are analogous to some things in the court system, OpinML is not intended for any such use. For one, it will take significant effort to work out all the possible methods of subversion and causes of inaccuracy in OpinML evidence. Perhaps it will some day provide the inspiration for a legal evidence markup system.

### What other possible uses do you forsee for WarFlagger/OpinML?

Citizen reporting and media verification. The **EyeWitness** and **SecondHand** flags were made with this use in mind.


### If the moderation system is hackable, won't people just use it to form their own echo chambers?

Short answer: Yes.

WarFlagger is designed to break down echo chambers. It's dedicated to those who wish to get to the bottom of things, even when that's not comfortable. However, there are reasons to allow some flex on this point.

Hackability is important. Every moderation system implemented thus far has had shortcomings. So will WarFlagger. The best way to arrive at a significantly better system is to allow a number of people with different perspectives to work on the problem. Hence, hackability is more important than the strict lack of echo chambers.

Second, there are people who are never going to be able to handle completely open conversation. While we may see these people as deficient, we can't be sure that they have nothing to contribute. Leave them their echo chambers.

I suspect that biased moderation systems will end up being reasonably detectable. The average user may not see it right away, but more experienced users will be able to point out the flaws. Further discussion can be found in the wiki under [Scenarios](https://github.com/BnMcGn/warflagger/wiki/Scenarios) 


