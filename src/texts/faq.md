## Welcome to WarFlagger

## General

### What is WarFlagger?

WarFlagger is your chance to correct the internet. It allows you to comment on any site on the internet without censorship from the owners.

### How does it work?

WarFlagger is based on OpinML, an open standard file format. It allows you to own your own content, pass it around to others and host it, all without the permission of WarFlagger's owners. 

OpinML is published in units called opinions. With an opinion you can vote or comment on targets. Targets can be other opinions or web pages in general.

Each OpinML opinion has a [Flag](https://github.com/BnMcGn/warflagger/wiki/Flags) field to indicate your intention and an optional [Excerpt](https://github.com/BnMcGn/warflagger/wiki/Excerpts) specifier for annotation of the target.

### Can I post anything that I like?

Yes, if you are self-hosting your content, but expect to be called on it by other community members if you post irresponsibly. WarFlagger reserves the right to refuse to host and/or retransmit content at its sole discretion.

### Won't WarFlagger turn into an echo chamber or get taken over by correctness nazis or voting blocs or be bought out by Big Money?

All of these are valid concerns. For more things to worry about, plus possible solutions to them, please see the [scenarios](https://github.com/BnMcGn/warflagger/wiki/Scenarios) section of the WarFlagger Wiki.

WarFlagger is designed to extract useful discussion from knock-down-drag-out internet flame wars. It aims to be resilient to all sorts of partisan savagery and dirty tricks. It has a few layers of defense.

The final defense is a ranking and moderation system that is completely open and hackable. You can choose which community members you respect, choose your own ranking algorithm, or ultimately download the raw source of the discussion and process it on your own machine. \[Note: not implemented at this point.\]

## Usage

### What flags are available in OpinML?

They can be found [here](/flags/) with descriptions.

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

While its goals are analogous to some things in the court system, OpinML is not intended for any such use. For one, it will take significant effort to work out all the possible methods of subversion and causes of inaccuracy in OpinML evidence. Perhaps it will some day provide the inspiration for a legal evidence markup system.

### What other possible uses do you foresee for WarFlagger/OpinML?

Citizen reporting and media verification. The **EyeWitness** and **SecondHand** flags were made with this use in mind.




