## Welcome to Warflagger

Hi friends,

Thank you for participating in the warflagger project beta.

## General
### What is warflagger?

Warflagger is an attempt to tame acrimonious debate on the internet. It takes inspiration from user moderated sites such as slashdot, stackexchange and reddit.

### How does it differ from these sites?
(Warning: I'm not as familiar with some of these sites as I should be. In some cases I'm going on what other people have said about them.)

Stackexchange: Stackexchange is designed for clear questions with a single best answer. Warflagger will tackle messier discussions. 

Slashdot: The slashdot comment system comes close to the goals of warflagger. Warflagger could be described as slashdot comments for the whole internet. Warflagger differs from slashdot by being based on an open, distributable comment format called OpinML. This allows for the complete detachment of discussion from any single owning entity. It will allow you, the end user, to refactor the raw debate by selecting your own trusted moderators and meta-moderators.

Reddit: Reddit is based on a straight forward up/down vote system. Warflagger is at the other end of the complexity spectrum. OpinML comments have an excerpt field to allow commenters to pinpoint the item of discussion in the source text. There is a flag field - like that of slashdot - to specify the nature of your disagreement or approval. There is also a votevalue field, to further tune what you mean to say.
Reddit, on the vote scoring side, is (AFAIK) a simple democracy. If the majority vote something up it goes up. If most people dislike it, it disappears. Warflagger acknowledges that some discussions will never reach consensus and that the minority should be heard. It is meant for cases of idealogical dispute, where partisan debaters are expected not only to eternally disagree, but also to maliciously sabotage the debate. (Note: at the time of this writing, the warflagger scoring system has not been implemented. Even when implemented, I don't expect it to be initially invulnerable.)

Wikipedia: Wikipedia's goal is to present all the facts on a topic in a professional manner, ie. with neutrality of tone and proper references. Unfortunately it has suffered claims of inaccuracy and bias. Warflagger can act as a layer on top of wikipedia pages. It can present a precise view of the level of agreement, verification, conflict or doubt about each statement on a page. Users can drill down to the best arguments that each side of a conflict makes, and to the best rebuttals of those arguments.

Snopes: Warflagger could be described as a crowdsourced snopes.

### What does OpinML look like?

Here's a sample:


    @prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    @prefix owl:  <http://www.w3.org/2002/07/owl#> .
    
    @prefix opinneg: <http://opinml.org/2013/01/opinML/opinions/negative#> .
    @prefix opin: <http://opinml.org/2013/01/opinML/util#> .
    @prefix : <http://warflagger.net/sample/sample.n3#> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .
    
    <mailto:sample@warflagger.net> opin:opinion :osn-12345 .
    :osn-12345 opin:target <http://asdf.com/whatisasdf.html> ;
    	opin:excerpt "asdf is a four letter word." ;
    	opin:flag opinneg:Disagree ;
    	opin:votevalue -1 ;
    	opin:datestamp "2013-09-03 12:50:23.077589" ;
    	opin:comment "asdf is not a word" .


OpinML is a species of RDF, preferably in N3 format. The first two blocks above are header. The actual opinion starts with the mailto on the third line. OpinML files can contain multiple opinions.

## Usage

### What flags are available in OpinML?

They can be found [here](/flag_descriptions/) with descriptions.

### What is the reference field for?

Like target, reference is meant to hold an URL. Unlike target, it is optional. Use it to connect something to a target link. 

Examples: 

- If you wish to flag an opinion as **Redundant**, it's a good practice to link to the post that the opinion duplicates.
- When replying to a **NeedsReference** request, you can put an answering URL in the reference field, especially if you are leaving the comment field blank.
- The reference field is mandatory when using the **SameThing** flag to merge two discussions.

## Future

### Can OpinML be used for scientific peer review and research?

Possibly. This would require investigation by people familiar with the needs of the scientific community.

### Can Warflagger be used in place of the legal system for trying cases and evaluating evidence?

While it's goals are analogous to some things in the court system, OpinML is not intended for any such use. For one, it will take significant effort to work out all the possible methods of subversion and causes of inaccuracy in OpinML evidence. Perhaps it will some day provide the inspiration for a legal evidence markup system.

### What other possible uses do you forsee for Warflagger/OpinML?

Citizen reporting and media verification. The **EyeWitness** and **SecondHand** flags were made with this use in mind.

### What features do you plan to add in the future?

- A user definable filtering system. You should be able to specify what users you respect on what issues. Ie. You might find that for UserA you want to see what he flags **Interesting**, but that he is being too picky about **Inflammatory** or **Obscene**. Perhaps he is filtering for his children. Yet when UserB says **Obscene**, you know you don't want to see it. You should be able to select who you are going to respect and how you are going to respect them. User filters should be shareable, so that I can, for example, switch over to my opponents' viewpoint and see how they are seeing the accumulated facts. This should also allow curated collections of opinions to be made by trusted community members.
- Browser extensions that show you if a page has opinions attached, and whether serious debate is raging about its content. Also the possibility of viewing the whole web with OpinML highlighting overlaid on it.
- Some form of trust network, perhaps based on [FOAF](http://foaf-project.org/) to cut down on sockpuppeting

