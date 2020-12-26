# WarFlagger.net Source Repository

[![CircleCI](https://circleci.com/gh/BnMcGn/warflagger.svg?style=svg)](https://circleci.com/gh/BnMcGn/warflagger)

This is the source code for the [WarFlagger](http://warflagger.net/) website. 

## User Documentation

There is an [introduction](http://warflagger.net/introduction/) that contains links to the rest of the documentation, including a [FAQ](http://warflagger.net/faq/).

## Hacking

At this point the WarFlagger source code is not cleaned up for public consumption. It's not documented and has a bunch of dependencies that are not in quicklisp. Building your own working copy will need a fair amount of fiddling.


You will want to clone these to your local-projects directory:
- [ps-gadgets](https://github.com/BnMcGn/ps-gadgets)
- [sql-stuff](https://github.com/BnMcGn/sql-stuff)
- [thing-labels](https://github.com/BnMcGn/thing-labels)
- [thing-lister](https://github.com/BnMcGn/thing-lister)
- [userfig](https://github.com/BnMcGn/userfig)
- [webhax](https://github.com/BnMcGn/webhax)

You will need to copy the src/local-settings-template.lisp file to src/local-settings.lisp and fill out all of the fields.

Start the application server like this:

    > (ql:quickload 'wf-web)
    
It should come up on port 5000

The text extraction code is written in python. It depends on BeautifulSoup and pattern.
