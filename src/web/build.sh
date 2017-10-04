#!/bin/sh

mv wf-server.old wf-server.older
mv wf-server.img wf-server.old

sbcl --load build.lisp
