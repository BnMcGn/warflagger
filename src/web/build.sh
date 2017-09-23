#!/bin/sh

mv warflagger-server.old warflagger-server.older
mv warflagger-server.fcgi warflagger-server.old

sbcl --load build.lisp
