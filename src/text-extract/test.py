#!/usr/bin/env python

fh = open('/home/ben/dump.txt', 'w')
import sys
for ln in sys.stdin:
    print >> fh, "here"
    print >> fh, ln
print >> fh, "input closed"
fh.close()

