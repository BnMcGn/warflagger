#!/usr/bin/env python

import sys
import string 
import urllib2
from BeautifulSoup import BeautifulSoup
import os 
from pattern.web import plaintext
from shutil import move

import gadgets
from filelock import FileLock, FailLock


#FIXME: At least mildly evil...
cache_tmp = ''
cachepath = ''

#FIXME: cache_loc was for a different situation, but lots of stuff refers
# to it, so not replacing now. Clean up some time....
def cache_loc(url):
    assert len(cachepath)
    return cachepath 

def page_loc(url):
    return cache_loc(url) + 'page.html' + cache_tmp

def text_loc(url):
    return cache_loc(url) + 'text' + cache_tmp

def title_loc(url):
    return cache_loc(url) + 'title' + cache_tmp

def failure_loc(url):
    return cache_loc(url) + 'failed'

def get_url_fh(url):
    """Returns a filehandle to the opened URL"""
    redhand = urllib2.HTTPRedirectHandler()
    cookhand = urllib2.HTTPCookieProcessor()
    opener = urllib2.build_opener(redhand, cookhand)
    return opener.open(url, timeout=30)

def page2text(url):
    loc = cache_loc(url)
    fh = open(page_loc(url))
    fh2 = open(text_loc(url), 'w')
    text = plaintext("".join(fh))
    text = text.encode('utf-8')
    fh2.write(text)
    fh.close()
    fh2.close()

def extract_title(url):
    loc = cache_loc(url)
    page = open(page_loc(url))
    soup = BeautifulSoup(page.read())
    title = soup.find('title')
    tfile = open(title_loc(url), 'w')
    title = title.string.encode('utf-8')
    tfile.write(title)
    page.close()
    tfile.close()

def do_page_save(url):
    fname = tname = titlename = None
    if os.path.exists(failure_loc(url)):
        os.remove(failure_loc(url))
    with FailLock(cache_loc(url)+'processing') as lck:
        if lck.failed:
            return
        with gadgets.vars_set(globals(), cache_tmp='.tmp'):
            fname = page_loc(url) 
            try:
                uh = get_url_fh(url)
            except:
                gadgets.string_to_file(str(sys.exc_info()[1]), 
                                        failure_loc(url))
                return False
            fh = open(fname, 'w')
            for ln in uh:
                fh.write(ln)
            fh.close()
            uh.close()
            page2text(url)
            extract_title(url)
            tname = text_loc(url)
            titlename = title_loc(url)
        with FileLock(cache_loc(url)+'main') as lck2:
            move(fname, page_loc(url))
            move(tname, text_loc(url))
            move(titlename, title_loc(url))

cachepath = sys.argv[1]
if not cachepath.endswith("/"):
    cachepath += "/"
url = sys.stdin.next()
do_page_save(url)

