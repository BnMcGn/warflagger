#!/usr/bin/env python

import sys
import string
import urllib2
import json
from BeautifulSoup import BeautifulSoup, SoupStrainer
import os
from pattern.web import plaintext
from readability import Document
import pyPdf as pp
from shutil import move
from syslog import syslog, openlog, LOG_DEBUG, LOG_INFO

import gadgets
from filelock import FileLock, FailLock

openlog(ident="WF:Textract")

#FIXME: At least mildly evil...
cache_tmp = ''
cachepath = ''

#FIXME: cache_loc was for a different situation, but lots of stuff refers
# to it, so not replacing now. Clean up some time....
def cache_loc(url):
    assert len(cachepath)
    return cachepath

def page_loc(url, utype=None):
    if utype:
        return cache_loc(url) + 'page.' + utype + cache_tmp
    else:
        fname = cache_loc(url) + 'page.html' + cache_tmp
        if os.path.exists(fname):
            return fname
        fname = cache_loc(url) + 'page.pdf' + cache_tmp
        if os.path.exists(fname):
            return fname
        raise ValueError, "Can't calculate page location without type"

def text_loc(url):
    return cache_loc(url) + 'text' + cache_tmp

def title_loc(url):
    return cache_loc(url) + 'title' + cache_tmp

def failure_loc(url):
    return cache_loc(url) + 'failed'

def links_loc(url):
    return cache_loc(url) + 'links'

def get_url_fh(url):
    """Returns a filehandle to the opened URL"""
    redhand = urllib2.HTTPRedirectHandler()
    cookhand = urllib2.HTTPCookieProcessor()
    opener = urllib2.build_opener(redhand, cookhand)
    return opener.open(url, timeout=30)

def get_page_type(ufh):
    t = ufh.info().type
    if "html" in t:
        return "html"
    elif "pdf" in t:
        return "pdf"
    else:
        raise ValueError, "Unknown document type"

def extract_links(page):
    links = BeautifulSoup(page, parseOnlyThese=SoupStrainer('a'))
    links = [l for l in links if l.has_key('href')]
    return [(l.get('href'), l.getText()) for l in links]

def process_links(linktext):
    return json.dumps(extract_links(linktext))

def page2text(url):
    fh = open(page_loc(url))
    #Use readability to get a clean version of the page.
    clean = Document("".join(fh)).summary()
    text = plaintext(clean)
    text = text.encode('utf-8')
    gadgets.string_to_file(text, text_loc(url))
    links = process_links(clean)
    links = links.encode('utf-8')
    gadgets.string_to_file(links, links_loc(url))
    fh.close()

def extract_title(url):
    page = open(page_loc(url))
    soup = BeautifulSoup(page.read())
    title = soup.find('title')
    title = title.string.encode('utf-8')
    gadgets.string_to_file(title, title_loc(url))
    page.close()

def process_html(url):
    page2text(url)
    extract_title(url)

def pdf2text(fh):
    content = ""
    pdf = pp.PdfFileReader(fh)
    for i in range(0, pdf.getNumPages()):
        content += pdf.getPage(i).extractText() + "\n"
    content = " ".join(content.replace(u"\xa0", " ").strip().split())
    return content

def process_pdf(url):
    try:
        text = pdf2text(open(page_loc(url)))
    except:
        text = "Could not extract text from PDF file"
        syslog(LOG_DEBUG, "PDF extract failed: {0}".format(cache_loc(url)))
    gadgets.string_to_file(text.encode('utf-8'), text_loc(url))
    gadgets.string_to_file("", links_loc(url))
    try:
        pdread = pp.PdfFileReader(open(page_loc(url)))
        title = pdread.getDocumentInfo().title
    except:
        title = "No Title Found"
        syslog(LOG_DEBUG, "PDF title not found: {0}".format(cache_loc(url)))
    gadgets.string_to_file(title.encode('utf-8'), title_loc(url))

def do_page_save(url):
    fname = tname = titlename = None
    if os.path.exists(failure_loc(url)):
        os.remove(failure_loc(url))
    with FailLock(cache_loc(url)+'processing') as lck:
        if lck.failed:
            return
        with gadgets.vars_set(globals(), cache_tmp='.tmp'):
            try:
                uh = get_url_fh(url)
            except:
                gadgets.string_to_file(str(sys.exc_info()[1]),
                                        failure_loc(url))
                return False
            utype = get_page_type(uh)
            fname = page_loc(url, utype)
            fh = open(fname, 'w')
            #FIXME: see if this works for pdfs? binary? might be slow?
            for ln in uh:
                fh.write(ln)
            fh.close()
            uh.close()
            if utype == "html":
                process_html(url)
            else:
                process_pdf(url)
            tname = text_loc(url)
            titlename = title_loc(url)
        with FileLock(cache_loc(url)+'main') as lck2:
            move(fname, page_loc(url, utype))
            move(tname, text_loc(url))
            move(titlename, title_loc(url))

cachepath = sys.argv[1]
if not cachepath.endswith("/"):
    cachepath += "/"
url = sys.stdin.next()
syslog(LOG_INFO, "Called for URL:{0}".format(url))
syslog(LOG_DEBUG, "Cachepath: {0}".format(cachepath))
do_page_save(url)

