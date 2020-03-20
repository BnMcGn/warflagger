#!/usr/bin/env python3

import sys
import string
import urllib.request, urllib.error, urllib.parse
import json
from bs4 import BeautifulSoup, SoupStrainer
import os
from pattern.web import plaintext
from readability import Document
import PyPDF2 as pp
from shutil import move
from syslog import syslog, openlog, LOG_DEBUG, LOG_INFO
import subprocess
from shutil import copyfile

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
        raise ValueError("Can't calculate page location without type")

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
    redhand = urllib.request.HTTPRedirectHandler()
    cookhand = urllib.request.HTTPCookieProcessor()
    opener = urllib.request.build_opener(redhand, cookhand)
    return opener.open(url, timeout=30)

def get_page_type(ufh):
    t = ufh.info().get("Content-Type")
    if "html" in t:
        return "html"
    elif "pdf" in t:
        return "pdf"
    else:
        raise ValueError("Unknown document type")

def extract_links(page):
    links = BeautifulSoup(page, parse_only=SoupStrainer('a'), features="lxml")
    links = [l for l in links if 'href' in l]
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
    soup = BeautifulSoup(page.read(), features='lxml')
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
    content = " ".join(content.replace("\xa0", " ").strip().split())
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

def get_file_type(fname):
    res = subprocess.run(("/usr/bin/file", fname), stdout=subprocess.PIPE)
    retval = str(res.stdout)
    if "HTML document" in retval:
        return "html"
    elif "PDF document" in retval:
        return "pdf"
    else:
        return False

def remote_page_save(url):
    try:
        uh = get_url_fh(url)
    except:
        gadgets.string_to_file(str(sys.exc_info()[1]),
                                failure_loc(url))
        return False
    utype = get_page_type(uh)
    fname = page_loc(url, utype)
    #FIXME: Added 'b' for python3. Don't know if it is the right thing.
    fh = open(fname, 'wb')
    #FIXME: see if this works for pdfs? binary? might be slow?
    for ln in uh:
        fh.write(ln)
    fh.close()
    uh.close()
    return utype, fname

def local_page_save(url, fname):
    if not (os.access(fname, os.R_OK) and os.path.isfile(fname)):
        raise FileNotFoundError("File not available: check name and permissions.")
    ftype = get_file_type(fname)
    oname = page_loc(url, ftype)
    copyfile(fname, oname)
    return ftype, oname

def do_page_save(url, localfname=None):
    fname = tname = titlename = None
    if os.path.exists(failure_loc(url)):
        os.remove(failure_loc(url))
    with FailLock(cache_loc(url)+'processing') as lck:
        if lck.failed:
            return
        with gadgets.vars_set(globals(), cache_tmp='.tmp'):
            if localfname:
                utype, fname = local_page_save(url, localfname)
            else:
                utype, fname = remote_page_save(url)
            if utype == "html":
                process_html(url)
            elif utype == "pdf":
                process_pdf(url)
            else:
                raise ValueError("Unknown document type")
            tname = text_loc(url)
            titlename = title_loc(url)
        with FileLock(cache_loc(url)+'main') as lck2:
            move(fname, page_loc(url, utype))
            move(tname, text_loc(url))
            move(titlename, title_loc(url))

cachepath = sys.argv[1]
if not cachepath.endswith("/"):
    cachepath += "/"
url = next(sys.stdin)
localfname = None
syslog(LOG_INFO, "Called for URL: {0}".format(url))
if len(sys.argv) > 2:
    syslog(LOG_INFO, "File supplied: {0}".format(sys.argv[2]))
    localfname = sys.argv[2]
syslog(LOG_DEBUG, "Cachepath: {0}".format(cachepath))
do_page_save(url, localfname)

