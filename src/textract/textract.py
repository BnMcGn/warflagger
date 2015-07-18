#from bs4 import BeautifulSoup
import string 
import urllib2
from BeautifulSoup import BeautifulSoup
import os 
from datetime import datetime, timedelta
from pattern.web import plaintext
from shutil import move
import threading
import glob

import gadgets
import local_settings
from generators import dictize
from filelock import FileLock, FailLock

#################
# Cache stuff
#
#################

cachepath = local_settings.cachepath
cachetime = timedelta(hours=1)

def get_url_fh(url):
    """Returns a filehandle to the opened URL"""
    redhand = urllib2.HTTPRedirectHandler()
    cookhand = urllib2.HTTPCookieProcessor()
    opener = urllib2.build_opener(redhand, cookhand)
    return opener.open(url, timeout=30)

#FIXME: At least mildly evil...
cache_tmp = ''

def cache_loc(url):
    return cachepath+str(byurl[url])+'/'

def page_loc(url):
    return cache_loc(url) + 'page.html' + cache_tmp

def text_loc(url):
    return cache_loc(url) + 'text' + cache_tmp

def title_loc(url):
    return cache_loc(url) + 'title' + cache_tmp

def failure_loc(url):
    return cache_loc(url) + 'failed'

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

#################
# Cache index
#
#################

urlindex = cachepath + 'urlindex.inf'

def read_index_file(fh):
    def splitter(fh):
        for ln in fh:
            num, url = ln.split(' ')
            yield (int(num), url.rstrip())
    data = list(splitter(fh))
    bynum = dictize(data, 'append')
    byurl = {v:k for k, v in data}
    return bynum, byurl

def write_index_file(fh, numdict):
    ns = sorted(numdict.keys())
    for n in ns:
        d = numdict[n]
        for itm in d:
            fh.write('%s %s\n' % (n, itm))

if os.path.exists(urlindex):
    fh = open(urlindex)
    bynum, byurl = read_index_file(fh)
    fh.close()
else:
    bynum, byurl = {}, {}

#FIXME: This maybe should have some kind of lock
def get_url_index(url):
    if byurl.has_key(url):
        return byurl[url]
    else:
        keys = bynum.keys()
        newkey = max(keys)+1 if len(keys) else 0
        bynum[newkey]=[url]
        byurl[url]=newkey
        fh = open(urlindex, 'w')
        write_index_file(fh, bynum)
        fh.close()
        return newkey

######################
# Download thread
#
######################

def save_page_to_cache(url):
    """Opens the url and saves it to the page cache, overwriting earlier
    versions of the page"""
    ind = get_url_index(url)
    dpath = cache_loc(url)
    if not os.path.exists(dpath):
        os.mkdir(dpath)
    t = threading.Thread(target=page_save_thread, args=[url])
    t.start()
    return ind

def page_save_thread(url):
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
                gadgets.touch(failure_loc(url))
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

