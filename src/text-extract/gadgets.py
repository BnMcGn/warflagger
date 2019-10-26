

import os
import sys
from multiprocessing import Process
from subprocess import Popen
import time
import string
import itertools
import generators
import re
import collections
import math
from contextlib import contextmanager

#from gizmo import * #text UI thingys

def diff_list(l1, l2):
    """Returns side by side equality test"""
    return [False if i1==i2 else True for (i1, i2) in zip(l1, l2)]

def list_difference(l1, l2, key1=None, key2=None):
    """Like set difference method, but list can have duplicates. If l1 has two
    identical items, and l2 has one of that item, res will have 1 of that item
    left. Sets would remove it"""
    if not key1:
        key1 = lambda x: x
    if not key2:
        key2 = lambda x: x
    res = list(l1)
    keys = list(map(key1, res))
    for x in l2:
        try:
            i = keys.index(key2(x))
            del(keys[i])
            del(res[i])
        except ValueError:
            continue
    return res

def list_intersection(l1, l2, key1=None, key2=None):
    if not key1:
        key1 = lambda x: x
    if not key2:
        key2 = lambda x: x
    ind = generators.dictize([(key1(x), i) for i, x in enumerate(l1)],
                             "append")
    for x in map(key2, l2):
        if x in ind:
            if len(ind[x])==1:
                del(ind[x])
            else:
                del(ind[x][0])
    notfound = set(generators.flatten1(list(ind.values())))
    return [x for i, x in enumerate(l1) if i not in notfound]

def sort_to_buckets(buclist, data_iter, keyfunc=None):
    """Sorts items in data_iter into len(buclist)+1 buckets, with the buclist
    (presumably sorted) providing the dividing points. Items are put into a
    bucket if they are <= the corresponding buclist item, with items greater
    than buclist[-1] put into the final bucket. Items will be compared directly
    with buclist items unless a key func is provided, then buclist[x] >=
    keyfunc(item) will be performed"""
    if not keyfunc:
        keyfunc = lambda x: x
    res = [[] for x in range(len(buclist)+1)]
    for d in data_iter:
        for i, b in enumerate(buclist):
            if b > keyfunc(d):
                res[i].append(d)
                break
        else:
            res[-1].append(d)
    return res

def middlezip(alist):
    lnth = (len(alist)/2)+(len(alist)&1)
    return list(zip(alist[:lnth], alist[-1:lnth-1:-1]))

def indexes(alist, srch):
	"""return a list of all the indexes of srch in alist"""
	res = []
	work = alist
	while srch in work:
		itm = work.index(srch)
		if len(res):
			res.append(itm+res[-1]+1)
		else:
			res.append(itm)
		work=work[itm+1:]
	return res
    
def first_match(iterable, predicate, equals=None,
                return_item=True, return_index=False, reverse=False):
    """Return the first item in iterable that matches predicate. If 
    predicate is None then item equal to value of equals key. Set 
    return_index to get (item, index) tuple. return_item=False, 
    return_index=True to just get index."""
    if predicate is None:
        predicate = lambda x: x == equals
    efunc = generators.denumerate if reverse else enumerate
    for i, x in efunc(iterable):
        if predicate(x):
            if return_item and return_index:
                return (x, i)
            elif return_item:
                return x
            elif return_index:
                return i
    raise ValueError("No match found")

def _could_match_pairs(txt):
    txt = "^{0}$".format(txt.lower())
    return set([''.join(x) for x in generators.window(txt, 2)])

def could_match(str1, str2):
    closenuf = .7
    s1, s2 = list(map(_could_match_pairs, (str1, str2)))
    isec = s1.intersection(s2)
    return fudqual(len(s1), len(isec), closenuf*len(s1)) and \
        fudqual(len(s2), len(isec), closenuf*len(s2))

def digits(n):
	return [int(x) for x in str(n) if not x=='.']

def from_digits(seq):
	return int(''.join(map(str, seq)))
            
def whitespace_index(text):
    for i, t in enumerate(text):
        if t in string.whitespace:
            return i
    raise ValueError("whitespace not found")

def whitespace_rindex(text):
    return len(text)-whitespace_index(reversed(text))-1

def fudqual(a, b, fudge):
    return True if abs(a-b)<fudge else False

def fudgex(alist, key, fudge):
    for i, tm in enumerate(alist):
        if fudqual(key, tm, fudge):
            return i
    raise ValueError("Nothing approximately equal to %s found" % str(key))

def infudge(alist, key, fudge):
    try:
        fudgex(alist, key, fudge)
        return True
    except ValueError:
        return False

class memoized(object):
   """Decorator that caches a function's return value each time it is called.
   If called later with the same arguments, the cached value is returned, and
   not re-evaluated.
   """
   def __init__(self, func):
      self.func = func
      self.cache = {}
   def __call__(self, *args):
      try:
         return self.cache[args]
      except KeyError:
         value = self.func(*args)
         self.cache[args] = value
         return value
      except TypeError:
         # uncachable -- for instance, passing a list as an argument.
         # Better to not cache than to blow up entirely.
         return self.func(*args)
   def __repr__(self):
      """Return the function's docstring."""
      return self.func.__doc__
   def __get__(self, obj, objtype):
      """Support instance methods."""
      return functools.partial(self.__call__, obj)

def in12(height, length=1):
    return (height**2+144)**0.5*length

def splitfilter(specfunc, alist):
    '''like filter, but the discards go into the second list'''
    one, two = [], []
    for x in alist:
        if specfunc(x):
            one.append(x)
        else:
            two.append(x)
    return one, two

def exceptfilter(specfunc, alist):
    '''like splitfilter, but the input list is not split on booleans, but
    on whether or not the called function throws an exception. The second
    contains any exceptions'''
    good, bad = [], []
    for x in alist:
        try:
            specfunc(x)
            good.append(x)
        except Exception as ex:
            bad.append(ex)
    return (good, bad)

def exceptmap(specfunc, alist, maintain_spacing=False):
    '''like exceptfilter, but returns result of specfunc applied to the
    elements of list, instead of treating specfunc as a predicate. Set
    maintain_spacing to True to maintain index spacing in both lists,
    padding them with None.'''
    good, bad = [], []
    for x in alist:
        try:
            ix = specfunc(x)
            good.append(ix)
            if maintain_spacing:
                bad.append(None)
        except Exception as ex:
            bad.append(ex)
            if maintain_spacing:
                good.append(None)
    return (good, bad)

def dict_spread(adict):
    ks, vals = list(zip(*list(adict.items())))
    for valslice in zip(*vals):
        yield dict(list(zip(ks, valslice)))

def dict_merge(*dicts):
    res = {}
    for d in dicts:
        res.update(d)
    return res

def is_non_string_iterable(itm):
    if hasattr(itm, 'lower'):
        return False
    elif hasattr(itm, 'next') or hasattr(itm, 'index'):
        return True
    else:
        return False

class SmSingle():
    item=None
    def __init__(self, item):
        self.item=item

def supermap(func, *args, **kwargs):
    assert len(args) or len(kwargs)
    diters, dstatic = splitfilter(
        lambda x: is_non_string_iterable(x[1]),
        list(kwargs.items()))
    dstatic = [(k, v.item) if isinstance(v, SmSingle) else (k, v)\
               for k, v in dstatic]
    itered_args = [itertools.repeat(None)]
    for x in args:
        if is_non_string_iterable(x):
            itered_args.append(x)
        elif isinstance(x, SmSingle):
            itered_args.append(itertools.repeat(x.item))
        else:
            itered_args.append(itertools.repeat(x))
    diters = dict(diters)
    diters['_drop']=itertools.repeat(None)
    for xargs, xkws in zip(zip(*itered_args),
                                      dict_spread(diters)):
        xargs = xargs[1:]
        del(xkws['_drop'])
        yield func(*xargs, **dict_merge(xkws, (dict(dstatic))))

def tryit(func, *data):
    if not hasattr(func, '__call__'):
        raise ValueError("First parameter is not a function")
    try:
        func(*data)
        return True
    except:
        return False

def run_as_process(func):
    def launcher(*args, **keywds):
        Process(target=func, args=args, kwargs=keywds).start()
    return launcher

def showparams(func):
    """decorator for debugging functions. prints the parameters"""
    def wrapper(*args, **kw):
        for x in args:
            print(args)
        for itm in list(kw.items()):
            print('%s: %s' % itm)
        return func(*args, **kw)
    return wrapper

def coroutine(func):
    def start(*args,**kwargs):
        cr = func(*args,**kwargs)
        next(cr)
        return cr
    return start

def paste_to_self(text):
    """For pasting to IDLE command shell"""
    time.sleep(.01)
    Popen(('xdotool', 'getactivewindow'))
    Popen(('xdotool', 'type', text))

class ObjectStore(object):
    def __init__(self, **kw):
        self.__dict__.update(kw)

def getbasename(fname):
    fname = os.path.split(fname)[-1]
    return fname.split('.')[0]

def choose_from_list(alist, returnkey=False):
    print("Choose an option:")
    for i, item in enumerate(alist):
        print(i+1, item)
    res = input("1-%d:" % len(alist))
    try:
        return int(res)-1 if returnkey else alist[int(res)-1]
    except:
        return None

def threeway(item, result_list, key=None):
    if not key:
        key = lambda x: x
    item = key(item)
    i = 1
    if item < 0:
        i=0
    elif item > 0:
        i=2
    return result_list[i]

def paste_to_self(txt):
    import pygtk
    pygtk.require('2.0')
    import gtk
    clipboard = gtk.clipboard_get()
    clipboard.set_text(txt)
    clipboard.store()
    Popen(('xdotool', 'key', 'ctrl+v'))
    import time
    time.sleep(.5)
    Popen(('xdotool', 'key', 'Return'))

def fraw_input(prompt=''):
    sys.stdout.flush()
    return input(prompt=prompt)

def string_bool(itm):
    if not itm:
        return False
    if itm.lower() in ('no', 'false'):
        return False
    try:
        if int(itm)==0:
            return False
    except:
        pass
    return True

class RegexBattery():
    strict = False
    patterns = None
    keys = None
    error = None

    def __init__(self):
        self.patterns = []
        self.keys = set()

    def add(self, key, pat, flags=0, return_index=0):
        self.keys.add(key)
        if hasattr(pat, 'startswith'):
            pat = re.compile(pat, flags)
        self.patterns.append((key, pat, return_index))

    def search(self, text):
        res = {}
        lkeys = set()
        for k, p, i in self.patterns:
            if k in lkeys:
                continue
            r = re.search(p, text)
            if r:
                lkeys.add(k)
                res[k] = r.groups()[i]
        diff = self.keys.difference(list(res.keys()))
        self.error = diff
        if self.strict and diff:
            raise ValueError("Match Failed: {0} missing".format(diff))
        return res

def adds_to_val(value, thelist):
    if len(thelist)==1:
        if value in thelist:
            return [value]
        else:
            return False
    for g in range(2, len(thelist)+1):
        for nms in itertools.combinations(thelist, g):
            if sum(nms)==value:
                return list(nms)
    return False

def running_tally(aniter):
    val = 0
    for x in aniter:
        val += x
        yield val

#Returns indices out of thelist, unlike adds_to_val
def adds_to_val_linear(value, thelist):
    bal = [0] + list(running_tally(thelist))
    for i in range(len(bal)):
        curr = bal[i]
        xbal = curr-value
        try:
            ii = bal[:i].index(xbal)
            return (ii, i)
        except ValueError:
            continue
    return None


@contextmanager
def file_lock(lock_file):
    if os.path.exists(lock_file):
        print('Only one script can run at once. '\
              'Script is locked with %s' % lock_file)
        sys.exit(-1)
    else:
        open(lock_file, 'w').write("1")
        try:
            yield
        finally:
            os.remove(lock_file)

class vars_set():
    def __init__(self, module, **kw):
        self.data = kw
        self.stor = {}
        self.nonx = []
        if hasattr(module, '__dict__'):
            self.env = module.__dict__
        else:
            self.env = module
    def __enter__(self):
        m = self.env
        keys = list(self.data.keys())
        for k in keys:
            if k in m:
                self.stor[k] = m[k]
            else:
                self.nonx.append(k)
            m[k] = self.data[k]
    def __exit__(self, ttype, value, traceback):
        m = self.env
        keys = list(self.data.keys())
        for k in keys:
            if k in self.nonx:
                del(m[k])
            else:
                m[k] = self.stor[k]
        self.stor = {}
        self.nonx = []

def touch(fname, times=None):
    with open(fname, 'a'):
        os.utime(fname, times)

def string_to_file(a_string, fname):
    #FIXME: Should be a unicode file? doesn't want to write bytes without 'b'
    if type(a_string) == str:
        a_string = a_string.encode()
    with open(fname, 'wb') as fh:
        fh.write(a_string)

def fractindex(numbers, index):
    """Given a list of numeric values and an index, returns the value 
    stored at index. If the index is fractional, then it will calculate
    the appropriate 'midway' value between the whole portion of the index
    value and the next list item above it.
    Eg.: fractindex([2.8, 4], 0.5) => 3.4"""
    if int(index)==index:
        return numbers[int(index)]
    frac, whole = math.modf(index)
    if frac < 0:
        lower = abs(frac)
        upper = 1 - lower
        whole = len(numbers) + int(whole) - 1
    else:
        upper = frac
        lower = 1 - frac
        whole = int(whole)
    return numbers[whole] * lower + numbers[whole+1] * upper

    
