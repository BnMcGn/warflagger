#!/usr/bin/env python
# -*- coding: iso-8859-1 -*-

#from gadgets import coroutine
from itertools import islice

#FIXME: Should be in gadgets
def coroutine(func):
    def start(*args,**kwargs):
        cr = func(*args,**kwargs)
        cr.next()
        return cr
    return start

#Simple version of window, below
def context(an_iter):
    an_iter = iter(an_iter)
    prev_item = None
    this_item = an_iter.next()
    for next_item in an_iter:
        yield (prev_item, this_item, next_item)
        prev_item = this_item
        this_item = next_item
    yield (prev_item, this_item, None)
    
def window(an_iter, length):
    """Creates a len length window in an_iter and steps through the iter returning the window as a tuple each time it is called"""
    an_iter=iter(an_iter)#in case it's a list
    stor = tuple(islice(an_iter, length))
    if len(stor) == length:
        yield stor
        for thing in an_iter:
            stor = stor[1:] + (thing,)
            yield stor

def ring_window(an_iter, length):
    """Like window, but the window overlaps onto the beginning of the source iter."""
    #FIXME: not very efficient. Window already makes a list out of it.
    an_iter = list(an_iter)
    return window(itertools.chain(an_iter, an_iter[:length]))

def group(an_iter, length=2):
    """Like window but with no overlap. Last set is yielded even if not full."""
    stor = []
    for x in an_iter:
        stor.append(x)
        if len(stor)>=length:
            yield tuple(stor)
            stor = []
    if len(stor):
        yield tuple(stor)

def dump(aniter):
    for itm in aniter:
        print itm
        yield itm

def progress(aniter, tick):
    count = 0
    bigdig = 0
    for itm in aniter:
        count+=1
        if count>tick:
            bigdig+=1
            print "%d x %d" % (bigdig, tick)
            count=0
        yield itm

def dictize(aniter, mode, initial=None):
    """iter must contain (key,value) pairs. mode is a string, one of: replace, keep,
    tally, sum, append, or a custom function that takes two arguments. 
    
    replace: default dict behavior. New value overwrites old if key exists. This
    is essentially a pass-thru.
    keep: Noop if kv already exists in dict.
    tally: Ignore value, count how many times each key occurs.
    sum: Each key contains a sum of the (presumably summable) values that arrive
    with that key.
    append: each key contains a list of the items that arrived with that key.
    add: each key contains a set of the items that arrived with that key.
    Custom func: The first argument is the existing key value. This function
    won't be called if the key doesn't exist. The second is the newly arrived value. 
    The return value will replace the existing value in the internal dict

    initial optional argument: function that gets called the first time a key
    occurs. It's parameter is the value. It's return is placed in the dict. Use
    to specify a default value."""
    data = {}
    modes = "replace keep tally sum append add".split(' ')
    funcs = [lambda e, n: n,
             lambda e, n: e,
             lambda e, n=None: e+1,
             lambda e, n: e+n,
             lambda e, n: e+[n],
             lambda e, n: e.union([n])]
    inits = [lambda v: v,
             lambda v: v,
             lambda v: 1,
             lambda v: v,
             lambda v: [v],
             lambda v: set([v])]
    if mode in modes:
        modei = modes.index(mode)
        func = funcs[modei]
        if not initial:
            initial = inits[modei]
    else:
        assert hasattr(mode, '__call__'), '2nd argument must be a function or\
        one of: %s' % ' '.join(modes)
        func = mode
        if not initial:
            initial = lambda x: x
        
    for (k, v) in aniter:
        if k in data:
            data[k] = func(data[k], v)
        else:
            data[k] = initial(v)
    return data

class StateMachinery(object):
    iter = None
    state = None #this must be set
    initial='state_sample'
    def __init__(self, aniter):
        self.iter = aniter
        
    def __iter__(self):
        return self

    def next(self):
        if not self.state:
            self.state = (getattr(self, self.initial),)
        return self.state[0](*self.state[1:])

    def state_sample(self, param=False):
        #set next state
        self.state=(self.state_sample, True)
        return self.iter.next()

#test
class MyState(StateMachinery):

    def state_one(self):
        v = self.iter.next()
        if v < 3:
            print 'here'
            self.state=(self.state_two, v)
        return 'one'

    def state_two(self, param):
        v = 0
        for x in range(param):
            v = self.iter.next()
        self.state = (self.state_three, v)
        return 'two'

    def state_three(self, param):
        if param==0:
            self.state = (self.state_one, )
            return self.state_one()
        else:
            self.state = (self.state_three, param - 1)
            return self.iter.next()
    initial = 'state_one'

def states2(iterable, test, state2, filter1=lambda x: x, include_match=True):
    iterable = unyield(iterable)
    if filter1 == None:
        for x in iterable:
            if test(x):
                if include_match:
                    iterable.unyield(x)
                subiter = state2(iterable)
                for y in subiter:
                    yield y
    else:
        for x in iterable:
            if test(x):
                if include_match:
                    iterable.unyield(x)
                subiter = state2(iterable)
                for y in subiter:
                    yield y
            else:
                yield filter1(x)

class readahead():
    gen = None
    stack = None
    def __init__(self, gen):
        self.gen = gen
        self.stack = []
    def __iter__(self):
        return self
    def next(self):
        if len(self.stack):
            return self.stack.pop()
        else:
            return self.gen.next()
    def readahead(self, count=1):
        """Feeds items from the source iterator into the stack. count
        tells how many times to call next. You may preview or tamper with 
        stuff using the .stack property. StopIteration will fall through
        .readahead() if the source iterator ends, so you should be 
        prepared to catch it if there is any doubt about where you are 
        in your iterator
        """
        for x in range(count):
            itm = self.gen.next()
            self.stack.insert(0, itm)
    def iter(self):
        data = reversed(self.stack)
        def gen():
            for x in data:
                yield x
            while True:    
                self.readahead()
                yield self.stack[0]
        return gen()


class unyield(readahead):
    def unyield(self, itm):
        self.stack.append(itm)

def combinations2(iter1, iter2, func=lambda a,b: True):
	return ((x, y) for y in iter2 for x in iter1 if func(x, y))

def chunk(iter, key=lambda x: x):
    last = None
    store = None
    for itm in iter:
        k = key(itm)
        if store==None:
            store=[]
            last = k
        elif k==last:
	        pass
        else:
            yield store
            store=[]
            last = k
        store.append(itm)
    if store:
        yield store

def change_indices(iterable, key=lambda x: x):
    """Yields indices of state changes in iterable. Comparisons can be done on
    different properties or aspects of the contents of iterable by defining a
    different key function."""
    started = False
    last = None
    for i, itm in enumerate(iterable):
        k = key(itm)
        if started:
            if k != last:
                yield i
                last = k
        else:
            last = k
            started = True

def flatten1(aniter):
    for x in aniter:
        for y in x:
            yield y

def denumerate(finite_iterable):
    """Like enumerate, returns (index, item) pairs, but in reverse order,
    starting with the end of the list/highest index and proceeding to the
    beginning/lowest"""
    data = list(reversed(finite_iterable))
    for i, x in zip(range(len(data)-1, -1, -1), data):
        yield i, x

def rotations(finite_iterable):
    itr = finite_iterable
    for x in range(len(itr)):
        yield itr[x:] + itr[:x]

def take(iter, quantity):
    return [x for i, x in zip(range(quantity), iter)]
