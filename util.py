import codecs
import sys

# Is this python 3?
py3 = sys.version_info >= (3, 0)

# Take a path to a UTF-8 or UTF-16 file. Return an object to be used with utflines()
def utfopen(path):
    with open(path, 'rb') as f:
        start = f.read(2) # Check first bytes for BOM
        utf16 = start.startswith(codecs.BOM_UTF16_BE) or start.startswith(codecs.BOM_UTF16_LE)
    return codecs.open(path, 'r', 'utf-16' if utf16 else 'utf-8-sig')

# Iterator that sweeps a file 1 character at a time
def filechars(f):
    while True:
        ch = f.read(1)
        if ch:
            yield ch
        else:
            return

# String to unicode
def utf8string(s):
    if py3:
        return path
    return codecs.decode(s, 'utf-8')
    # TODO: py3

# Switch statement taken from http://code.activestate.com/recipes/410692/
# Used under the terms of the PSF license

class switch(object):
    def __init__(self, value):
        self.value = value
        self.fall = False

    def __iter__(self):
        """Return the match method once, then stop"""
        yield self.match
    
    def match(self, *args):
        """Indicate whether or not to enter a case suite"""
        if self.fall or not args:
            return True
        elif self.value in args: # changed for v1.5, see activestate
            self.fall = True
            return True
        else:
            return False
