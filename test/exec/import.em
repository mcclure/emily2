# Test import behaves as expected

let a = inherit Object
	b = 1
	c = 2
	d = inherit Object
		e = 3
		f = 4
		g = 5

import a.b
from a import c
from a import d.e

from a.d import
	f, g

# Expect: 1.0 2.0 3.0 4.0 5.0

print
	b
	c
	e
	f
	g
	ln
