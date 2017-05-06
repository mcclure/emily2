# Test import * behaves as expected

let z = 10

let a = inherit Object
	b = 3
	method c = z

z = 20

let d = inherit Object
	from a import *

z = 30

from d import *

# Expect: 3.0 30.0 3.0 20.0 3.0 20.0

print
	a.b
	a.c
	d.b
	d.c
	b
	c
	ln
