# Test instances (ie new and fields)

# Basic fields and scoping
# Expect:
# 1.0 2.0 3.0 4.0 5.0 10.0 3.0
# 1.0 7.0 9.0 4.0 8.0 10.0 9.0

let a = inherit object
	field a = 1
	field b = 2
	field c = 3
	x = 4
	y = 5
	z = 6
	method cProperty = this.c

let b = new a
	b = 7
	y = 8

b.c = 9
b.z = 10

print
	a.a, a.b, a.c
	a.x, a.y, a.z
	a.cProperty
	ln
	b.a, b.b, b.c
	b.x, b.y, b.z
	b.cProperty
	ln
