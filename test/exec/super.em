# Test object super-- sort of a scope test

# Expect:
# 10.0
# 12.0
# 15.0

let obj1 = inherit object
	v = 1
	method f = function()
		10

let obj2 = inherit obj1
	v = 2
	method f = function()
		+
			current.v
			super.f()

let obj3 = inherit obj2
	v = 3
	method f = function()
		let inner = inherit obj1
			f = function()
				super.f()     # Totally not what a reasonable user would expect
		+
			current.v
			inner.f()

println
	obj1.f()
	obj2.f()
	obj3.f()
