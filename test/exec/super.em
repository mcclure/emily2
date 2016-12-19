# Test object super-- sort of a scope test

# Expect:
# 10.0
# 12.0
# 15.0

let obj1 = new object
	v = 1
	method f = function()
		10

let obj2 = new obj1
	v = 2
	method f = function()
		+
			current.v
			super.f()

let obj3 = new obj2
	v = 3
	method f = function()
		let inner = new obj1
			f = function()
				super.f()     # Totally not what a reasonable user would expect
		+
			current.v
			inner.f()

println
	obj1.f()
	obj2.f()
	obj3.f()
