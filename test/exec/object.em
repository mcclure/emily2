# Test object ops

# Test object creation
# Expect:
# 3.0
# 4.0
# 5.0

let obj1 = new object
	a = 1
	b = 2
	c = 3

let obj2 = new obj1
	c = 4
	d = 5

println
	obj1.c
	obj2.c
	obj2.d

# Test object creation with dynamic values
# Expect:
# 3.0
# 10.0
# 3.0
# 20.0

let counter = 0
while (< counter 2)
	counter =
		+ counter 1
	let obj3 = new obj2
		c =
			* counter 10
	println
		obj1.c
		obj3.c

# Test object key assignment
# Expect:
# 10.0
# 11.0

obj2.c = 10
let obj1.e = 11

println
	obj2.c
	obj2.e

# Test object key assignment with dynamic keys
# Expect:
# 20.0
# 21.0

let key = if 1 (.c) (.d)
obj2 key = 20.0
obj2 (if null (.c) (.d)) = 21.0

println
	obj2.c
	obj2.d

# Test nested assignment. While we're at it test creation of an empty object
# Expect:
# 30.0

obj2.c = new object
	x = new object
		y = null

obj2.c.x.y = new object()
let obj2.c.x.y.z = new object
	w = 0
obj2.c.x.y.z.w = 30.0

println
	obj2.c.x.y.z.w

# Test nested, dynamic key assignment. I acknowledge this section is gross
# Expect:
# 40.0

key = .y
obj2.c.x key .z.w = 40.0
key = .x
println
	obj2.c key .y.z.w