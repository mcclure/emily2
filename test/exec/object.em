# Test object ops

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

# Expect:
# 10.0
# 11.0

obj2.c = 10
let obj1.e = 11

println
	obj2.c
	obj2.e
