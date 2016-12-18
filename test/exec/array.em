# Test array ops

# Test array creation
# Expect:
# 3.0

let a = array
	1
	2
	3
	4

println
	a 2

# Test arrays with dynamic contents
# Expect:
# 2.0
# 4.0
# 6.0

let counter = 0
while (< counter 3)
	counter =
		+ counter 1
	let inner = array
		counter
		* counter 2
		* counter 3
	println
		inner 1

# Test array assignment
# Expect:
# 10.0
# 11.0

a 2 = 10.0
a (+ 1 2) = 11.0

println
	a 2
	a 3

# Test empty array

let empty = array()
