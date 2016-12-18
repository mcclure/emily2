# Test array ops

# Expect:
# 3.0

let a = array
	1
	2
	3
	4

println
	a 2

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