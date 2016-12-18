# Test closure memory

# Expect:
# 1.0
# 101.0
# 3.0
# 103.0
# 6.0
# 106.0

let generator = function (x)
	let counter = x
	let generated = function (x)
		counter =
			+ counter x
		counter
	generated

let one = generator 0
let two = generator 100

println
	one 1
	two 1
	one 2
	two 2
	one 3
	two 3
