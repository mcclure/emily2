# Test user-defined operators
# Expect:
# 24.0
# 24.0

let add = \+
let times = \*

# Inside this block, use ALGOL-like operator precedence
let x = do 
	macro
		splitMacro(600, "add")
		splitMacro(610, "times")

	3 times 4 add 6 times 2

# Back outside the block, everything's LISP again
let y =
	add (times 3 4) (times 2 6)

println
	x
	y
