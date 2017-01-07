# Test user-defined operators
# Expect:
# 24.0
# 24.0

# Inside this block, use ALGOL-like operator precedence
let x = do 
	macro
		splitMacro(600, "+")
		splitMacro(610, "*")

	3 * 4 + 6 * 2

# Back outside the block, everything's LISP again
let y =
	+ (* 3 4) (* 2 6)

println
	x
	y
