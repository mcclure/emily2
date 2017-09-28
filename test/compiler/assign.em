# Minimal compileable -- assume blocks but no math or fn calls
# Expect: 7

# Tags: compiler

profile experimental

let x = 2
let y = 3
let z = x

let a = do
	x = y
	z = x
	x
