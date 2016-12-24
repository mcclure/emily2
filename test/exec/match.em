# Test match statement

# Basic
# Expect: Value is: null

let None = inherit object
let Some = inherit object
	field value = null

let v = new Some(null)

with v match
	None = println "No value"
	Some x =
		print
			"Value is:"
			x
			ln

# Closure
# Expect:
# VALUE IS: 3.0
# VALUE IS: 2.0
# NO VALUE

let maybeClosure = do
	let x = "VALUE IS:"
	let y = "NO VALUE"
	match
		None = println y
		Some z =
			print
				x
				z
				ln

maybeClosure
	new Some (3)
maybeClosure
	new Some (2)
maybeClosure
	None

# Test is
# Expect:
# False True False True

let q = inherit None

print
	is None object
	is object None
	is q None
	is None q
	ln
