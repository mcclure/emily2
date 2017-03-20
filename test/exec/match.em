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

# Literals
# Expect: Success Success

print
	with 3 match
		2 = 1
		"3" = 2
		3 = "Success"
		"ok" = 3
		4 = 4
	with "ok" match
		2 = 5
		"3" = 6
		3 = 7
		"ok" = "Success"
		4 = 8
	ln

# Complex unpack
# Expect:
# 4.0 5.0 6.0
# 4.0
# 4.0 5.0 6.0

let Vector3 = inherit object
	field x = null
	field y = null
	field z = null

let p = new Vector3( 4, 5, 6 )

with p match # Unpack vector
	Some x = println "?"
	Vector3(a, b, c) = print a b c ln

with p match # Unpack vector, do not use all fields
	Some x = println "??"
	Vector3(a) = print a ln

with p match # Unpack untyped
	Some x = println "???"
	array(a,b,c) = print a b c ln
	Vector3(a,b,c) = print "????" # First match not best match

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
# null 1.0 null 1.0

let q = inherit None

print
	is None object
	is object None
	is q None
	is None q
	ln
