# Test match with too many fields in unpack
# Expect failure

let None = inherit object
let Some = inherit object
	field value = null

let p = new Some(3)

with p match # Unpack vector
	None = println "?"
	Some(a, b, c) = print a b c ln
