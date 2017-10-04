# Test string ops

let x = "one two three"

let i = x.iter

# Expect:
# n o n null x 13 3q 4 atom
# e e r

print
	x 1
	i.next
	i.next
	"".iter.more
	"xyz".iter.next
	x.length
	+ (3..toString) "q"
	+ ("3".toNumber) 1
	.atom.toString
	ln

let i2 = x.reverseIter

print
	i2.next, i2.next, i2.next
	ln
