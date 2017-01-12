# Test iterators

# Expect: 1.0 2.0 3.0

let x = array
	1
	2
	3

let i = x.iter

while (i.more)
	let z = i.next
	print z
print ln