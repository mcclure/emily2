# Test iterators

# Expect: 1 2 3

let x = array
	1
	2
	3

let i = x.iter

while (i.more)
	let z = i.next
	print z
print ln