# Test object ops [assign to nonexistent key]
# Expect failure

let x = new object
	a = 3

x.b = 4
