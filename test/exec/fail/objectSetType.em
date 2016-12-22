# Test object ops [assign to string key]
# Expect failure

let x = inherit object
	a = 3

let x "b" = 4
