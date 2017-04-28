# let and export not allowed together
# Expect failure

let z = inherit object
	export q = 3
