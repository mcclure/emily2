let total = inherit object
	value = 0
	method increment = function (x)
		this.value =
			+
				this.value
				x

total.increment 10
total.increment 20

# Expect: 30.0
println
	total.value

# Okay, so the method x = function thing is just a tad awkward, but
# the neat thing about it is methods are automatically get-properties

let totalShadow = inherit total
	method valuePlusOne =
		+
			this.value
			1

# Expect: 31.0
println
	totalShadow.valuePlusOne

# Also did I mention scopes are objects in this language

let x = 3
let method xPlusThree =
	+ x 3

# Expect: 3.0
println x

# Expect: 6.0
println xPlusThree

# Expect: 13.0
x = 10
println xPlusThree
