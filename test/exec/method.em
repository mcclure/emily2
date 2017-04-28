# Test methods on objects

# Test 'this' mutation
# Expect: 30.0 30.0

let total = inherit Object
	value = 0
	method increment = function (x)
		this.value =
			+
				this.value
				x
	method valueProperty = this.value

total.increment 10
total.increment 20

print
	total.value, total.valueProperty, ln

# Test inheritance with 'this' (child calls parent)
# Expect: 31.0

let totalShadow = inherit total
	method valuePlusOne =
		+
			this.value
			1

println
	totalShadow.valuePlusOne

# Test inheritance with 'this' (parent calls child)

let totalMirror = inherit totalShadow
	value = 100

# Expect:
# 30.0 30.0
# 100.0 100.0 101.0

print
	total.value,       total.valueProperty,                                 ln
	totalMirror.value, totalMirror.valueProperty, totalMirror.valuePlusOne, ln

# Expect:
# 30.0 30.0
# 110.0 110.0 111.0

totalMirror.increment 10

print
	total.value,       total.valueProperty,                                 ln
	totalMirror.value, totalMirror.valueProperty, totalMirror.valuePlusOne, ln

# Test methods on scope object

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
