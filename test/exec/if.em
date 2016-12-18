# Test if statement

# Expect:
# two
# x false
# y true

let x = 0
let y = 2

if x
	println "zero"
if y
	println "two"

println
	if x ("x true") ("x false")

println
	if y ("y true") ("y false")
