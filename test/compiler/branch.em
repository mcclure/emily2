# Minimal if

# Tags: compiler

# Expect:
# True
# 1

profile experimental

let x = 0
let y = x < 5
# let z = y && x > (0 - 5)

if (y)
	println "True"
	x = 1
else
	println "False"
	x = 2

println x