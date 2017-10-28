# Minimal if test

# Tags: compiler

# Expect:
# True
# True2
# 1

profile experimental

let x = 0
let y = x < 5

if (y)
	println "True"
	x = 1
else
	println "False"
	x = 2

if (y)
	println "True2"

let z = x > 1

if (z)
	println "True3"

println x