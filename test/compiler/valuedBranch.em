# Minimal if with if returning a value

# Tags: compiler

# Expect:
# False
# 0
# 4
# 6

profile experimental

let x = 0
let y = 0

# TODO where's the then statement
let z = if (
	if (x < 10)
		x > 10
	else
		x < 20
)
	println "True"
	y = 3
	5
else
	let q = 6
	println "False"
	y = 4
	q

println x
println y
println z
