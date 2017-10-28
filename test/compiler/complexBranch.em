# Minimal if with weird looking cond

# Expect:
# True
# 1
# 3

profile experimental

let x = 0
let y = 0

# TODO where's the then statement
if (
	do
		let z = x < 4
		if (z)
			x = x + 1
		x > 0
)
	println "True"
	y = 3
else
	println "False"
	y = 4

println x
println y