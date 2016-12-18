# Test scope assignment
# Expect:
# 1.0
# 2.0
# 3.0
# 4.0
# 5.0
# 4.0
# 4.0
# 2.0
# 2.0

let x = 1
println x

do
	x = 2
	println x

	do
		let x = 3
		println x

		do
			x = 4
			println x

			do
				let x = 5
				println x
			println x
		println x
	println x
println x

