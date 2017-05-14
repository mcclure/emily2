# Just totally nail down that the backslash parser acts as expected
# Expect:
# 3.0
# 5.0
# 5.0

let \method = 3

println \method

let \let = null
\let = inherit Object
	\method = 5

println
	\let.method
	\let \.method
