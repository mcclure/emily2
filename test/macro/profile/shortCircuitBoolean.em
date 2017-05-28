# Test short circuiting boolean operators

# Expect:
# GET FALSE
# null
# GET FALSE
# null
# GET TRUE
# GET FALSE
# null
# GET TRUE
# GET TRUE
# 1.0
# GET FALSE
# GET FALSE
# null
# GET FALSE
# GET TRUE
# 1.0
# GET TRUE
# 1.0
# GET TRUE
# 1.0

macro shortCircuitBoolean

let getFalse = function ()
	println "GET FALSE"
	null

let getTrue = function ()
	println "GET TRUE"
	1

# Awkward subtleness: Doubles as a test of "when" println prints
println
	getFalse() && getFalse()
	getFalse() && getTrue()
	getTrue()  && getFalse()
	getTrue()  && getTrue()
	getFalse() || getFalse()
	getFalse() || getTrue()
	getTrue()  || getFalse()
	getTrue()  || getTrue()