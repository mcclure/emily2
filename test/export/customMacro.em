# Test export. IS ALSO IMPORTED BY MACRO.EM
# Arg: --exported
# Expect:
# Printed
# [Macros: 1]
# w: 2.0

export w = 2

export macro
	splitMacro(600, "+")

macro
	splitMacro(610, "*")

println "Printed"
