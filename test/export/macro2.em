# Test importing macros from a module which exports nothing else
# Expect:
# 9.0

let add = \+

macro project.customMacroOnly

println
	5 add 4
