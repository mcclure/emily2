# Test using macro/profile on a package also imports symbols

profile experimental

# Expect: null null -3

print
	1.0 && null
	\&& (1.0, null)
	\~ 3
	ln
