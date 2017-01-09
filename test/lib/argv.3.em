# Arg: test/lib/argv.2.em
# Arg: -xyz
# Arg: Blueberry muffins
# Expect: -xyz
# Expect: Blueberry muffins
# Expect: test/lib/argv.2.em

println
	argv 0
	argv 1
	argv 2
