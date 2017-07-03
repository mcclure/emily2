# Test argv

# Arg: test/lib/argv.em
# Arg: -xyz
# Arg: Blueberry muffins
# Expect: 3
# Expect: -xyz
# Expect: Blueberry muffins
# Expect: test/lib/argv.em

println
	argv.length
	argv 0
	argv 1
	argv 2
