# Arg: --ast
# Expect: (One two (three four (five), six), One two (three four (five six)), One (two three) (four (five (six)) seven (eight nine), ten))

One two
	three four
		five
	six

One two
	three four
		five
		six

One (two three)
	four (
five
	six
	) seven
		eight nine
	ten