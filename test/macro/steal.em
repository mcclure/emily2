# Test multi-line macros
# Arg: --ast2
# Expect: [Sequence(Scoped) [If [Var x] [Sequence(Returning) [NumberLiteral 3.0]] [Sequence(Returning) [NumberLiteral 4.0]]] [If [Var x] [Sequence(Returning) [NumberLiteral 5.0]] [Sequence(Returning) [NumberLiteral 6.0]]] [Apply [Apply [Var someFunction] [If [Var x] [Sequence(Returning) [NumberLiteral 7.0]] [Sequence(Returning) [NumberLiteral 8.0]]]] [NumberLiteral 9.0]] [Array [If [Var x] [Sequence(Returning) [NumberLiteral 10.0]] [Sequence(Returning) [NumberLiteral 11.0]]]] [Apply [Var someFunction] [If [Var x] [Sequence(Returning) [NumberLiteral 12.0]] [Sequence(Returning) [NumberLiteral 13.0]]]] [Let Scope [AtomLiteral z] [If [Var x] [Sequence(Returning) [NumberLiteral 14.0]] [Sequence(Returning) [NumberLiteral 15.0]]]]]

if (x)
	3
else
	4

if (x)
	5


else
	6

someFunction
	if (x)
		7
	else
		8
	9

array
	if (x)
		10
	else
		11

# I'm not sure I like this, but currently it works
someFunction if (x)
	12
else
	13

let z = if (x)
	14
else
	15
