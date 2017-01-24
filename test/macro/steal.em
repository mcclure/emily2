# Test multi-line macros
# Arg: --ast2
# Expect: [Sequence [If [Var x] [Sequence(Returning) [NumberLiteral 3.0]] [Sequence(Returning) [NumberLiteral 4.0]]] [If [Var x] [Sequence(Returning) [NumberLiteral 5.0]] [Sequence(Returning) [NumberLiteral 6.0]]] [Apply [Apply [Var someFunction] [If [Var x] [Sequence(Returning) [NumberLiteral 7.0]] [Sequence(Returning) [NumberLiteral 8.0]]]] [NumberLiteral 9.0]]]

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
