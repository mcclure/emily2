# String garbage
# TODO: Test some unicode

# Expect:
# null null null 1.0 1.0 null null null
# null null 1.0 null null null null null
# null null 1.0 1.0 1.0 null null null
# null null null null null null null 1.0
# null null null null null 1.0 null null
# null null null null null null 1.0 null
# null null null null null 1.0 1.0 null
# null 1.0 null null null null null null

let testChars = array
	"x"
	"3"
	"\n"
	" "
	"\t"
	"("
	"]"
	"\""

let testFunctions = array
	.isNonLineSpace
	.isLineSpace
	.isSpace
	.isQuote
	.isOpenParen
	.isCloseParen
	.isParen
	.isDigit

let iFn = testFunctions.iter
while (iFn.more)
	let fn = iFn.next

	let iCh = testChars.iter
	while (iCh.more)
		print ((char fn) (iCh.next))

	print ln
