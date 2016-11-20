from util import switch
import unicodedata

# AST

class Node:
	pass

class ExpGroup(Node):
	def __init__(s, line=0, char=0, openedWithParenthesis=False):
		s.line = line
		s.char = char
		s.openedWithParenthesis = openedWithParenthesis
		s.statements = []
		s.appendStatement()
		s.indent = None

	def finalStatement(s):
		return s.statements[-1]

	def appendStatement(s):
		s.statements.append( Statement() )

class Statement: # Not a node, only a helper for ExpGroup
	def __init__(s):
		s.nodes = []
		s.dead = False

class Error:
	def __init__(s, line, char, msg):
		s.line = line
		s.char = char
		s.msg = msg

# Parser

# Lexer FSM:
# All states except Quote,Comment,Dot,Cr,Indent:
#     ( -> [Push frame] Scanning; ) -> [Pop frame] [un-Dead] Scanning;
#     , -> [un-Dead] Scanning; \r -> Cr; LineSpace -> [newline] Indent; # -> Comment
# Indent:
#     NonLineSpace -> [Check indent level, push or pop frame];
#     Comment -> [Indent]; , -> [Error];
#     Other -> [Verify/set indent level unless line blank] Reinterpret as if scanning
# Scanning:
#     NonLineSpace -> Scanning; . -> Dot;
#	  Digit -> Number; " -> Quote; # -> Comment, Other -> Symbol
# Cr: \n -> [newline] Scanning; Other -> [newline] Reinterpret as if Scanning
# Dot: Digit -> Number; ()#".[newline] -> [Error,Dead]; Other -> Symbol
# Symbol:
#     Number, LineSpace, ()#." -> Reinterpret as if Scanning;
#     NonLineSpace -> Scanning; Other -> Symbol
# Number:
#     Digit -> Number; Dot -> Number; Dot[multiple] -> [Error] Dead;
#     NonLineSpace -> Scanning; Other -> Reinterpret as if Scanning
# Quote:
#     " -> Scanning; '\' -> [Backslashed]; [When backslashed] nr\" -> [Reinterpret] Quote;
#     \r -> QuoteCr; [LineSpace] -> [Eat if backslashed] [newline] Quote;
#     '\other' -> [Error] Quote; Other -> Quote
# QuoteCr:
#     \n -> [Eat if backslashed] [newline] Quote; Other -> [newline] Reinterpret as if Quote
# Comment: \r \n -> [newline] Scanning; Other -> Comment

# Indent: Line-starting whitespace
# Cr: Hit CR, looking for LF
# Dot: Ambiguous; could become either Symbol or Number
# QuoteCr: Hit CR, looking for LF, inside quote
class ParserState:
	Indent, Scanning, Cr, Dot, Symbol, Number, Quote, QuoteCr, Comment = range(9)

def isNonLineSpace(ch):
	if ch == '\r' or ch == '\n':
		return True
	return unicodedata.category(ch) == 'Zs'
def isLineSpace(ch):
	if ch == '\t':
		return True
	cat = unicodedata.category(ch)
	return cat == 'Zl' or cat == 'Zp'
def isQuote(ch):
	if ch == u'"':
		return True
	cat = unicodedata.category(ch)
	return cat == 'Pi' or cat == 'Pf'
def isOpenParen(ch):
	return unicodedata.category(ch) == 'Ps'
def isCloseParen(ch):
	return unicodedata.category(ch) == 'Pe'
def isDigit(ch):
	return ch >= ord(u'0') and ch <= ord(u'9')

class ParserMachine:
	def __init__(s):
		s.line = 1
		s.char = 0
		s.groupStack = []
		s.errors = []
		s.appendGroup()

	def finalGroup(s):
		return s.groupStack[-1]

	def appendGroup(s, inStatement = False, openedWithParenthesis = False):
		group = ExpGroup(s.line, s.char, openedWithParenthesis)
		if inStatement:
			this.finalGroup().finalStatement().nodes.append( group )
		s.groupStack.append( group )

	def reset(s, state, backslashed = False):
		s.parserState = state
		s.backslashed = backslashed
		s.currentIndent = ''

	def newline(s):
		s.line += 1
		s.char = 0

	def handleLineSpace(s, ch):
		if ch == '\r':
			s.reset(ParserState.Cr)
		else:
			s.newline()
			s.reset(ParserState.Indent)

	def error(s, msg, survivable = False): # Survivable as in: Can we continue parsing syntax
		s.errors.append(Error(s.line, s.char, msg))
		if not survivable:
			s.finalGroup().finalStatement().dead = True
			s.reset(ParserState.Scanning)

	def ast(s, iter):
		s.reset(ParserState.Indent)

		for ch in iter:
			for case in switch(s.parserState):
				s.char += 1

				if case(ParserState.Cr):
					s.newline()
					s.newexp()
					s.reset(ParserState.Indent)
					if i == u'\n':
						break # If complete CRLF, DONE

				if case(ParserState.Indent):
					if isNonLineSpace(ch):
						s.currentIndent += ch
						break # If indent continued, DONE
					if isLineSpace(ch):
						s.handleLineSpace(ch)
						break # If indent ended line (with newline), DONE
					if ch == u'#':
						s.reset(ParserState.Comment)
						break; # If indent ended line (with comment), DONE
					if ch == u',':
						s.error("Comma at start of line not understood") # FIXME: This should clear entire line
						break # If line starts with comment (illegal), DONE

					# Line has begun! Adjust group and move into scanning state:
					finalGroup = s.finalGroup()

					# First line of group
					if finalGroup.indent is None:
						# TODO: Detect indent-after-content error
						finalGroup.indent = s.currentIndent
						s.finalGroup().appendStatement()

					# Indent or dedent event
					elif finalGroup.indent != s.currentIndent:
						# Indent event
						if s.currentIndent.startswith(final):
							s.appendGroup(True)

						# Dedent event (or error)
						else:
							unrollIdx = len(s.groupStack) - 1
							parenthesisIssue = s.finalGroup().openedWithParenthesis
							if not parenthesisIssue:
								while True:
									unrollIdx -= 1
									if unrollIdx < 0:
										break
									group = s.groupStack[unrollIdx]
									if group.openedWithParenthesis:
										parenthesisIssue = True
										break
									if group.indent == s.currentIndent:
										break

							# Error
							if parenthesisIssue:
								group = s.groupStack(unrollIdx) # Redundant but ehhh
								s.error("Indentation on this line doesn't match any since parenthesis on line %d char %d", group.line, group.char)
							elif unrollIdx < 0:
								s.error("Indentation on this line doesn't match any previous one")

							# Dedent
							else:
								while len(s.groupStack) > unrollIdx+1:
									s.groupStack.pop()
								s.finalGroup().appendStatement()

					# If we didn't break above, we're done with the indent.
					s.reset(ParserState.Scanning)

				if case(ParserState.Dot):
					if isNonLineSpace(ch):
						break
					if isDigit(ch):
						# TODO SWITCH TO NUMBER
						break
					if ch == '.' or ch == '(' or ch == ')' or ch == '"':
						s.error("'.' was followed by special character '%s'" % ch)
					elif ch == '#' or isLineSpace(ch):
						s.error("Line ended with a '.'")
					else:
						# TODO SWITCH TO SYMBOL
						break

				if case(ParserState.Quote):
					# TODO PUSH TO QUOTE
					break

				# These checks are shared by: Scanning Symbol Number Comment
				if isLineSpace(ch):
					s.handleLineSpace(ch)
					break # Consumed newline. DONE

				if case(ParserState.Comment): # FIXME: This + linespace could go earlier?
					break # Inside comment, don't care. DONE

				# These checks are shared by: Scanning Symbol Number
				if isOpenParen(ch):
					s.appendGroup(True, True)
					s.reset(ParserState.Scanning)
					break # Have consumed (. DONE
				if isCloseParen(ch):
					# Unroll stack looking for last parenthesis-based group
					unrollIdx = len(s.groupStack)
					while True:
						unrollIdx -= 1
						if unrollIdx < 0:
							break
						group = s.groupStack[unrollIdx]
						if group.openedWithParenthesis:
							break

					# None found
					if unrollIdx < 0:
						s.error("Stray right parenthesis matches nothing", True)

					s.reset(ParserState.Scanning)
					break # Have consumed (. DONE
				if isQuote(ch):
					s.reset(ParserState.Quote)
					break # Have consumed ". DONE
				if ch == ',':
					s.finalGroup().appendStatement()
					s.reset(ParserState.Scanning)
					break # Have consumed ,. DONE
				if ch == '#':
					s.reset(ParserState.Comment)
					break # Have consumed #. DONE

				if ch == '.' and not case(ParserState.Number): # Shared by: Scanning Symbol
					s.reset(ParserState.Dot)
					break # Have consumed .. DONE
				elif isDigit(ch):
					if case(ParserState.Scanning):
						s.reset(ParserState.Number)
				else: # Symbol character
					if case(ParserState.Scanning) or case(ParserState.Number):
						s.reset(ParserState.Symbol)

				if case(ParserState.Number):
					# TODO ADD TO NUMBER
					break

				if case(ParserState.Symbol):
					# TODO ADD TO SYMBOL
					break

		# Done; unroll all groups
		while len(s.groupStack) > 1:
			group = s.finalGroup()
			if group.openedWithParenthesis:
				s.error("Parenthesis on line %d char %d never closed", group.line, group.char)
			s.groupStack.pop()

def ast(iter):
	parser = ParserMachine()
	parser.ast(iter)
	if parser.errors:
		output = []
		for e in parser.errors:
			output.append("Line %s char %s: %s" % (e.line, e.char, e.msg))
		raise Exception("\n".join(output))
	return parser.finalGroup()
