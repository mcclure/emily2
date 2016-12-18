# Parser: Transformations ("macros") applied to parse tree to gradually make it executable

from core import *
from util import switch
import reader
import execution

class ParserException(EmilyException):
	pass

# Standard macros-- "ast"

class Macro(object):
	def __init__(s, progress):
		s.progress = progress

# Macro machinery

class MacroLevel(object):
	def __init__(s, progress, contents):
		s.progress = progress
		s.contents = contents

class MacroShortCircuit(Exception):
	def __init__(s, error):
		s.error = error

class Parser(object):
	def __init__(s):
		s.errors = []
		s.macros = []
		s.loadAll(standard_macros)

	def _innerLoad(s, macro):
		for existing in s.macros:
			if existing.progress == macro.progress:
				existing.contents.append(macro)
				return
		s.macros.append(MacroLevel(macro.progress, [macro]))

	def _sort(s):
		s.macros.sort(key=lambda x:x.progress)

	def load(s, macro):
		s._innerLoad(macro)
		s._sort()

	def loadAll(s, macros): # FIXME: O(n^2)
		for macro in macros:
			s._innerLoad(macro)
		s._sort()

	def checkComplete(s, node):
		if node.progress < ProgressBase.Executable:
			return s.errorAt(node, "Internal error: Macro processing completed but this node is unfinished")
		return None

	def errorAt(s, loc, msg):
		s.errors.append( Error(loc, msg) )
		return execution.InvalidExec(loc)

	def process(s, nodes):
		if not nodes:
			raise Exception("Internal error: macro process() cannot work on an empty list")
		try:
			for level in s.macros:
				left = []
				right = nodes
				while right:
					at = right.pop(0)
					if at.progress > level.progress:
						left.append(at)
						continue
					for macro in level.contents:
						if macro.match(left, at, right):
							result = macro.apply(s, left, at, right)
							# TODO: Catch exceptions
							if type(result) == Error:
								raise MacroShortCircuit(result)
							else:
								(left, at, right) = result
							break
					if at:
						left.append(at)
				nodes = left
		except MacroShortCircuit as e:
			return s.errorAt( e.error.loc, e.error.msg )

		if not nodes:
			# at is known non-None because otherwise we would have failed earlier
			return s.errorAt(at.loc, "Macro malfunctioned and produced an empty list")
		if nodes[0].__class__ == reader.ExpGroup:
			if not nodes[0].statements:
				result = s.makeUnit(nodes[0])
			elif len(nodes[0].statements) > 1:
				return s.errorAt(nodes[0].loc, "Line started with a multiline parenthesis group. Did you mean to use \"do\"?")
			else:
				result = s.process(nodes.pop(0).statements[0].nodes) # FIXME: Hold on, what's line/char in this case?
		else:
			result = nodes.pop(0)
			completenessError = s.checkComplete(result)
			if completenessError:
				return completenessError

		while nodes:
			arg = nodes.pop(0)
			if arg.__class__ == reader.ExpGroup:
				if len(arg.statements) == 1 and not arg.statements[0].nodes:
					return s.makeUnit(arg)
				else:
					idx = 0
					for statement in arg.statements:
						idx += 1
						if not statement.nodes:
							return s.errorAt(arg.loc, "Argument #%s to function is blank" % (idx))
						result = execution.ApplyExec(result.loc, result, s.process(statement.nodes))
			else:
				completenessError = s.checkComplete(arg)
				if completenessError:
					return completenessError
				result = execution.ApplyExec(result.loc, result, arg)

		s.checkComplete(result)
		return result

	def makeUnit(s, grp):
		return execution.NullLiteralExec(grp.loc)

	def makeSequence(s, loc, statements, shouldReturn = False):
		execs = [s.process(stm.nodes) for stm in statements]
		hasLets = False
		for exe in execs: # FIXME: This approach will do something weird if you = in a argument list or condition
			if exe.__class__ == execution.SetExec and exe.isLet:
				hasLets = True
				break
		return execution.SequenceExec(loc, shouldReturn, hasLets, execs)

# Standard macros-- "make values"

# TODO: do, if, while

def isSymbol(exp, match):
	return exp.__class__ == reader.SymbolExp and exp.content == match

# Abstract macro: matches on just one known symbol
class OneSymbolMacro(Macro):
	def match(s, left, node, right):
		return isSymbol(node, s.symbol())

# = sign
class SetMacro(OneSymbolMacro):
	def __init__(s):
		super(SetMacro, s).__init__(progress = ProgressBase.Macroed + 100)

	def symbol(s):
		return u"="

	def apply(s, m, left, node, right):
		isLet = False
		target = None
		for idx in range(len(left)):
			if isSymbol(left[idx], u"let"):
				isLet = True
			else:
				break
		if left:
			left = left[idx:]
		if len(left) == 0:
			return Error(node.loc, "Missing name")
		key = left[-1]
		if len(left) > 1:
			if key.__class__ != reader.SymbolExp or not key.isAtom:
				return Error(key.loc, "Assigned key must be a field name")
			target = m.process(left[:-1])
		else:
			if key.__class__ != reader.SymbolExp or key.isAtom:
				return Error(key.loc, "Assigned name must be alphanumeric")
		return ([], execution.SetExec(node.loc, isLet, key.content, m.process(right), target), [])

# Abstract macro: Expects SYMBOL (GROUP)
class SeqMacro(OneSymbolMacro):
	def apply(s, m, left, node, right):
		if not right:
			return Error(node.loc, u"Emptiness after \"%s\"" % (s.symbol()))
		seq = right.pop(0)
		if seq.__class__ != reader.ExpGroup:
			return Error(node.loc, u"Expected a (group) after \"%s\"" % (s.symbol()))
		return (left, s.construct(m, seq), right)

# do (statements)
class DoMacro(SeqMacro):
	def __init__(s):
		super(DoMacro, s).__init__(progress = ProgressBase.Macroed + 400)

	def symbol(s):
		return u"do"

	def construct(s, m, seq):
		return m.makeSequence(seq.loc, seq.statements, True)

# if (cond) (ifBlock) (elseBlock?) -- OR -- while (cond) (whileBlock)
class IfMacro(OneSymbolMacro):
	def __init__(s, loop):
		super(IfMacro, s).__init__(progress = ProgressBase.Macroed + 400)
		s.loop = loop

	def symbol(s):
		return u"while" if s.loop else u"if"

	def apply(s, m, left, node, right):
		if not right:
			return Error(node.loc, u"Emptiness after \"%s\"" % (s.symbol()))
		cond = right.pop(0)
		if not right:
			return Error(node.loc, u"Emptiness after \"%s (condition)\"" % (s.symbol()))
		seq = right.pop(0)
		if seq.__class__ != reader.ExpGroup:
			return Error(node.loc, u"Expected a (group) after \"%s (condition)\"" % (s.symbol()))
		elseq = None
		if not s.loop and right:
			elseq = right.pop(0)
			if elseq.__class__ != reader.ExpGroup:
				return Error(node.loc, u"Expected a (group) after \"%s (condition) (ifGroup)\"" % (s.symbol()))
		
		cond = m.process([cond])
		seq = m.makeSequence(seq.loc, seq.statements, not s.loop)
		if elseq:
			elseq = m.makeSequence(elseq.loc, elseq.statements, True)
		return (left, execution.IfExec(node.loc, s.loop, cond, seq, elseq), right)

# function (args) (body) -- OR -- func (args) (body)
class FunctionMacro(Macro):
	def __init__(s):
		super(FunctionMacro, s).__init__(progress = ProgressBase.Macroed + 400)

	def match(s, left, node, right):
		return isSymbol(node, u"function") or isSymbol(node, u"func")

	def apply(s, m, left, node, right):
		name = node.content
		if not right:
			return Error(node.loc, u"Emptiness after \"%s\"" % (name))
		argSymbols = right.pop(0)
		if argSymbols.__class__ != reader.ExpGroup:
			return Error(node.loc, u"Expected a (group) after \"%s\"" % (name))
		if not right:
			return Error(node.loc, u"Emptiness after \"%s (args)\"" % (name))
		seq = right.pop(0)
		if seq.__class__ != reader.ExpGroup:
			return Error(node.loc, u"Expected a (group) after \"%s (args)\"" % (name))
		args = []
		for stm in argSymbols.statements:
			if not stm.nodes:
				return Error(node.loc, u"Arg #%d on %s is blank" % (len(args)+1, name))
			if stm.nodes[0].__class__ != reader.SymbolExp:
				return Error(node.loc, u"Arg #%d on %s is not a symbol" % (len(args)+1, name))
			args.append(stm.nodes[0].content)
		return (left, execution.MakeFuncExec(node.loc, args, m.makeSequence(seq.loc, seq.statements, True)), right)

# array (contents)
class ArrayMacro(SeqMacro):
	def __init__(s):
		super(ArrayMacro, s).__init__(progress = ProgressBase.Macroed + 500)

	def symbol(s):
		return u"array"

	def construct(s, m, seq):
		return execution.MakeArrayExec(seq.loc, [m.process(stm.nodes) for stm in seq.statements])

# array (contents)
class ObjectMacro(OneSymbolMacro):
	def __init__(s):
		super(ObjectMacro, s).__init__(progress = ProgressBase.Macroed + 500)

	def symbol(s):
		return u"new"

	def apply(s, m, left, node, right):
		if not right:
			return Error(node.loc, u"Emptiness after \"new\"")
		base = right.pop(0)
		base = m.process([base])
		if not right:
			seq = reader.ExpGroup(base.loc)
		else:
			seq = right.pop(0)
			if seq.__class__ != reader.ExpGroup:
				return Error(node.loc, u"Expected a (group) after \"%s (args)\"" % (name))

		seq = m.makeSequence(seq.loc, seq.statements, False).execs
		for assign in seq:
			if assign.__class__ != execution.SetExec:
				return Error(assign.loc, "Found a stray value expression inside an object literal")
			if assign.source:
				return Error(assign.loc, "Assignment inside object literal was not of form key=value")
			if assign.isLet:
				return Error(assign.loc, "\"let\" is redundant in an object literal")
			assign.isLet = True
		return (left, execution.MakeObjectExec(node.loc, base, seq), right)

# Final pass: Turn everything not swallowed by a macro into a value
class ValueMacro(Macro):
	def __init__(s):
		super(ValueMacro, s).__init__(progress = ProgressBase.Macroed + 900)

	def match(s, left, node, right):
		c = node.__class__
		return c == reader.QuoteExp or c == reader.NumberExp or c == reader.SymbolExp

	def apply(s, m, left, node, right):
		for case in switch(node.__class__):
			if case(reader.QuoteExp):
				node = execution.StringLiteralExec(node.loc, node.content)
			elif case(reader.NumberExp):
				value = node.integer
				if node.dot:
					value += "."
				if node.decimal is not None:
					value += node.decimal
				node = execution.NumberLiteralExec(node.loc, float(value))
			elif case(reader.SymbolExp):
				if node.isAtom:
					node = execution.AtomLiteralExec(node.loc, node.content)
				else:
					node = execution.VarExec(node.loc, node.content)
			else:
				return Error(node.loc, "Internal error: AST node of indecipherable type %s found in a place that shouldn't be possible" % (node.__class__.__name__))
			
		return (left, node, right)

standard_macros = [
	DoMacro(), IfMacro(False), IfMacro(True), FunctionMacro(),
	ArrayMacro(), ObjectMacro(),
	SetMacro(),
	ValueMacro()
]

def exeFromAst(ast):
	parser = Parser()
	result = parser.makeSequence(ast.loc, ast.statements) # TODO test to make sure it's a group
	if parser.errors:
		output = []
		for e in parser.errors:
			output.append(u"Line %s char %s: %s" % (e.loc.line, e.loc.char, e.msg))
		raise ParserException(u"\n".join(output))
	return result