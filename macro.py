# Transformations applied to parse tree to gradually make it executable

from core import *
from util import switch
import parse
import execution

class MacroException(EmilyException):
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

class MacroMachine(object):
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
		if nodes[0].__class__ == parse.ExpGroup:
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
			if arg.__class__ == parse.ExpGroup:
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
	return exp.__class__ == parse.SymbolExp and exp.content == match

class SetMacro(Macro):
	def __init__(s):
		super(SetMacro, s).__init__(progress = ProgressBase.Macroed + 100)

	def match(s, left, node, right):
		return isSymbol(node, '=')

	def apply(s, m, left, node, right):
		isLet = False
		for idx in range(len(left)):
			if isSymbol(left[idx], 'let'):
				isLet = True
			else:
				break
		if left:
			left = left[idx:]
		if len(left) == 0:
			return Error(node.loc, "Missing name")
		if len(left) > 1:
			return Error(left[1].loc, "\"=\" can't do indices yet")
		if left[0].__class__ != parse.SymbolExp:
			return Error(left[0].loc, "Variable name must be alphanumeric")
		return ([], execution.SetExec(node.loc, isLet, left[0].content, m.process(right)), [])


class DoMacro(Macro):
	def __init__(s):
		super(DoMacro, s).__init__(progress = ProgressBase.Macroed + 400)

	def match(s, left, node, right):
		return isSymbol(node, 'do')

	def apply(s, m, left, node, right):
		if not right:
			return Error(node.loc, "Emptiness after \"do\"")
		seq = right.pop(0)
		if seq.__class__ != parse.ExpGroup:
			return Error(node.loc, "Expected a (group) after \"do\"")
		return (left, m.makeSequence(seq.loc, seq.statements, True), right)

class IfMacro(Macro):
	def __init__(s, loop):
		super(IfMacro, s).__init__(progress = ProgressBase.Macroed + 400)
		s.loop = loop

	def symbol(s):
		return "while" if s.loop else "if"

	def match(s, left, node, right):
		return isSymbol(node, s.symbol())

	def apply(s, m, left, node, right):
		if not right:
			return Error(node.loc, "Emptiness after \"%s\"" % (s.symbol()))
		cond = right.pop(0)
		if not right:
			return Error(node.loc, "Emptiness after \"%s (condition)\"" % (s.symbol()))
		seq = right.pop(0)
		if seq.__class__ != parse.ExpGroup:
			return Error(node.loc, "Expected a (group) after \"%s (condition)\"" % (s.symbol()))
		return (left, execution.IfExec(node.loc, s.loop, m.process([cond]), m.makeSequence(seq.loc, seq.statements, not s.loop), None), right)

class FunctionMacro(Macro):
	def __init__(s):
		super(FunctionMacro, s).__init__(progress = ProgressBase.Macroed + 400)

	def match(s, left, node, right):
		return isSymbol(node, "function") or isSymbol(node, "func")

	def apply(s, m, left, node, right):
		name = node.content
		if not right:
			return Error(node.loc, "Emptiness after \"%s\"" % (name))
		args = right.pop(0)
		if args.__class__ != parse.ExpGroup:
			return Error(node.loc, "Expected a (group) after \"%s\"" % (name))
		if not right:
			return Error(node.loc, "Emptiness after \"%s (args)\"" % (name))
		seq = right.pop(0)
		if seq.__class__ != parse.ExpGroup:
			return Error(node.loc, "Expected a (group) after \"%s (args)\"" % (name))
		args = []
		for stm in seq.statements:
			if not stm:
				return Error(node.loc, "Arg #%d on %s is blank" % (len(args)+1, name))
			if stm.nodes[0].__class__ != parse.SymbolExp:
				return Error(node.loc, "Arg #%d on %s is not a symbol" % (len(args)+1, name))
			args.append(stm.nodes[0].content)
		return (left, execution.MakeFuncExec(node.loc, args, m.makeSequence(seq.loc, seq.statements, True)), right)

class ValueMacro(Macro):
	def __init__(s):
		super(ValueMacro, s).__init__(progress = ProgressBase.Macroed + 900)

	def match(s, left, node, right):
		c = node.__class__
		return c == parse.QuoteExp or c == parse.NumberExp or c == parse.SymbolExp

	def apply(s, m, left, node, right):
		for case in switch(node.__class__):
			if case(parse.QuoteExp):
				node = execution.StringLiteralExec(node.loc, node.content)
			elif case(parse.NumberExp):
				value = node.integer
				if node.dot:
					value += "."
				if node.decimal is not None:
					value += node.decimal
				node = execution.NumberLiteralExec(node.loc, float(value))
			elif case(parse.SymbolExp):
				if node.isAtom:
					node = execution.AtomLiteralExec(node.loc, node.content)
				else:
					node = execution.VarExec(node.loc, node.content)
			else:
				return Error(node.loc, "Internal error: AST node of indecipherable type %s found in a place that shouldn't be possible" % (node.__class__.__name__))
			
		return (left, node, right)

standard_macros = [
	DoMacro(), IfMacro(False), IfMacro(True), FunctionMacro(),
	SetMacro(),
	ValueMacro()
]

def exeFromAst(ast):
	macros = MacroMachine()
	result = macros.makeSequence(ast.loc, ast.statements) # TODO test to make sure it's a group
	if macros.errors:
		output = []
		for e in macros.errors:
			output.append("Line %s char %s: %s" % (e.loc.line, e.loc.char, e.msg))
		raise MacroException("\n".join(output))
	return result