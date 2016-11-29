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
			raise Exception("Internal error: Macro processing completed but this node is unfinished")

	def process(s, nodes):
		for level in s.macros:
			left = []
			right = nodes
			while right:
				at = right.pop(0)
				if at.progress > level.progress:
					continue
				for macro in level.contents:
					if macro.match(left, at, right):
						(left, at, right) = macro.apply(left, at, right)
						break
				if at:
					left.append(at)
			nodes = left

		if not nodes:
			raise Exception("UNIMPLEMENTED") # TODO: Return null
		if nodes[0].__class__ == parse.ExpGroup:
			if not nodes[0].statements:
				raise Exception("Not understood: Line started with a ().")
			elif len(nodes[0].statements) > 1:
				raise Exception("Line started with a multiline parenthesis group. Did you mean to use \"do\"?")
			else:
				result = s.process(nodes.pop(0).statements[0].nodes) # FIXME: Hold on, what's line/char in this case?
		else:
			result = nodes.pop(0)
			s.checkComplete(result)

		while nodes:
			arg = nodes.pop(0)
			if arg.__class__ == parse.ExpGroup:
				for statement in arg.statements:
					result = execution.ApplyExec(result, s.process(statement.nodes))
			else:
				s.checkComplete(arg)
				result = execution.ApplyExec(result, arg)

		s.checkComplete(result)
		return result

	def makeSequence(s, statements):
		return execution.SequenceExec(False, [s.process(stm.nodes) for stm in statements])

# Standard macros-- "make values"

# TODO: do, if, while


class ValueMacro(Macro):
	def __init__(s):
		super(ValueMacro, s).__init__(progress = ProgressBase.Macroed + 900)

	def match(s, left, node, right):
		return node.__class__ != parse.ExpGroup

	def apply(s, left, node, right):
		for case in switch(node.__class__):
			if case(parse.QuoteExp):
				node = execution.StringLiteralExec(node.content)
			elif case(parse.NumberExp):
				value = node.integer
				if node.dot:
					value += "."
				if node.decimal is not None:
					value += node.decimal
				node = execution.NumberLiteralExec(float(value))
			elif case(parse.SymbolExp):
				if node.isAtom:
					node = execution.AtomLiteralExec(node.content)
				else:
					node = execution.VarExec(node.content)
			else:
				raise Exception("Shouldn't have got here")
			
		return (left, node, right)

standard_macros = [
	# DoMacro(), IfMacro(), WhileMacro(), SetMacro(),
	ValueMacro()
]

def exeFromAst(ast):
	macros = MacroMachine()
	result = macros.makeSequence(ast.statements) # TODO test to make sure it's a group
	if macros.errors:
		output = []
		for e in parser.errors:
			output.append("Line %s char %s: %s" % (e.line, e.char, e.msg))
		raise MacroException("\n".join(output))
	return result