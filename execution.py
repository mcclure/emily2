# Execution tree classes

import sys
from core import *
from util import unicodeJoin, quotedString

# Values

class PythonFunctionValue(object):
	def __init__(s, argCount, fn, startingArgs = []): # Takes ownership of startingArgs
		s.argCount = argCount
		s.fn = fn
		s.args = startingArgs

	def apply(s, value):
		if s.argCount == 0:
			return s.fn()
		newArgs = s.args + [value]
		if len(s.args)+1 >= s.argCount:
			return s.fn(*newArgs)
		return PythonFunctionValue(s.argCount, s.fn, newArgs)

class ObjectValue(object):
	def __init__(s):
		s.atoms = {}

	def apply(s, value):
		if type(value) != AtomLiteralExec:
			raise Exception("Objects have atom keys only")
		return s.atoms[value.value] # FIXME throw

# Executable nodes

class Executable(Node):
	def __init__(s, loc, immutable = False): # , line, char
		super(Executable, s).__init__(loc)
		s.immutable = immutable
		s.progress = ProgressBase.Executable

class InvalidExec(Executable):
	def __init__(s, loc):
		super(InvalidExec, s).__init__(loc)
		
	def __unicode__(s):
		return u"[Invalid node]" % (unicodeJoin(u" ", s.execs))

	def eval(s, scope):
		raise Exception("Cannot evaluate invalid program")

class SequenceExec(Executable):
	def __init__(s, loc, shouldReturn, execs):
		super(SequenceExec, s).__init__(loc)
		s.shouldReturn = shouldReturn
		s.execs = execs

	def __unicode__(s):
		return u"[Sequence %s]" % (unicodeJoin(u" ", s.execs))

	def eval(s, scope):
		for exe in s.execs:
			result = exe.eval(scope)
		return result if s.shouldReturn else None

class LiteralExec(Executable):
	def __init__(s, loc, source):
		super(LiteralExec, s).__init__(loc) # source.line, source.char

class StringLiteralExec(Executable):
	def __init__(s, loc, value):
		super(StringLiteralExec, s).__init__(loc)
		s.value = value

	def __unicode__(s):
		return u"[StringLiteral %s]" % (quotedString(s.value))

	def eval(s, scope):
		return s.value

class NumberLiteralExec(Executable):
	def __init__(s, loc, value):
		super(NumberLiteralExec, s).__init__(loc)
		s.value = value

	def __unicode__(s):
		return u"[NumberLiteral %s]" % (s.value)

	def eval(s, scope):
		return s.value

class AtomLiteralExec(Executable):
	def __init__(s, loc, value):
		super(AtomLiteralExec, s).__init__(loc)
		s.value = value

	def __unicode__(s):
		return u"[AtomLiteral %s]" % (s.value)

	def eval(s, scope):
		return s

class NullLiteralExec(Executable):
	def __init__(s, loc):
		super(NullLiteralExec, s).__init__(loc)

	def __unicode__(s):
		return u"[NullLiteral]"

	def eval(s, scope):
		return None

class IfExec(Executable):
	def __init__(s, loc, loop, condClause, ifClause, elseClause):
		super(IfExec, s).__init__(loc)
		s.loop = loop
		s.condClause = condClause
		s.ifClause = ifClause
		s.elseClause = elseClause

	def __unicode__(s):
		return u"[%s %s]" % ("While" if s.loop else "If",
			unicodeJoin(u" ", [s.condClause, s.ifClause] + ([s.elseClause] if s.elseClause else [])))

	def eval(s, scope):
		if not s.loop:
			if s.condClause.eval(scope):
				return s.ifClause.eval(scope)
			if s.elseClause:
				return s.elseClause.eval(scope)
		else:
			while s.condClause.eval(scope):
				s.ifClause.eval(scope)
		return None

class VarExec(Executable):
	def __init__(s, loc, symbol, source=None):
		super(VarExec, s).__init__(loc)
		s.source = source
		s.symbol = symbol

	def __unicode__(s):
		return u"[Var %s]" % (s.symbol)

	def eval(s, scope):
		return scope.atoms[s.symbol]

class SetExec(Executable):
	def __init__(s, loc, isLet, symbol, valueClause, source=None): # TODO: indexClauses
		super(SetExec, s).__init__(loc)
		s.isLet = isLet
		s.source = source
		s.symbol = symbol
		s.valueClause = valueClause

	def __unicode__(s):
		return u"[%s %s %s]" % ("Let" if s.isLet else "Set", s.symbol, unicode(s.valueClause))

	def eval(s, scope):
		scope.atoms[s.symbol] = s.valueClause.eval(scope)
		return None

class ApplyExec(Executable):
	def __init__(s, loc, f, arg): # f for function
		super(ApplyExec, s).__init__(loc) # FIXME: f.location
		s.f = f
		s.arg = arg

	def __unicode__(s):
		return u"[Apply %s]" % (unicodeJoin(u" ", [s.f, s.arg]))

	def eval(s, scope):
		return s.f.eval(scope).apply(s.arg.eval(scope))

# Base scope

defaultScope = ObjectValue()
defaultScope.atoms['+'] = PythonFunctionValue(2, lambda x,y: x + y)
defaultScope.atoms['-'] = PythonFunctionValue(2, lambda x,y: x - y)
defaultScope.atoms['*'] = PythonFunctionValue(2, lambda x,y: x * y)
defaultScope.atoms['/'] = PythonFunctionValue(2, lambda x,y: x / y)
defaultScope.atoms['%'] = PythonFunctionValue(2, lambda x,y: x % y)

def toBool(x):
	return 1 if x else None
defaultScope.atoms['bool'] = PythonFunctionValue(1, toBool)
defaultScope.atoms['number'] = PythonFunctionValue(1, lambda x: float(x))
defaultScope.atoms['string'] = PythonFunctionValue(1, lambda x: str(x))
defaultScope.atoms['not'] = PythonFunctionValue(1, lambda x: toBool(not x))
defaultScope.atoms['and'] = PythonFunctionValue(2, lambda x,y: toBool(x and y))
defaultScope.atoms['or'] = PythonFunctionValue(2, lambda x,y: toBool(x or y))
defaultScope.atoms['xor'] = PythonFunctionValue(2, lambda x,y: toBool(bool(x) != bool(y)))
defaultScope.atoms['eq'] = PythonFunctionValue(2, lambda x,y: toBool(x == y))
defaultScope.atoms['ne'] = PythonFunctionValue(2, lambda x,y: toBool(x != y))
defaultScope.atoms['lt']  = PythonFunctionValue(2, lambda x,y: toBool(x <  y))
defaultScope.atoms['lte'] = PythonFunctionValue(2, lambda x,y: toBool(x <= y))
defaultScope.atoms['gt']  = PythonFunctionValue(2, lambda x,y: toBool(x >  y))
defaultScope.atoms['gte'] = PythonFunctionValue(2, lambda x,y: toBool(x >= y))
defaultScope.atoms['null'] = None

printWrapperLastNewline = True
def printWrapper(x):
	global printWrapperLastNewline
	if type(x) == str and x.endswith('\n'):
		printWrapperLastNewline = True
		sys.stdout.flush()
	else:
		if printWrapperLastNewline:
			printWrapperLastNewline = False
		else:
			sys.stdout.write(' ')
	sys.stdout.write( unicode(x) ) # FIXME: Unicode
	return printWrapperValue
printWrapperValue = PythonFunctionValue(1, printWrapper)
defaultScope.atoms['print'] = printWrapperValue

def printlnWrapper(x):
	print unicode(x)
	return printlnWrapperValue
printlnWrapperValue = PythonFunctionValue(1, printlnWrapper)
defaultScope.atoms['println'] = printlnWrapperValue

defaultScope.atoms['exit'] = PythonFunctionValue(1, sys.exit)
defaultScope.atoms['ln'] = "\n"