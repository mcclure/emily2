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

# TODO: Can any code be shared with PythonFunctionValue?
class FunctionValue(object):
	def __init__(s, argNames, exe, scope, startingArgs = []): # Takes ownership of startingArgs
		s.argNames = argNames
		s.exe = exe
		s.scope = scope
		s.args = startingArgs

	def apply(s, value):
		if not s.argNames:
			return s.exe.eval(s.scope)
		newArgs = s.args + [value]
		if len(newArgs) >= len(s.argNames):
			scope = ObjectValue(s.scope)
			for idx in range(len(s.argNames)):
				scope.atoms[s.argNames[idx]] = newArgs[idx]
			return s.exe.eval(scope)
		return FunctionValue(s.argNames, s.exe, s.scope, newArgs)

class ObjectValue(object):
	def __init__(s, parent=None):
		s.atoms = {}
		s.parent = parent

	def lookup(s, key): # Already sanitized for atom correctness
		if key in s.atoms:
			return s.atoms[key]
		if s.parent:
			return s.parent.lookup(key)
		raise Exception("Object lacks key %s" % (key))

	def assign(s, isLet, key, value):
		if isLet or key in s.atoms:
			s.atoms[key] = value
		elif s.parent:
			s.parent.assign(isLet, key, value)
		else:
			raise Exception("Object lacks key %s being set" % (key))

	def apply(s, key):
		if type(key) != AtomLiteralExec:
			raise Exception("Objects have atom keys only")
		return s.lookup(key.value)

class ArrayValue(object):
	def __init__(s, values):
		s.values = values

	def apply(s, key):
		if type(key) == float:
			key = int(key) # IS ROUND-DOWN ACTUALLY GOOD?
		if type(key) == int:
			return s.values[key]
		raise Exception("Arrays have key values only")

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
	def __init__(s, loc, shouldReturn, hasScope, execs):
		super(SequenceExec, s).__init__(loc)
		s.shouldReturn = shouldReturn
		s.hasScope = hasScope
		s.execs = execs

	def __unicode__(s):
		tags = (["Scoped"] if s.hasScope else []) + (["Returning"] if s.shouldReturn else [])
		return u"[Sequence%s %s]" % (("(%s)"%u", ".join(tags)) if tags else "", unicodeJoin(u" ", s.execs))

	def eval(s, scope):
		if s.hasScope:
			scope = ObjectValue(scope)
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
		return scope.lookup(s.symbol)

class SetExec(Executable):
	def __init__(s, loc, isLet, symbol, valueClause, source=None): # TODO: indexClauses
		super(SetExec, s).__init__(loc)
		s.isLet = isLet
		s.source = source
		s.symbol = symbol
		s.valueClause = valueClause

	def __unicode__(s):
		return u"[%s %s %s]" % ("Let" if s.isLet else "Set", s.symbol, unicode(s.valueClause))

	def eval(s, scope, targetOverride = None):
		if targetOverride:
			target = targetOverride
		elif s.source:
			target = s.source.eval(scope)
		else:
			target = scope
		target.assign(s.isLet, s.symbol, s.valueClause.eval(scope))
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

class MakeFuncExec(Executable):
	def __init__(s, loc, args, body): # f for function
		super(MakeFuncExec, s).__init__(loc) # FIXME: f.location
		s.args = args
		s.body = body

	def __unicode__(s):
		return u"[Function [%s] %s]" % (u", ".join(s.args), unicode(s.body))

	def eval(s, scope):
		return FunctionValue(s.args, s.body, scope)

class MakeArrayExec(Executable):
	def __init__(s, loc, contents):
		super(MakeArrayExec, s).__init__(loc)
		s.contents = contents

	def __unicode__(s):
		return u"[Array %s]" % (unicodeJoin(u", ", s.contents))

	def eval(s, scope):
		values = [None] * len(s.contents)
		idx = 0
		for exe in s.contents:
			values[idx] = exe.eval(scope)
			idx += 1
		return ArrayValue(values)

rootObject = ObjectValue() # Singleton "root object"

class MakeObjectExec(Executable):
	def __init__(s, loc, base, values):
		super(MakeObjectExec, s).__init__(loc)
		s.base = base
		s.values = values

	def __unicode__(s):
		return u"[New %s [%s]]" % (unicode(s.base), unicodeJoin(u", ", s.values))

	def eval(s, scope):
		base = s.base.eval(scope)
		if base == rootObject: # Tiny optimization: Don't actually inherit from Object
			base = None
		result = ObjectValue(base)
		for exe in s.values:
			exe.eval(scope, result)
		return result

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
defaultScope.atoms['=='] = PythonFunctionValue(2, lambda x,y: toBool(x == y))
defaultScope.atoms['!='] = PythonFunctionValue(2, lambda x,y: toBool(x != y))
defaultScope.atoms['<']  = PythonFunctionValue(2, lambda x,y: toBool(x <  y))
defaultScope.atoms['<='] = PythonFunctionValue(2, lambda x,y: toBool(x <= y))
defaultScope.atoms['>']  = PythonFunctionValue(2, lambda x,y: toBool(x >  y))
defaultScope.atoms['>='] = PythonFunctionValue(2, lambda x,y: toBool(x >= y))
defaultScope.atoms['null'] = None
defaultScope.atoms['object'] = rootObject

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