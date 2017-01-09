# Execution tree classes

import sys
from core import *
from util import unicodeJoin, quotedString
import parser

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

def isImpl(parent, child):
	if parent == child:
		return True
	if type(child) == ObjectValue:
		if parent == rootObject:
			return True
		while child.parent:
			child = child.parent
			if parent == child:
				return True
	return False

class MatchFunctionValue(object):
	def __init__(s, matches, scope):
		s.matches = matches
		s.scope = scope

	def apply(s, value):
		for m in s.matches:
			if not m.targetExe or isImpl(m.targetExe.eval(s.scope), value):
				scope = s.scope
				if m.unpacks:
					scope = ObjectValue(scope)
					unpackIdx = 0
					for atom in m.unpacks:
						scope.atoms[atom.value] = value.apply(unpackIdx)
						unpackIdx += 1
				return m.statement.eval(scope)
		raise Exception("No match met")

class SuperValue(object):
	def __init__(s, parent, target):
		s.parent = parent
		s.target = target

	def apply(s, key):
		if type(key) != AtomLiteralExec:
			raise Exception("Objects have atom keys only")
		return MethodPseudoValue.fetch(s.parent, key.value, s.target)

# Pseudovalue since it can never escape
class MethodPseudoValue(object):
	def __init__(s, scope=None, owner=None, exe=None, pythonFunction=None):
		s.scope = scope
		s.owner = owner
		s.exe = exe
		s.pythonFunction = pythonFunction

	def call(s, target):
		if s.pythonFunction:
			return s.pythonFunction.apply(target)
		else:
			scope = ObjectValue(s.scope)
			scope.atoms['this'] = target
			scope.atoms['current'] = s.owner
			scope.atoms['super'] = SuperValue(s.owner.parent, target)
			return s.exe.eval(scope)

	@staticmethod
	def fetch(source, key, this):
		value = source.innerLookup(key)
		if type(value) == MethodPseudoValue:
			return value.call(this)
		return value

class ObjectValue(object):
	def __init__(s, parent=None, fields=None):
		s.atoms = {}
		s.parent = parent
		s.fields = fields

	def key(s, key):
		if type(key) == float:
			key = int(key)
		if type(key) == int:
			key = s.fields[key]
		if type(key) != AtomLiteralExec:
			raise Exception("Object has atom%s keys only" % (" or numeric" if s.fields else ""))
		return key

	def innerLookup(s, key): # Already sanitized for atom correctness, method irrelevant
		if key in s.atoms:
			return s.atoms[key]
		if s.parent:
			return s.parent.innerLookup(key)
		raise Exception("Object lacks key %s" % (key))

	def lookup(s, key): # Already sanitized for atom correctness
		return MethodPseudoValue.fetch(s, key, s)

	def apply(s, key):
		key = s.key(key)
		return s.lookup(key.value)

	def innerAssign(s, isLet, key, value): # Again, sanitized for atom correctness
		if isLet or key in s.atoms:
			s.atoms[key] = value
		elif s.parent:
			s.parent.innerAssign(isLet, key, value)
		else:
			raise Exception("Object lacks key %s being set" % (key))

	def assign(s, isLet, key, value):
		key = s.key(key)
		return s.innerAssign(isLet, key.value, value)

arrayPrototype = ObjectValue()
arrayPrototype.atoms['length'] = MethodPseudoValue(pythonFunction=PythonFunctionValue(1, lambda x:float(len(x.values))))
arrayPrototype.atoms['append'] = MethodPseudoValue(pythonFunction=PythonFunctionValue(2, lambda x,y:x.values.append(y)))

class ArrayValue(object):
	def __init__(s, values):
		s.values = values

	def apply(s, key):
		if type(key) == float:
			key = int(key) # IS ROUND-DOWN ACTUALLY GOOD?
		if type(key) == int:
			return s.values[key]
		if type(key) == AtomLiteralExec:
			return MethodPseudoValue.fetch(arrayPrototype, key.value, s)
		raise Exception("Arrays have number keys only")

	def assign(s, isLet, key, value):
		if type(key) == float:
			key = int(key)
		elif type(key) != int:
			raise Exception("Arrays have int keys only")
		s.values[key] = value
		return None

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

class MakeMatchExec(Executable):
	def __init__(s, loc, matches):
		super(MakeMatchExec, s).__init__(loc)
		s.matches = matches

	def __unicode__(s):
		result = u"[Match"
		for match in s.matches:
			result += " [Case %s [%s] %s]" % (unicode(this.targetExe), unicodeJoin(", ", this.unpacks), unicode(this.statement))
		result += "]"
		return result

	def eval(s, scope):
		return MatchFunctionValue(s.matches, scope)

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
	def __init__(s, loc, isLet, isMethod, isField, target, index, valueClause): # TODO: indexClauses
		super(SetExec, s).__init__(loc)
		s.isLet = isLet
		s.isMethod = isMethod
		s.isField = isField
		s.target = target
		s.index = index
		s.valueClause = valueClause

	def __unicode__(s):
		return u"[%s %s %s %s]" % ("Let" if s.isLet else "Set", unicode(s.target) if s.target else u"Scope", unicode(s.index), unicode(s.valueClause))

	def eval(s, scope, targetOverride = None, indexOverride = None):
		if targetOverride:
			target = targetOverride
		elif s.target:
			target = s.target.eval(scope)
		else:
			target = scope

		if indexOverride: # FIXME: Will act surprisingly if index evaluates to None
			index = indexOverride
		else:
			index = s.index.eval(scope)

		if s.isMethod:
			value = MethodPseudoValue(scope, target, s.valueClause)
		else:
			value = s.valueClause.eval(scope)

		target.assign(s.isLet, index, value)
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
	def __init__(s, loc, base, values, assigns, isInstance):
		super(MakeObjectExec, s).__init__(loc)
		s.base = base
		s.values = values
		s.assigns = assigns
		s.isInstance = isInstance

	def __unicode__(s):
		return u"[%s %s [%s]]" % ("New" if s.isInstance else "Inherit", unicode(s.base), unicodeJoin(u", ", s.values))

	def eval(s, scope):
		base = s.base.eval(scope)
		if base == rootObject: # Tiny optimization: Don't actually inherit from Object
			base = None
		infields = base.fields if base else None
		result = ObjectValue(base)
		if s.isInstance and infields:
			for field in infields:
				result.assign(True, field, base.apply(field))
		valueProgress = 0
		for exe in s.values:
			value = exe.eval(scope)
			result.atoms[ infields[valueProgress].value ] = value
			valueProgress += 1
		for exe in s.assigns:
			key = exe.index.eval(scope) # do this early for field handling
			if exe.isField:
				if type(key) != AtomLiteralExec:
					raise "Objects have atom keys only"
				if not result.fields:
					result.fields = list(infields) if infields else []
				result.fields.append(key)
			exe.eval(scope, result, key)
		if not result.fields:
			result.fields = infields
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
defaultScope.atoms['with'] = PythonFunctionValue(2, lambda x,y: y.apply(x))
defaultScope.atoms['is'] = PythonFunctionValue(2, isImpl)

def makeSplitMacro(progress, symbol):
	if progress < 0 or progress >= 1000:
		raise Exception("Macro progress must be between 0 and 999 inclusive")
	if type(symbol) != unicode:
		raise Exception("Macro symbol is not a symbol")
	return parser.SplitMacro(ProgressBase.Macroed + progress, symbol)
defaultScope.atoms['splitMacro'] = PythonFunctionValue(2, makeSplitMacro)

def printable(x):
	return unicode(x) if x is not None else "null"

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
	sys.stdout.write( printable(x) ) # FIXME: Unicode
	return printWrapperValue
printWrapperValue = PythonFunctionValue(1, printWrapper)
defaultScope.atoms['print'] = printWrapperValue

def printlnWrapper(x):
	print printable(x)
	return printlnWrapperValue
printlnWrapperValue = PythonFunctionValue(1, printlnWrapper)
defaultScope.atoms['println'] = printlnWrapperValue

defaultScope.atoms['exit'] = PythonFunctionValue(1, sys.exit)
defaultScope.atoms['ln'] = "\n"
defaultScope.atoms['argv'] = ArrayValue([])