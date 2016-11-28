# Execution tree classes

from core import *
from util import unicodeJoin, quotedString

class Executable(Node):
	def __init__(s, immutable = False): # , line, char
		super(Executable, s).__init__()
		s.immutable = immutable
		s.progress = ProgressBase.Executable
		#s.line = line
		#s.char = char

class SequenceExec(Executable):
	def __init__(s, execs):
		s.execs = execs

	def __unicode__(s):
		return u"[Sequence %s]" % (unicodeJoin(u" ", s.execs))

class LiteralExec(Executable):
	def __init__(s, source):
		super(LiteralExec, s).__init__() # source.line, source.char

class StringLiteralExec(Executable):
	def __init__(s, source, value):
		super(StringLiteralExec, s).__init__(source)
		s.value = value

	def __unicode__(s):
		return u"[StringLiteral %s]" % (quotedString(s.value))

class NumberLiteralExec(Executable):
	def __init__(s, source, value):
		super(NumberLiteralExec, s).__init__(source)
		s.value = value

	def __unicode__(s):
		return u"[NumberLiteral %s]" % (s.value)

class AtomLiteralExec(Executable):
	def __init__(s, source, value):
		super(NumberLiteralExec, s).__init__(source)
		s.value = value

	def __unicode__(s):
		return u"[AtomLiteral %s]" % (s.value)

class IfExec(Executable):
	def __init__(s, loop, condClause, ifClause, elseClause):
		super(IfExec, s).__init__()
		s.loop = loop
		s.condClause = condClause
		s.ifClause = ifClause
		s.elseClause = elseClause

	def __unicode__(s):
		return u"[%s %s]" % ("While" if s.loop else "If",
			unicodeJoin(u" ", [s.condClause, s.ifClause] + ([s.elseClause] if s.elseClause else [])))

class VarExec(Executable):
	def __init__(s, symbol, source=None):
		super(VarExec, s).__init__()
		s.source = source
		s.symbol = symbol

	def __unicode__(s):
		return u"[Var %s]" % (s.symbol)

class SetExec(Executable):
	def __init__(s, symbol, valueClause, source=None):
		super(SetExec, s).__init__()
		s.source = source
		s.symbol = symbol
		s.valueClause = valueClause

	def __unicode__(s):
		return u"[Set %s %s]" % (s.symbol, unicode(s.valueClause))

class ApplyExec(Executable):
	def __init__(s, f, arg): # f for function
		super(ApplyExec, s).__init__() # FIXME: f.location
		s.f = f
		s.arg = arg

	def __unicode__(s):
		return u"[Apply %s]" % (unicodeJoin(u" ", [s.f, s.arg]))