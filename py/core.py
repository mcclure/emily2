# Shared classes used by all bits of the parse chain

# Every Node has a progress field. The progress of a node >= the progress of its children (if any)

# Progress comes in several phases:
class ProgressBase:
	Parsed = 1000      # Parse tree structure
	Macroed = 2000     # "AST" structure
	Executable = 3000  # Execution tree structure

class EmilyException(Exception):
	pass

class Printable(object):
	def __str__(s):
		return unicode(s).encode('utf-8')

class Node(Printable):
	# Currently the parser handles ., comma and () itself so lowest possible is 1000
	def __init__(s, loc, progress = ProgressBase.Parsed):
		s.loc = loc
		s.progress = progress

class Error(object):
	def __init__(s, loc, msg):
		s.loc = loc
		s.msg = msg

class Loc(Printable):
	def __init__(s, line, char):
		s.line = line
		s.char = char
