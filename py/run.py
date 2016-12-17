# Execution engine

class StackFrame(object):
	def __init__(s, scope):
		s.scope = scope
		s.ownScope = False

class ExecutionEngine(object):
	def __init__(s):
		s.stack = []
