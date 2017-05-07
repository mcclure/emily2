# Shared classes used by all bits of the interpreter chain

# TODO: Important question to consider:
# Is it actually useful to have macro stages fit "between" parser and executable? 
# I think I originally imagined macros would update nodes to "at least" their own
# progress level, but in practice it seems macros only update targets all the way to
# "executable". Maybe macro-priority and progress should be two separate enums
let ProgressBase = inherit Object
	none = 0   
	reader = 1000      # Expression tree structure
	parser = 2000      # "AST" structure
	executable = 3000  # Execution tree structure

let Loc = inherit Object
	field line = 0
	field char = 0

	method toString = nullJoin array
		"line "
		this.line
		" char "
		this.char

let Node = inherit Object
	field loc = null
	progress = ProgressBase.none

let Error = inherit Object
	field loc = null
	field msg = null