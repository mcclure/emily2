#!/usr/bin/env python

# Entry point / command line interface

import optparse
import parse
import macro
import util
import sys
import execution

# Command line frontend

help  = "%prog [filename]\n"
help += "\n"
help += "Accepted arguments:\n"
help += "-e [string]               # Instead of file, execute inline string\n"
help += "--ast                     # Don't execute, dump AST\n"
help += "--ast2                    # Don't execute, run macros and dump AST"

parser = optparse.OptionParser(usage=help)
for a in ["-ast", "-ast2"]: # Single letter args, flags
    parser.add_option("-"+a, action="store_true")
for a in ["e"]: # Long args with arguments
    parser.add_option("-"+a, action="append")

(options, cmds) = parser.parse_args()

def flag(a, b=None):
    x = getattr(options, a)
    if x:
        return x
    return []

if flag('e'):
	if len(flag('e')) > 2:
		parser.error("Multiple -e arguments seen")
	if cmds:
		parser.error("Both -e and filename seen")
else:
	if not cmds:
		parser.error("No file given")
	if len(cmds) > 1:
		parser.error("Multiple files given")

# TODO: Convert -e to unicode

sys.setrecursionlimit(10000)

try:
	ast = parse.ast(
		util.utf8string(flag('e')[0]) if flag('e') 
			else util.filechars(util.utfopen(cmds[0]))
		)

	if flag('ast'):
		print ast
		sys.exit(0)
	
	ast = macro.exeFromAst(ast)

	if flag('ast2'):
		print ast
		sys.exit(0)

	ast.eval(execution.defaultScope)

except parse.EmilyException as e:
	print >>sys.stderr, e
	sys.exit(1)
