#!/usr/bin/env python

# Entry point / command line interface

import optparse
import sys

import core
import util
import reader
import parser
import execution

# Command line frontend -- mimic optparse

help  = "{0} [filename] [program-args]\n"
help += "\n"
help += "Accepted arguments:\n"
help += "-e [string]               # Instead of file, execute inline string\n"
help += "--ast                     # Don't execute, dump AST\n"
help += "--ast2                    # Don't execute, run macros and dump AST"

cmd_store_true = ["--ast", "--ast2"] # Single letter args, flags
cmd_append = ["-e"] # Long args with arguments

def cmd_strip(s):
	return s.lstrip('-')
def cmd_fail(msg):
	inner_help = help.format(sys.argv[0])
	out = "%s: Error: %s\n\nUsage:\n%s\n" % (sys.argv[0], msg, inner_help)
	sys.stderr.write(out)
	sys.exit(2)

options, target, argv = {}, None, []

def flag(a, b=None):
    if a in options:
    	return options[a]
    return []

# Parse arguments


loop_loading = None
for i in range(1,len(sys.argv)):
	arg = sys.argv[i]

	if loop_loading:
		loop_loadkey = cmd_strip(loop_loading)
		if loop_loadkey in options:
			options[loop_loadkey].append(arg)
		else:
			options[loop_loadkey] = [arg]
		loop_loading = None
	elif arg[0] == '-':
		if arg in cmd_store_true:
			options[cmd_strip(arg)] = True
		elif arg in cmd_append:
			loop_loading = arg
			continue
		else:
			cmd_fail("Option not recognized: %s" % (arg))
	else:
		target = arg

	if target or 'e' in options:
		argv = sys.argv[i+1:]
		break

if loop_loading:
	cmd_fail("No such option: %s" % (loop_loading))

# Interpret parsed arguments

if flag('e'):
	if len(flag('e')) > 2:
		cmd_fail("Multiple -e arguments seen")
else:
	if not target:
		cmd_fail("No file given")

# TODO: Convert -e to unicode

sys.setrecursionlimit(10000)

try:
	ast = reader.ast(
		util.utf8string(flag('e')[0]) if flag('e') 
			else util.filechars(util.utfopen(target))
	)

	if flag('ast'):
		print ast
		sys.exit(0)
	
	ast = parser.exeFromAst(ast)

	if flag('ast2'):
		print ast
		sys.exit(0)

	scope = execution.ObjectValue(execution.defaultScope)
	scope.atoms['argv'] = execution.ArrayValue(argv)
	ast.eval(scope)

except core.EmilyException as e:
	print >>sys.stderr, e
	sys.exit(1)
