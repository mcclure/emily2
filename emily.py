import optparse
import parse
import util
import sys

# Command line frontend

help  = "%prog [filename]\n"
help += "\n"
help += "Accepted arguments:\n"
help += "-e [string]               # Instead of file, execute inline string"
help += "--ast                     # Don't execute, dump AST"

parser = optparse.OptionParser(usage=help)
for a in ["-ast"]: # Single letter args, flags
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
try:
	ast = parse.ast(
		util.utf8string(flag('e')[0]) if flag('e') 
			else util.filechars(util.utfopen(cmds[0]))
		)

	if flag('ast'):
		print ast
except parse.ParseException as e:
	print >>sys.stderr, e
	sys.exit(1)
