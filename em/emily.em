# Self-hosting interpreter for e2

from project.util import *
from project import (reader, parser, execution)

let cmdAst = false
let cmdAst2 = false
let cmdExported = false
let cmdExecute = null
let cmdTarget = null
let cmdValid = false

let scriptArgv = array()

# --- Parse arguments ---

do
	let i = argv.iter
	while (i.more)
		let arg = i.next

		with arg match
			"--ast" =
				cmdAst = true
			"--ast2" =
				cmdAst2 = true
			"--exported" =
				cmdExported = true
			"-e" = do
				if (not (i.more))
					stderr.println "Missing argument for -e"
					exit 2
				cmdExecute = i.next
			_ = do
				if (== "-" (arg 0))
					stderr.print "Unrecognized argument" arg ln
					exit 2
				cmdTarget = arg

		if (or(cmdExecute, cmdTarget))
			while (i.more)
				scriptArgv.append(i.next)
			cmdValid = true

if (not cmdValid)
	stderr.println "Must supply either file name or -e"
	exit 2

# --- Run ---

let codeIter = do
	if cmdTarget
		file.in cmdTarget
	else
		cmdExecute.iter

let ast = reader.makeAst codeIter

if cmdTarget
	execution.setEntryFile cmdTarget
	codeIter.close

if cmdAst
	println (ast.toString)
else
	let exe = parser.exeFromAst ast

	if cmdAst2
		println (exe.toString)
	else
		let scope = new ObjectValue(defaultScope)

		let scriptArgvValue = array()
		let i = scriptArgv.iter
		while (i.more)
			scriptArgvValue.append (new StringValue(i.next))
		scope.atoms.set "argv" (new ArrayValue(scriptArgvValue))
		
		let exportScope = new ObjectValue

		exe.evalSequence(scope, exportScope)

		if cmdExported
			execution.debugScopeDump(exportScope)
