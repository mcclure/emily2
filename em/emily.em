# Self-hosting interpreter for e2

let cmdAst = null
let cmdAst2 = null
let cmdExecute = null
let cmdTarget = null
let cmdValid = null

let scriptArgv = array()

# Parse arguments

let i = argv.iter
while (i.more)
	let arg = i.next

	with arg match
		"--ast" =
			cmdAst = 1
		"--ast2" =
			cmdAst2 = 1
		"-e" = do
			if (not (i.more))
				# TODO Write on stderr not stdout
				println "Missing argument for -e"
				exit 2
			cmdExecute = i.next
		_ = do
			# TODO: Throw if starts with -
			cmdTarget = arg

	if (or(cmdExecute, cmdTarget))
		while (i.more)
			scriptArgv.append(i.next)
		cmdValid = 1

if (not cmdValid)
	println "Must supply either file name or -e"
	exit 2

println
	"ok"
	cmdExecute
	cmdTarget
