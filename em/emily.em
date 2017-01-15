# Self-hosting interpreter for e2

let cmdAst = null
let cmdAst2 = null
let cmdExecute = null
let cmdTarget = null
let cmdValid = null

let scriptArgv = array()

# --- Parse arguments ---

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

# --- Util ---

let lastFrom = function(a)
	a (- (a.length) 1)

# --- Core types ---

let ProgressBase = inherit object
	none = 0   
	parsed = 1000      # Parse tree structure
	macroed = 2000     # "AST" structure
	executable = 3000  # Execution tree structure

let Loc = inherit object
	field line = 0
	field char = 0

let Node = inherit object
	field loc = null
	progress = ProgressBase.none

let Error = inherit object
	field loc = null
	field msg = null

# --- Reader ---

let ExpGroup = inherit Node
	field openedWithParenthesis = null
	field indent = null
	field statements = array()

let StringContentExp = inherit Node
	field content = ""

let SymbolExp = inherit StringContentExp
	field isAtom = null

let QuoteExp = inherit StringContentExp

let NumberExp = inherit Node
	field integer = ""
	field dot = null
	field decimal = null

let Statement = inherit object
	field nodes = array()
	field dead = null

let Reader = inherit object
	field line = 1
	field char = 0
	field groupStack = array()
	field errors = array()

	method finalGroup = lastFrom (this.groupStack)

	method ast = function (i)
		

let ast = function(iter)
	let reader = new Reader
	reader.ast iter
	if (> 0 (reader.errors.length))
		print(reader.errors.length, "errors", ln)
		exit 1
	reader.finalGroup

println
	"ok"
	cmdExecute
	cmdTarget

