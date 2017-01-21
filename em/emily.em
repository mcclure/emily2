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

	method finalStatement = lastFrom (this.statements)

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

let StatementKind = inherit object
let StatementKind.Outermost = inherit StatementKind
let StatementKind.Indent = inherit StatementKind
let StatementKind.Parenthesis = inherit StatementKind

let makeAst = function(i)
	let line = 1
	let char = 0
	let groupStack = array()
	let errors = array()

	let method loc = new Loc(line, char)
	let method finalGroup = lastFrom (this.groupStack)
	let appendExp = function (node)
		finalGroup.finalStatement.nodes.append node
	let appendGroup = function (statementKind)
		let group = new ExpGroup(== statementKind (StatementKind.Parenthesis))
		if (!= statementKind (StatementKind.Outermost))
			appendExp group
		groupStack.append group

	appendGroup (StatementKind.Outermost)

	while (i.more)
		let ch = i.next

	if (> 0 (errors.length))
		print(errors.length, "errors", ln)
		exit 1
	
	finalGroup

let codeIter = with cmdTarget match
	null = cmdExecute.iter
	_ =    file.in cmdTarget

let ast = makeAst codeIter

if (cmdTarget)
	codeIter.close

println
	"ok"
	cmdExecute
	cmdTarget

