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

let Linked = inherit object
	field value = null
	field next = null # This looks like an iterator but is immutable. Is this bad
	method more = (!= (this.next) null)

# --- Core types ---

let ProgressBase = inherit object
	none = 0   
	parsed = 1000      # Parse tree structure
	macroed = 2000     # "AST" structure
	executable = 3000  # Execution tree structure

let Loc = inherit object
	field line = 0
	field char = 0

	method toString = +( +( +("line ", this.line.toString ), " char " ), this.char.toString )

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
	field method statements = array( new Statement )

	method finalStatement = lastFrom (this.statements)
	method toString = do
		let result = "("
		let i = this.statements.iter
		while (i.more)
			if (> (result.length) 1)
				result = + result ", "
			result = + result (i.next.toString)
		result = + result ")"
		result

let StringContentExp = inherit Node
	field content = ""

let SymbolExp = inherit StringContentExp
	field isAtom = null

	method toString = +
		if (this.isAtom) (".") else ("")
		this.content

let QuoteExp = inherit StringContentExp

let NumberExp = inherit Node
	field integer = ""
	field dot = null
	field decimal = null

let Statement = inherit object
	field method nodes = array()
	field dead = null

	method toString = do
		let result = ""
		let i = this.nodes.iter
		while (i.more)
			if (> (result.length) 0)
				result = + result " "
			result = + result (i.next.toString)
		result

let StatementKind = inherit object
let StatementKind.Outermost = inherit StatementKind
let StatementKind.Indent = inherit StatementKind
let StatementKind.Parenthesis = inherit StatementKind

let makeAst = function(i)
	let lineAt = 1
	let charAt = 0
	let groupStack = null
	let errors = array()

	let method loc = new Loc(lineAt, charAt)
	let method finalGroup = groupStack.value
	let method lastExp = lastFrom (finalGroup.finalStatement.nodes)
	let appendExp = function (node)
		finalGroup.finalStatement.nodes.append node
	let appendGroup = function (statementKind)
		let group = new ExpGroup(loc, == statementKind (StatementKind.Parenthesis))
		if (!= statementKind (StatementKind.Outermost))
			appendExp group
		groupStack = new Linked(group, groupStack)

	let State = inherit object
		enter = nullfn
		leave = nullfn
		handle = nullfn
	let state = null
	let method nextState = function (x)
		state.leave()
		state = x
		state.enter()

	let BasicState = inherit State
		subHandle = nullfn
		method handle = function(ch)
			if (char.isOpenParen ch)
				appendGroup (StatementKind.Parenthesis)
				nextState Scanning
			else
				if (char.isCloseParen ch)
					if (finalGroup.openedWithParenthesis)
						groupStack = groupStack.next
						nextState Scanning
					else
						fail "Close parenthesis mismatched"
				else
					this.subHandle ch

	let Scanning = inherit BasicState
		subHandle = function(ch)
			if (not (char.isNonLineSpace ch))
				nextState Symbol
				state.handle ch

	let Symbol = inherit BasicState
		enter = function(ch)
			appendExp
				new SymbolExp
		subHandle = function(ch)
			if (char.isSpace ch)
				nextState Scanning
				state.handle ch
			else
				let e = lastExp
				e.content = + (e.content) ch

	state = Scanning
	appendGroup (StatementKind.Outermost)

	while (i.more)
		let ch = i.next
		state.handle ch
		charAt = + charAt 1

	if (groupStack.more)
		errors.append
			new Error(loc, +( +("Parenthesis on ", groupStack.value.loc.toString), " never closed"))

	if (< 0 (errors.length))
		let i = errors.iter
		println "Compilation failed:"
		while (i.more)
			let error = i.next
			println ( +( +(error.loc.toString, ": "), error.msg ) )
		exit 1
	
	finalGroup

let codeIter = do
	if cmdTarget
		file.in cmdTarget
	else
		cmdExecute.iter

let ast = makeAst codeIter

if cmdTarget
	codeIter.close

if cmdAst
	println (ast.toString)
else
	println "EXECUTION CURRENTLY UNIMPLEMENTED"
