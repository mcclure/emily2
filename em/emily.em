# Self-hosting interpreter for e2

let false = null
let true = 1

let cmdAst = false
let cmdAst2 = false
let cmdExecute = null
let cmdTarget = null
let cmdValid = false

let scriptArgv = array()

# --- Parse arguments ---

let i = argv.iter
while (i.more)
	let arg = i.next

	with arg match
		"--ast" =
			cmdAst = true
		"--ast2" =
			cmdAst2 = true
		"-e" = do
			if (not (i.more))
				# TODO Write on stderr not stdout
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

# --- Util ---

let lastFrom = function(a)
	a (- (a.length) 1)

# Linked list / stack object
let Linked = inherit object
	field value = null
	field next = null # This looks like an iterator but is immutable. Is this bad
	method more = (!= (this.next) null)

let cmp = function (x, y)
	if (< x y)
		-1
	elif (> x y)
		1
	else
		0

let insertLinked = function(cmp, list, value)
	let worse = function(node)
		or (not list) (> 0 (cmp value (node.value)))
	insert = new Linked(value)
	if (worse list)
		insert.next = list
		insert
	else
		let node = list
		let done = false
		while (and (not done) node)
			if (worse (node.next))
				insert.next = node.next
				node.next = insert
				done = true
		list

let foldl = function(default, f, ary)
	let i = ary.iter
	if (not (i.more))
		default
	else
		let value = i.next
		while (i.more)
			value = f(value, i.next)
		value

# String ops
let join = function(joiner)
	foldl "" function (x,y) ( +( +(x.toString, joiner), y.toString) )
let nullJoin = join ""
let startsWith = function(x, y)
	let idx = 0
	let valid = (<= (y.length) (x.length)) # Don't bother if x is shorter
	while (and valid (< idx (y.length)))   # Iterate until difference found
		if (!= (x idx) (y idx))
			valid = false
		idx = + idx 1
	valid

# --- Core types ---

let ProgressBase = inherit object
	none = 0   
	parsed = 1000      # Parse tree structure
	macroed = 2000     # "AST" structure
	executable = 3000  # Execution tree structure

let Loc = inherit object
	field line = 0
	field char = 0

	method toString = nullJoin array
		"line "
		this.line
		" char "
		this.char

let Node = inherit object
	field loc = null
	progress = ProgressBase.none

let Error = inherit object
	field loc = null
	field msg = null

# --- Reader ---

let ExpGroup = inherit Node
	field openedWithParenthesis = false
	field indent = null
	field method statements = array( new Statement )

	method finalStatement = lastFrom (this.statements)
	method toString = nullJoin array
		"("
		join(", ", this.statements)
		")"

let StringContentExp = inherit Node
	field content = ""

let SymbolExp = inherit StringContentExp
	field isAtom = false

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
	field dead = false

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
	let appendStatement = function ()
		finalGroup.statements.append (new Statement)

	let error = function(str)
		errors.append
			new Error(loc, str)
		finalGroup.finalStatement.dead = true
		nextState Scanning

	let State = inherit object
		enter = nullfn
		leave = nullfn
		handle = nullfn
	let state = null
	let nextState = function (x)
		state.leave()
		state = x
		state.enter()

	let newline = function ()
		lineAt = + lineAt 1
		charAt = 0
	let handleLineSpace = function(ch)
		if (== ch "\r")
			nextState Cr
		else
			nextState new Indent

	let BasicState = inherit State
		subHandle = nullfn
		method handle = function(ch)
			if (char.isLineSpace ch)
				handleLineSpace ch
			elif (char.isOpenParen ch)
				appendGroup (StatementKind.Parenthesis)
				nextState Scanning
			elif (char.isCloseParen ch)
				let done = false
				let node = groupStack
				while (and (not done) node)
					if (node.value.openedWithParenthesis)
						done = true
					node = node.next

				if (not done)
					error "Close parenthesis mismatched"
				else
					groupStack = node
					nextState Scanning
			elif (== ch ",")
				appendStatement()
				nextState Scanning
			elif (== ch "#")
				nextState Comment
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

	let Cr = inherit State
		handle = function(ch)
			nextState new Indent
			if (!= ch "\n") # Eat LFs, handle everything else
				state.handle ch

	let Indent = inherit State
		field current = ""
		method handle = function(ch)
			if (char.isLineSpace ch)
				handleLineSpace ch
			elif (char.isNonLineSpace ch)
				this.current = + (this.current) ch
			elif (== ch ",")
				error "Comma at start of line not understood"
				nextState Scanning
			elif (== ch "#")
				nextState Comment
			else # Non-whitespace content
				if (char.isCloseParen ch)
					1	# Do nothing-- just switch to scan
				elif (== (finalGroup.indent) null) # First non-whitespace content of group
					finalGroup.indent = this.current

					if (> (finalGroup.finalStatement.nodes.length) 0)
						appendStatement()
						error "Indentation after ( is ambiguous; please add a , at the end of the previous line."
				elif (== (this.current) (finalGroup.indent))
					appendStatement()
				elif (startsWith (this.current) (finalGroup.indent)) # Added indentation
					appendGroup (StatementKind.Indent)
					finalGroup.indent = this.current
				else # This is either dropped indentation, or an error
					let done = false
					let parenthesisIssue = false
					let node = groupStack
					while (and (not done) node)
						if (== (node.value.indent) (this.current))
							done = true
						elif (node.value.openedWithParenthesis)
							parenthesisIssue = true
							done = true
						else
							node = node.next

					if (not done)
						error "Indentation on this line doesn't match any previous one"
					elif (parenthesisIssue)
						error (+ "Indentation on this line doesn't match any since open parenthesis at " (finalGroup.loc.toString))
					else
						groupStack = node
						appendStatement()

				nextState Scanning
				state.handle ch

	let Comment = inherit State
		handle = function(ch)
			if (char.isLineSpace ch)
				handleLineSpace ch

	state = new Indent
	appendGroup (StatementKind.Outermost)

	while (i.more)
		let ch = i.next
		state.handle ch
		charAt = + charAt 1

	while (groupStack.more)
		if (finalGroup.openedWithParenthesis)
			errors.append
				new Error(loc, nullJoin array("Parenthesis on ", groupStack.value.loc, " never closed"))
		groupStack = groupStack.next

	if (< 0 (errors.length))
		let i = errors.iter
		stderr.println "Compilation failed:"
		while (i.more)
			let error = i.next
			stderr.println
				nullJoin array (error.loc, ": ", error.msg)
		exit 1
	
	finalGroup

# --- Parser ---

let Macro = inherit object
	progress = ProgressBase.parsed

let insertMacro = insertLinked function(x,y)
	cmp (x.progress) (y.progress)

let Parser = inherit object
	field macros = null # Linked[Macro]
	field errors = array()

	method makeSequence = function(loc, statements, shouldReturn)
		"MACROS CURRENTLY UNIMPLEMENTED"

let exeFromAst = function (ast)
	let parser = new Parser
	parser.makeSequence(ast.loc, ast.statements, false)

# --- Run ---

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
	let exe = exeFromAst ast

	if cmdAst2
		println (exe.toString)
	else
		println "EXECUTION CURRENTLY UNIMPLEMENTED"