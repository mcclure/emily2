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

let popLeft = function(a)
	let left = a 0
	let idx = 0
	while (< idx (- (a.length) 1))
		a idx = a (+ idx 1)
		idx = + idx 1
	a.pop
	left

# Linked list / stack object
let Linked = inherit object
	field value = null
	field next = null # This looks like an iterator but is immutable. Is this bad
	method more = (!= (this.next) null)

let cloneLinked = function(list)
	if (list)
		let head = new Linked
		let node = head
		while (list)
			let next = new Linked(list.value)
			node.next = next
			list = list.next
		head.next
	else
		null

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

let checkErrors = function(errors)
	if (< 0 (errors.length))
		let i = errors.iter
		stderr.println "Compilation failed:"
		while (i.more)
			let error = i.next
			stderr.println
				nullJoin array (error.loc, ": ", error.msg)
		exit 1

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

# TODO: Important question to consider:
# Is it actually useful to have macro stages fit "between" parser and executable? 
# I think I originally imagined macros would update nodes to "at least" their own
# progress level, but in practice it seems macros only update targets all the way to
# "executable". Maybe macro-priority and progress should be two separate enums
let ProgressBase = inherit object
	none = 0   
	reader = 1000      # Expression tree structure
	parser = 2000      # "AST" structure
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
	method nonempty = > (this.statements.length) 0

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

	checkErrors errors
	
	finalGroup

# --- Parser ---

# Base/helper

let Macro = inherit object
	progress = ProgressBase.reader

let insertMacro = insertLinked function(x,y)
	cmp (x.progress) (y.progress)

let isSymbol = function(node, goal)
	with node match
		SymbolExp = and (not node.isAtom) (== (node.content) goal)
		_ = false

let SequenceTracker = inherit object
	field statements = null
	field idx = 0

	method more = > (this.statements.length) (this.idx)
	method next = do
		let result = this.statements (this.idx)
		this.idx = + (this.idx) 1
		result

	method steal = function(symbol)
		if (this.more)
			let nodes = this.statements(this.idx).nodes
			if (and nodes (isSymbol (nodes 0) symbol))
				this.next
				nodes
			else
				null
		else
			null

# Macro classes


# Parser

let ProcessResult = inherit object
	field left = null
	field at = null
	field right = null

let Parser = inherit object
	field macros = null # Linked[Macro]
	field errors = array()

	method makeSequence = function(loc, statements, shouldReturn)
		let execs = array()
		let tracker = new SequenceTracker(statements)
		let parser = this
		let customMacros = false
		let hasLets = false

		while (tracker.more)
			let statement = tracker.next

			let exe = parser.process(loc, statement.nodes, tracker)
			if (is UserMacroList exe)
				if (not customMacros)   # TODO: Only dupe after descending levels
					parser = cloneParser(parser)
					customMacros = true
				m.loadAll(exe.contents)
			else
				execs.append(exe)

		while (and (not hasLets) (i.more))
			let exe = i.next
			if (and (is SetExec exe) (exe.isLet))
				hasLets = true

		new SequenceExec(loc, shouldReturn, hasLets, execs)

	method loadAll = function(macros)
		let i = macros.iter
		while (i.more)
			let add = i.next
			this.macros = insertMacro(this.macros, add)

	method error = function(loc, msg)
		this.errors.append( new Error(loc, msg) )
		new InvalidExec(loc)

	method checkComplete = function(node)
		if (< (node.progress) (ProgressBase.executable))
			this.error(node.loc, "Macro malfunctioned and left an unfinished node here at end of processing")
		else
			null

	method process = function(loc, nodes, tracker)
		# Callers must define the semantics of empty lists themselves.
		if (== (nodes.length) 0)
			error(loc, "Internal error: Parser attempted to evaluate an empty statement. This is a bug in the interpreter.")
		
		# First, apply all macros to the statement.
		let macroNode = this.macros
		let foundError = null
		while (and macroNode (not foundError))
			let m = macroNode.value
			let left = array()
			let right = nodes

			# One by one move the items out of right into left and macro-filter along the way
			while (and (right.length) (not foundError))
				let at = popLeft right
				if (> (at.progress) (level.progress))
					left.append at
				else
					# FIXME: Need to bring in "macro levels" concept from parser.py
					# So that same-priority keys get processed in a single sweep
					if (m.matches(left, at, right))
						let result = m.apply(left, at, right, tracker)

						# Unpack
						with result match
							Error =
								foundError = result
							ProcessResult(_left, _at, _right) = do
								left = _left
								at = _at
								right = _right

						if (at)
							left.append at

				nodes = left

			macroNode = macroNode.next

		if (foundError)
			foundError
		elif (not (nodes.length))
			# TODO: Try to figure out which macro? parser.py assumes "at" is the culprit...
			this.error(loc, "Macro malfunctioned and produced an empty list")
		else
			# We now have a list of progress=Macroed symbols. Treat the list as curried applications.
			# We need to pack the list of values down into one application tree & sanitize with "checkComplete"
			let result = null       # Current "leftmost value"
			let resultError = null  # Short-circuit if a problem occurs

			# The "result" value starts as the zeroth item in the list
			if (is ExpGroup (nodes 0))
				let firstNode = popLeft nodes
				with (firstNode.statements.length) match
					0 = # ()
						result = new Unit (firstNode.loc)
					1 = # (arg)
						result = this.process (firstNode.loc, firstNode.statements(0).nodes, null)
					_ = # (arg1, arg2, ...)
						resultError = this.error (firstNode.loc, "Line started with a multiline parenthesis group. Did you mean to use \"do\"?")
			else
				result = popLeft(nodes)
				resultError = this.checkComplete result # First write so this is safe

			# For each item in the list, apply result = result(b)
			while (and (not resultError) (nodes.length))
				let arg = popLeft(nodes)
				if (is ExpGroup arg)
					if (not (arg.nonempty)) # ()
						result = new ApplyExec(arg.loc, result, new Unit(arg.loc))
					else                    # (arg1, arg2, ...)
						let argIdx = 0 # Used by error message
						let tracker = new SequenceTracker(arg.statements)
						while (tracker.more)
							let statement = tracker.next
							argIdx = + argIdx 1
							if (not (statement.nodes.length))
								resultError = this.error
									arg.loc
									nullJoin
										"Argument #"
										argIdx
										" to function is blank"
							else
								result = new ApplyExec(arg.loc, result, this.process(arg, statement.nodes, tracker))
				else
					resultError = this.checkComplete arg
					if (not resultError)
						result = new ApplyExec(arg.loc, result, arg)

			if resultError
				resultError
			else
				result

let cloneParser = function(parser)
	new Parser(cloneLinked (parser.macros), parser.errors)

let exeFromAst = function(ast)
	let parser = new Parser
	let result = parser.makeSequence(ast.loc, ast.statements, false)

	checkErrors(parser.errors)

	result

# --- Execution ---

let Executable = inherit Node
	progress = ProgressBase.executable

# TODO: toString, eval

let InvalidExec = inherit Executable
	toString = "[Invalid node]"

let SequenceExec = inherit Executable
	field shouldReturn = false
	field hasScope = false
	field method execs = array()

	method toString = do
		let tags = array()
		if (this.hasScope)
			tags.append "Scoped"
		if (this.shouldReturn)
			tags.append "Returning"
		nullJoin array
			"[Sequence"
			if (tags.length)
				nullJoin("(", join "," tags, ")")
			else
				""
			join " " (this.execs)
			"]"

let UserMacroList = inherit Executable
	field contents = null

	toString = "[Misplaced macro node]"

let NullLiteralExec = inherit Executable
	toString = "[NullLiteral]"

let ApplyExec = inherit Executable
	field fn = null
	field arg = null

	method toString = nullJoin
		"[Apply "
		fn
		" "
		arg
		"]"

let Unit = NullLiteralExec # Just an alias

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
