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

do
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
		if (not node)
			true
		else
			> 0 (cmp value (node.value))
	let insert = new Linked(value)
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
			node = node.next
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

let nonempty = function (ary)
	if ary (ary.length)

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

let quotedString = function(s)
	let result = "\""
	let i = s.iter
	while (i.more)
		result = + result do
			let ch = i.next
			with (ch) match
				"\n" = "\\n"
				"\r" = "\\r"
				"\"" = "\\\""
				_ = ch
	result = + result "\""
	result

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
	method empty = or
		not (this.statements.length)
		not (this.statements(0).nodes.length)

let StringContentExp = inherit Node
	field content = ""

let SymbolExp = inherit StringContentExp
	field isAtom = false

	method toString = +
		if (this.isAtom) (".") else ("")
		this.content

let QuoteExp = inherit StringContentExp
	method toString = quotedString (this.content)

let NumberExp = inherit Node
	field integer = ""
	field dot = null
	field decimal = null

	method appendDot = function()
		this.dot = true
		this.decimal = ""

	method toString = nullJoin array
		this.integer
		if (this.dot) (".") else ("")
		if (this.decimal) (this.decimal) else ("")

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

	let recoverableError = function(str)
		errors.append
			new Error(loc, str)

	let error = function(str)
		recoverableError str
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
			newline()
			nextState new Indent

	# States-- see big diagram comment in reader.py

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
			elif (== ch "\"")
				nextState new Quote
			else
				this.subHandle ch

	let Scanning = inherit BasicState
		subHandle = function(ch)
			if (char.isDigit ch)
				nextState Number
				state.handle ch
			elif (== ch ".")
				nextState Dot
			elif (not (char.isNonLineSpace ch))
				nextState Symbol
				state.handle ch

	let Cr = inherit State
		handle = function(ch)
			newline()
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

	let Number = inherit BasicState
		enter = function(ch)
			appendExp
				new NumberExp
		subHandle = function(ch)
			let e = lastExp
			if (char.isSpace ch)
				nextState Scanning
				state.handle ch
			elif (char.isDigit)
				if (e.dot)
					e.decimal = + (e.decimal) ch
				else
					e.integer = + (e.integer) ch
			elif (and (not (e.dot)) (== ch "."))
				e.appendDot()
			else
				nextState Scanning
				state.handle ch

	let Symbol = inherit BasicState
		enter = function(ch)
			appendExp
				new SymbolExp
		subHandle = function(ch)
			if (char.isSpace ch)
				nextState Scanning
				state.handle ch
			elif (== ch ".")
				nextState Dot
			else
				let e = lastExp
				e.content = + (e.content) ch

	let Dot = inherit BasicState # Note: Do not ask to "handle" on switch when entering
		subHandle = function(ch)
			if (not (char.isNonLineSpace ch))
				if (char.isDigit ch)
					nextState Number
					lastExp.appendDot()
				else
					nextState Symbol
					lastExp.isAtom = true
				state.handle ch

	let Quote = inherit State
		field backslash = false

		enter = function(ch)
			appendExp
				new QuoteExp
		method handle = function(ch)
			let trueCh = ch

			if (this.backslash)
				trueCh = with ch match
					"\\" = "\\"
					"n" = "\n"
					"r" = "\r"
					"t" = "\t"
					_ = if (char.isQuote ch) (ch) else (null)
				this.backslash = false

				if (not trueCh)
					recoverableError
						nullJoin array
							"Unrecognized backslash sequence \\\""
							ch
							"\""
			elif (== ch "\\")
				this.backslash = true
				trueCh = null
			elif (char.isQuote ch)
				nextState Scanning
				trueCh = null
			
			if (trueCh)
				lastExp.content = + (lastExp.content) trueCh

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
		SymbolExp = and (not (node.isAtom)) (== (node.content) goal)
		_ = false

let stripLeftSymbol = function(list, goal)
	if (not (list.length))
		null
	else
		let left = list 0
		if (isSymbol(left, goal))
			popLeft list
			left
		else
			null

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

let getNextExp = function(parser, loc, isExp, symbol, ary)
	let failMsg = function(a) (parser.error(loc, nullJoin a))

	if (not (ary.length))
		failMsg array
			"Emptiness after \""
			symbol
			"\""
	elif (if isExp (not (is ExpGroup (ary 0))))
		failMsg array
			"Expected a (group) after \""
			symbol
			"\""
	else
		popLeft(ary)

# Macro classes

# FIXME: In which places is null allowed in place of array()? In which places *should* it be?

# Abstract macro: Matches on a single known symbol
let OneSymbolMacro = inherit Macro
	method matches = function(left, node, right)
		isSymbol(node, this.symbol)

# Abstract macro: Expects KNOWNSYMBOL (GROUP)
let SeqMacro = inherit OneSymbolMacro
	method apply = function(parser, left, node, right, _)
		let exp = getNextExp(parser, node.loc, true, this.symbol, right)

		if (is InvalidExec exp)
			exp
		else
			new ProcessResult(left, this.construct(parser, exp), right)

# a = b
let SetMacro = inherit OneSymbolMacro
	progress = + (ProgressBase.parser) 100
	symbol = "="

	method apply = function(parser, left, node, right, tracker) # DO I NEED TO COPY RIGHT?
		let exec = new SetExec(node.loc)

		let pending = true
		while (pending)
			if (stripLeftSymbol(left, "let"))
				exec.isLet = true
			elif (stripLeftSymbol(left, "field"))
				exec.isField = true
			elif (stripLeftSymbol(left, "method"))
				exec.isMethod = true
			else
				pending = false

		if (not (left.length))
			parser.error(node.loc, "Missing name in =")
		else
			let process = parser.process(node.loc)
			let index = left.pop
			let failedAt = null
			if (left.length)
				exec.targetClause = process(left, null) 
				exec.indexClause = process(array (index), null)
			else
				if (and (is SymbolExp index) (not (index.isAtom)))
					exec.indexClause = new AtomLiteralExec(index.loc, index.content)
				else
					failedAt = index.loc

			if (failedAt)
				parser.error(failedAt, "Assigned name must be alphanumeric")
			else
				exec.valueClause = process(right, tracker)

				new ProcessResult(null, exec, null)				

# do (statements)
let DoMacro = inherit SeqMacro
	progress = + (ProgressBase.parser) 400
	symbol = "do"

	method construct = function(parser, seq)
		parser.makeSequence(seq.loc, seq.statements, true)

# function(args) (body)
let FunctionMacro = inherit OneSymbolMacro
	progress = + (ProgressBase.parser) 400
	symbol = "function"

	method apply = function(parser, left, node, right, _)
		let getNextGroup = getNextExp(parser, node.loc, true)

		let argExp = getNextGroup(this.symbol, right)
		if (is InvalidExec argExp)
			argExp
		else
			let bodyExp = getNextGroup(+ (this.symbol) " (args)", right)
			if (is InvalidExec bodyExp)
				bodyExp
			else
				let args = array()
				let argError = null
				if (not (argExp.empty))
					let i = argExp.statements.iter
					while (and (not argError) (i.more))
						let stm = i.next
						let failBecause = function(reason)
							argError = parser.error
								node.loc
								nullJoin array
									"Arg #"
									args.length
									" on "
									this.symbol
									" is "
									reason

						if (not (stm.nodes.length))
							failBecause "blank"
						elif (!= (stm.nodes.length) 1)
							failBecause "an expression"
						elif (not (is SymbolExp (stm.nodes 0)))
							failBecause "not a symbol"
						else
							args.append(stm.nodes(0).content)

				if (argError)
					argError
				else
					new ProcessResult
						left
						new MakeFuncExec
							node.loc
							args
							parser.makeSequence(bodyExp.loc, bodyExp.statements, true)
						right

let IfMacro = inherit OneSymbolMacro
	field loop = false
	
	progress = + (ProgressBase.parser) 400

	method symbol = if (this.loop) ("while") else ("if")

	method apply = function(parser, left, node, right, tracker)
		let getNext = getNextExp(parser, node.loc)

		let condExp = getNext(false, this.symbol, right)
		if (is InvalidExec condExp)
			condExp
		else
			let seqExp = getNext(true, + (this.symbol) " (group)", right)

			if (is InvalidExec seqExp)
				seqExp
			else
				let condExec = parser.process(condExp, array(condExp), null)
				let seqExec = parser.makeSequence(seqExp.loc, seqExp.statements, not (this.loop))
				let elseExec = null

				if (not (this.loop))
					if (and (not (nonempty right)) tracker)
						right = tracker.steal "else"
					if (and (not (nonempty right)) tracker)
						right = tracker.steal "elif"
					if (nonempty right)
						if (isSymbol(right 0, "else"))
							popLeft(right)
							let elseExp = getNext(true, "else", right)
							if (elseExp)
								elseExec = parser.makeSequence(elseExp.loc, elseExp.statements, true)
						elif (isSymbol(right 0, "elif"))
							let elifSymbol = popLeft(right)
							let elseResult = this.apply(parser, array(), elifSymbol, right, tracker)
							if (is ProcessResult elseResult)
								elseExec = elseResult.at
								right = elseResult.right
							else
								elseExec = elseResult

				if (is InvalidExec elseExec)
					elseExec
				else
					new ProcessResult(left, new IfExec(node.loc, this.loop, condExec, seqExec, elseExec), right)

let ArrayMacro = inherit SeqMacro
	progress = + (ProgressBase.parser) 500
	symbol = "array"

	method construct = function(parser, seq)
		new MakeArrayExec(seq.loc, parser.makeArray(seq))

let ObjectMacro = inherit OneSymbolMacro
	field instance = false

	progress = + (ProgressBase.parser) 500
	
	method symbol = if (this.instance) ("new") else ("inherit")

	method apply = function(parser, left, node, right, _)
		let getNext = getNextExp(parser, node.loc)
		let baseExp = getNext(false, this.symbol, right)
		if (is InvalidExec baseExp)
			baseExp
		else
			let baseExec = parser.process(baseExp, array(baseExp), null)

			let seqExp = if (not (right.length))
				new ExpGroup(baseExp.loc)
			else
				getNext(true, + (this.symbol) " [base]", right)

			if (is InvalidExec seqExp)
				seqExp
			else
				let seq = if (not (seqExp.empty))
					parser.makeSequence(seqExp.loc, seqExp.statements, false).execs
				else
					array()

				let values = array()
				let assigns = array()
				let foundSet = false
				let foundError = null

				let i = seq.iter
				while (and (not foundError) (i.more))
					let assign = i.next

					if (is SetExec assign)
						foundSet = true
						if (assign.targetClause)
							foundError = parser.error(assign.loc, "Assignment inside object literal was not of form key=value")
						elif (assign.isLet)
							foundError = parser.error(assign.loc, "Found a stray value expression inside an object literal")
						else
							assign.isLet = true
							assigns.append assign
					else
						if foundSet
							foundError = parser.error(assign.loc, "Found a stray value expression inside an object literal")
						else
							values.append assign

				if (foundError)
					foundError
				else
					new ProcessResult(left, new MakeObjectExec(node.loc, baseExec, values, assigns, this.instance), right)

let ValueMacro = inherit Macro
	progress = + (ProgressBase.parser) 900

	method matches = function(left, node, right)
		with node match
			QuoteExp = true
			NumberExp = true
			SymbolExp = true
			_ = false

	method apply = function(parser, left, node, right, _)
		node = with node match
			QuoteExp(loc, content) = new StringLiteralExec(loc, content)
			NumberExp(loc, integer, dot, decimal) = do
				let value = integer
				if (dot)
					value = + value "."
				if (decimal)
					value = + value decimal
				new NumberLiteralExec(loc, value.toNumber)
			SymbolExp(loc, content, isAtom) = do
				if (isAtom)
					new AtomLiteralExec(loc, content)
				else
					new VarExec(loc, content)
			_ = parser.error(node.loc, "Internal error: AST node of indecipherable type found at end of macro processing")

		if (is InvalidExec node)
			node
		else
			new ProcessResult(left, node, right)

let standardMacros = array
	SetMacro
	DoMacro
	FunctionMacro
	new IfMacro( false )
	new IfMacro( true )
	ArrayMacro
	new ObjectMacro ( false )
	new ObjectMacro ( true )
	ValueMacro

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

		let i = execs.iter
		while (and (not hasLets) (i.more))
			let exe = i.next
			if ( if (is SetExec exe) (exe.isLet) ) # SHORT CIRCUITING "AND" NEEDED BADLY
				hasLets = true

		new SequenceExec(loc, shouldReturn, hasLets, execs)

	method makeArray = function(expGroup)
		let result = array()
		if (not (expGroup.empty))
			let tracker = new SequenceTracker(expGroup.statements)
			while (tracker.more)
				let statement = tracker.next
				let exe = this.process(expGroup.loc, statement.nodes, tracker)
				result.append exe
		result

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
			this.error(loc, "Internal error: Parser attempted to evaluate an empty statement. This is a bug in the interpreter.")
		else
			# First, apply all macros to the statement.
			let macroNode = this.macros
			let foundError = null
			while (and macroNode (not foundError))
				let m = macroNode.value
				let left = array()
				let right = nodes

				# One by one move the items out of right into left and macro-filter along the way
				
				while (
						and
							if (right) (right.length)
							not foundError
					)
					let at = popLeft right

					# FIXME: Need to bring in "macro levels" concept from parser.py
					# So that same-priority keys get processed in a single sweep
					if (
							and 
								<= (at.progress) (m.progress)
								m.matches(left, at, right)
						)
						# apply() returns either a ProcessResult so processing can continue,
						# or a bare InvalidExec to signal that processing should short-circuit.
						let result = m.apply(this, left, at, right, tracker)

						# Unpack
						with result match
							InvalidExec =
								foundError = result
							ProcessResult(_left, _at, _right) = do
								left = _left
								at = _at
								right = _right

						if (not left)
							left = array()
						if (at)
							left.append at
					else
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
					if (firstNode.empty)                   # ()
						result = new Unit (firstNode.loc)
					elif (== (firstNode.statements.length) 1) # (arg)
						result = this.process (firstNode.loc, firstNode.statements(0).nodes, null)
					else                                   # (arg1, arg2, ...)
						resultError = this.error (firstNode.loc, "Line started with a multiline parenthesis group. Did you mean to use \"do\"?")
		
				else
					result = popLeft(nodes)
					resultError = this.checkComplete result # First write so this is safe

				# For each item in the list, apply result = result(b)
				while (and (not resultError) (nodes.length))
					let arg = popLeft(nodes)

					if (is ExpGroup arg)
						if (arg.empty) # ()
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
										nullJoin array
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
	parser.loadAll standardMacros
	let result = parser.makeSequence(ast.loc, ast.statements, false)

	checkErrors(parser.errors)

	result

# --- Execution ---

# Execution tree

let Executable = inherit Node
	progress = ProgressBase.executable

	method fail = function (msg)
		fail
			nullJoin array
				"Execution failed at "
				this.loc
				":\n\t"
				msg

let InvalidExec = inherit Executable
	toString = "[Invalid node]"

	eval = function (scope)
		this.fail "Tried to execute invalid program"

let SequenceExec = inherit Executable
	field shouldReturn = false
	field hasScope = false
	field method execs = array()

	method eval = function (scope)
		if (this.hasScope)
			scope = new ObjectValue(scope)
		let i = this.execs.iter
		let result = null
		while (i.more)
			result = i.next.eval(scope)
		if (this.shouldReturn)
			result
		else
			null

	method toString = do
		let tags = array()
		if (this.hasScope)
			tags.append "Scoped"
		if (this.shouldReturn)
			tags.append "Returning"
		nullJoin array
			"[Sequence"
			if (tags.length)
				nullJoin array("(", join "," tags, ")")
			else
				""
			" "
			join " " (this.execs)
			"]"

let UserMacroList = inherit Executable
	field contents = null

	toString = "[Misplaced macro node]"

let LiteralExec = inherit Executable
	field value = null

let StringLiteralExec = inherit LiteralExec
	method toString = nullJoin array("[StringLiteral ", quotedString(this.value), "]")

	method eval = function (scope)
		new StringValue(this.value)

let NumberLiteralExec = inherit LiteralExec
	method toString = nullJoin array("[NumberLiteral ", this.value, "]")

	method eval = function (scope)
		new NumberValue(this.value)

let AtomLiteralExec = inherit LiteralExec
	method toString = nullJoin array("[AtomLiteral ", this.value, "]")

	method eval = function (scope)
		this

# Does not inherit LiteralExec because it holds no value
let NullLiteralExec = inherit Executable
	toString = "[NullLiteral]"

	method eval = function (scope)
		NullValue

let VarExec = inherit Executable
	field symbol = null

	method toString = nullJoin array("[Var ", this.symbol, "]")

	method eval = function (scope)
		scope.lookup (this.symbol)

let ApplyExec = inherit Executable
	field fn = null
	field arg = null

	method toString = nullJoin array
		"[Apply "
		this.fn
		" "
		this.arg
		"]"

	method eval = function (scope)
		this.fn.eval(scope).apply (this.arg.eval(scope))

let SetExec = inherit Executable
	field isLet = false
	field isMethod = false
	field isField = false
	field targetClause = null
	field indexClause = null
	field valueClause = null

	method toString = nullJoin array
		"["
		if (this.isLet) ("Let") else ("Set")
		" "
		if (this.targetClause) (this.targetClause) else ("Scope")
		" "
		this.indexClause
		" "
		this.valueClause
		"]"

	method setEval = function (scope, target, index)
		let value = if (this.isMethod)
			fail "Not implemented yet"
		else
			this.valueClause.eval scope

		target.assign(this.isLet, index, value)

	method eval = function (scope)
		this.setEval
			scope
			if (this.targetClause)
				this.targetClause.eval scope
			else
				scope
			this.indexClause.eval scope

let MakeFuncExec = inherit Executable
	field args = null
	field body = null

	method toString = nullJoin array
		"[Function ["
		join ", " (this.args)
		"] "
		this.body
		"]"

	method eval = function(scope)
		new FunctionValue(this.args, this.body, scope)

let MakeObjectExec = inherit Executable
	field baseClause = null
	field method values = array()
	field method assigns = array()
	field isInstance = false

	method toString = nullJoin array
		"["
		if (this.isInstance) ("New") else ("Inherit")
		this.baseClause
		"["
		join ", " (this.values)
		"]]"

	method eval = function(scope)
		let base = this.baseClause.eval(scope)
		if (== base rootObject) # Tiny optimization: Don't actually inherit from Object
			base = null
		let infields = if (base) (base.fields) else (null)
		let result = new ObjectValue(base)
		if (and (this.isInstance) infields) # FIXME: This calls method fields even when not needed
			let i = infields.iter
			while (i.more)
				let f = i.next
				result.assign(true, f, base.apply(f))

		if (
				>
					if (this.values) (this.values.length) else (0) # FIXME: Wait, when will this ever happen?
					if (infields) (infields.length) else (0)
			)
				fail "Tried to specify more values in \"new\" than this object has fields"

		let valueProgress = 0
		let i = this.values.iter
		while (i.more)
			let value = i.next.eval(scope)
			result.atoms.set (infields(valueProgress).value) value
			valueProgress = + valueProgress 1

		let i = this.assigns.iter
		while (i.more)
			let exe = i.next
			let index = exe.indexClause.eval(scope) # do this early for field handling
			if (exe.isField)
				if (not (is AtomLiteralExec index))
					fail "Objects have atom keys only"
				if (not (result.fields)) # FIXME: Again, when does this happen?
					result.fields = if infields (infields) else (array())
				result.fields.append(index)
			exe.setEval(scope, result, index)

		if (not (result.fields))
			result.fields = infields

		result


let MakeArrayExec = inherit Executable
	field contents = null

	method toString = nullJoin array
		"[Array "
		join ", " (this.contents)
		"]"

	method eval = function(scope)
		let values = array()
		let i = this.contents.iter
		while (i.more)
			values.append (i.next.eval(scope))
		new ArrayValue(values)

let IfExec = inherit Executable
	field loop = false
	field condClause = null
	field ifClause = null
	field elseClause = null

	method toString = nullJoin array
		"["
		if (this.loop) ("While") else ("If")
		" "
		this.condClause
		" "
		this.ifClause
		if (this.elseClause)
			+ " " (this.elseClause.toString)
		else
			""
		"]"

	method eval = function(scope)
		if (not (this.loop))
			if (isTrue(this.condClause.eval(scope)))
				this.ifClause.eval(scope)
			elif (this.elseClause)
				this.elseClause.eval(scope)
		else
			while (isTrue(this.condClause.eval(scope)))
				this.ifClause.eval(scope)
			null

let Unit = NullLiteralExec # Just an alias

# Values

# Util function
let isTrue = match
	NullValue = false
	NumberValue v = (!= v 0)
	_ = true

# Util function
let copyArgsWithAppend = function (ary, value)
	if (ary)
		let result = array()
		let i = ary.iter
		while (i.more)
			result.append(i.next)
		result.append value
		result
	else
		array(value)

let Value = inherit object
	apply = function(value)
		fail "Apply for this object unimplemented"

let ObjectValue = inherit Value
	field parent = null
	field fields = null
	field method atoms = new Dict

	# "True" lookup function: Doesn't think about methods, keys are strings
	method innerLookup = function(key)
		if (this.atoms.has key)
			this.atoms.get key
		elif (this.parent)
			this.parent.innerLookup key
		else
			fail
				nullJoin array
					"Key not found: "
					key

	# "External" lookup function: Keys are known strings
	method lookup = function (key)
		this.innerLookup key # TODO: Method filter

	# "True" assign function: keys are strings, key must exist
	method innerAssign = function (key, value)
		if (this.atoms.has key)
			this.atoms.set key value
		elif (this.parent)
			this.parent.innerAssign key value
		else
			fail (+ "Tried to assign nonexistent key " key)

	# "External" assign function: key has no known type
	method assign = function (isLet, index, value)
		# TODO: Sanitize for atom here
		let key = index.value
		if (isLet)
			this.atoms.set key value
		else
			this.innerAssign(key, value)

	method apply = function(value)
		with value match
			AtomLiteralExec = this.lookup (value.value)
			_ = fail "Object has atom keys only"

let FunctionValue = inherit Value
	field argNames = null
	field exe = null
	field scope = null
	field args = null

	method apply = function(value)
		if (not (this.argNames.length))
			this.exe.eval(this.scope)
		else
			let newArgs = copyArgsWithAppend(this.args, value)
			if (>= (newArgs.length) (this.argNames.length))
				let scope = new ObjectValue(this.scope)
				let idx = 0
				while (< idx (this.argNames.length))
					scope.atoms.set(this.argNames idx, newArgs idx)
					idx = + idx 1
				this.exe.eval(scope)
			else
				new FunctionValue(this.argNames, this.exe, this.scope, newArgs)

let ArrayValue = inherit Value
	field method values = array()

	method apply = function(value)
		with value match
			NumberValue number = this.values number
			AtomLiteralExec(_, key) = arrayPrototype.lookup key
			_ = fail "Only number or atom keys allowed on array"

	method assign = function(_, key, value)
		with key match
			NumberValue number = (this.values number = value)
			_ = fail "Tried to write non-number index on array"

let NullValue = inherit Value

let LiteralValue = inherit Value
	field value = null

let LiteralFunctionValue = inherit LiteralValue
	field count = 0
	method apply = function(value)
		let result = this.value value # Just killed tail recursion
		if (< (this.count) 2)
			result
		else
			new LiteralFunctionValue(result, - (this.count) 1)

let StringValue = inherit LiteralValue

let NumberValue = inherit LiteralValue

let TrueValue = new NumberValue(1)

# Stdlib: Array

let arrayPrototype = new ObjectValue

# Stdlib: Scope

let wrapBinaryNumber = function(f)
	new LiteralFunctionValue
		function(x,y)
			new NumberValue(f (x.value) (y.value))
		2

let wrapBinaryBool = function(f)
	new LiteralFunctionValue
		function(x,y)
			if (f (x.value) (y.value))
				TrueValue
			else
				NullValue
		2

let wrapPrintRepeat = function(f)
	let repeat = new LiteralFunctionValue
		function (x)
			f
				with x match
					StringValue v = v
					NumberValue v = v
					NullValue = "null"
					_ = "[Unprintable]"
			repeat
	repeat

let rootObject = new ObjectValue

let defaultScope = new ObjectValue

defaultScope.atoms.set "+" (wrapBinaryNumber +)
defaultScope.atoms.set "-" (wrapBinaryNumber -)
defaultScope.atoms.set "*" (wrapBinaryNumber *)
defaultScope.atoms.set "/" (wrapBinaryNumber /)

defaultScope.atoms.set "<"  (wrapBinaryBool <)
defaultScope.atoms.set "<=" (wrapBinaryBool <=)
defaultScope.atoms.set ">"  (wrapBinaryBool >)
defaultScope.atoms.set ">=" (wrapBinaryBool >=)
defaultScope.atoms.set "==" (wrapBinaryBool ==)
defaultScope.atoms.set "!=" (wrapBinaryBool !=)

defaultScope.atoms.set "print"   (wrapPrintRepeat print)
defaultScope.atoms.set "println" (wrapPrintRepeat println)

defaultScope.atoms.set "object" rootObject
defaultScope.atoms.set "null"   NullValue

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
		let scope = new ObjectValue(defaultScope)
		scope.atoms.set "argv" (new ArrayValue(scriptArgv))
		
		exe.eval(scope)
