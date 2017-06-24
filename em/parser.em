# Parser: Transformations ("macros") applied to parse tree to gradually make it executable

profile experimental

from project.util import *
from project.core import *
from project.reader import
	SymbolExp, QuoteExp, NumberExp, ExpGroup
from project.execution import
	UnitExec, StringLiteralExec, AtomLiteralExec, NumberLiteralExec
	InvalidExec, VarExec, ApplyExec, SetExec, IfExec, SequenceExec, ImportAllExec
	MakeFuncExec, MakeMatchExec, MakeArrayExec, MakeObjectExec
	ObjectValue, UserMacroList, macroExportList, profileScope, defaultScope

# Base/helper

export Macro = inherit Object
	progress = ProgressBase.reader

export insertMacro = insertLinked function(x,y)
	cmp (x.progress) (y.progress)

export isSymbol = function(node, goal)
	with node match
		SymbolExp = !node.isAtom && !node.isEscaped && node.content == goal
		_ = false

export stripLeftSymbol = function(list, goal)
	if (not (list.length))
		null
	else
		let left = list 0
		if (isSymbol(left, goal))
			popLeft list
			left
		else
			null

export SequenceTracker = inherit Object
	field statements = null
	field idx = 0

	method more = this.statements.length > this.idx
	method next = do
		let result = this.statements (this.idx)
		this.idx = this.idx + 1
		result

	method steal = function(symbol)
		if (this.more)
			let nodes = this.statements(this.idx).nodes
			if (nodes && isSymbol (nodes 0) symbol)
				this.next
				nodes
			else
				null
		else
			null

export getNextExp = function(parser, loc, isExp, symbol, ary)
	let failMsg = function(a) (parser.error(loc, nullJoin a))

	if (!ary.length)
		failMsg array
			"Emptiness after \""
			symbol
			"\""
	elif (isExp && !is ExpGroup (ary 0))
		failMsg array
			"Expected a (group) after \""
			symbol
			"\""
	else
		popLeft ary

# Macro classes

# FIXME: In which places is null allowed in place of array()? In which places *should* it be?

# Abstract macro: Matches on a single known symbol
export OneSymbolMacro = inherit Macro
	method matches = function(left, node, right)
		isSymbol(node, this.symbol)

# User requesting macro load
export MacroMacro = inherit OneSymbolMacro
	field \profile = false
	progress = ProgressBase.parser + 10

	method symbol = if (this.profile) ("profile") else ("macro")

	method apply = function(parser, left, node, right, _)
		let isExport = false
		let error = null

		if (left.length)
			if (!(left.length == 1) && isSymbol(left 0, "export"))
				error = parser.error(node.loc, "Stray garbage before \"" + this.symbol + "\"")
			isExport = true

		if (!right.length)
			error = parser.error(node.loc, "Emptiness after \"" + this.symbol + "\"")

		if error
			error
		else
			let macroGroup = right 0
			let result = null

			if (is SymbolExp macroGroup)
				# TODO: Consider only allowing this if atom keys. TODO: Do something more sensible when this fails?
				let macroObject = parser.process(node.loc, right, null).eval(profileScope)
				# TODO: This can fail
				let macroList = macroObject.apply(new AtomLiteralExec(node.loc, macroExportList))

				if (!is Array macroList)
					error = perser.error(macroGroup.loc, u"macro import path did not resolve to a valid module")
				else
					let payload = None
					let importObject = macroObject

					if (is LazyMacroLoader importObject)
						importObject = importObject.importObject

					if importObject
						payload = new ImportAllExec
							node.loc
							new StoredLiteralExec(node.loc, importObject)
							isExport

					result = new ProcessResult
							null
							new UserMacroList(node.loc, macroList, isExport, this.profile, payload)
							null

			elif (is ExpGroup macroGroup)
				if (right.length > 1)
					error = parser.error(node.loc, "Stray garbage after \"" + this.symbol + " (group)\"")
				let macroExecs = parser.makeArray(macroGroup)
				let macroValues = array()
				let i = macroExecs.iter
				while (i.more)
					macroValues.append
						i.next.eval(defaultScope)
				result = new ProcessResult
					null
					new UserMacroList(node.loc, macroValues, isExport, this.profile)
					null

			else
				error = parser.error(node.loc, u"Expected a path or a (group) after \"" + this.symbol + "\"")

			if error (error) else (result)

# Abstract macro: Expects KNOWNSYMBOL (GROUP)
export SeqMacro = inherit OneSymbolMacro
	method apply = function(parser, left, node, right, _)
		let exp = getNextExp(parser, node.loc, true, this.symbol, right)

		if (is InvalidExec exp)
			exp
		else
			new ProcessResult(left, this.construct(parser, exp), right)

# from a import b
export ImportMacro = inherit OneSymbolMacro
	progress = ProgressBase.parser + 10
	symbol = "import"

	method generateSetExec = function(parser, loc, prefix, target)
		if (!target.length)
			parser.error(loc, "Missing target to import")
		else
			let error = null

			if (prefix)
				let targetStart = target 0
				if (is SymbolExp targetStart)
					if (targetStart.isAtom)
						error = parser.error(targetStart.loc, "Expected a symbol after \"import\"")
					else
						target = cloneArray target
						target 0 = new SymbolExp(loc, targetStart.content, true)
				if (not error)
					target = catArray prefix target

			if (!error && target.length == 1)
				error = parser.error(target(0).loc, "import expects either multiple symbols or a \"from\" clause")

			if (not error)
				let targetEnd = lastFrom target
				if (is SymbolExp targetEnd && !targetEnd.isAtom)
					error = parser.error(targetEnd.loc, "End of import path needs to be an atom")

			if (error)
				error
			else
				let targetEnd = lastFrom target
				let symbol = new AtomLiteralExec(targetEnd.loc, targetEnd.content)
				
				new SetExec(loc, true, false, false, false, null, symbol, parser.process(loc, target, null))

	method apply = function(parser, left, node, right, tracker)
		let prefix = null
		let error = null

		if (left.length)
			if (isSymbol(left 0, "from"))
				if (left.length == 1)
					error = parser.error(left(0).loc, "Expected symbols between \"from\" and \"import\"")
				else
					prefix = cloneArray left
					popLeft prefix
			else
				error = parser.error(node.loc, "Stray garbage before \"import\"")

		if (error)
			error
		else
			let result = null

			if (right.length == 1 && is ExpGroup (right 0))
				let setExecs = array()
				let i = right(0).statements.iter
				while (!result && i.more)
					let setExec = this.generateSetExec(parser, node.loc, prefix, i.next.nodes)
					if (is Error setExec)
						result = setExec
					else
						setExecs.append setExec
				if (!result)
					result = new SequenceExec(node.loc, false, false, setExecs)
			elif (right.length == 1 && isSymbol (right 0) "*")
				result = new ImportAllExec(node.loc, parser.process(node.loc, prefix, null))
			else
				result = this.generateSetExec(parser, node.loc, prefix, right)

			if (is Error result)
				result
			else
				new ProcessResult(null, result, null)

# a = b
export SetMacro = inherit OneSymbolMacro
	progress = ProgressBase.parser + 100
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
			elif (stripLeftSymbol(left, "export"))
				exec.isExport = true
			else
				pending = false

		if (exec.isLet && exec.isExport)
			parser.error(node.loc, "Cannot use \"let\" and \"export\" together")
		if (!left.length)
			parser.error(node.loc, "Missing name in =")
		else
			let process = parser.process(node.loc)
			let index = left.pop
			let failedAt = null
			if (left.length)
				exec.targetClause = process(left, null) 
				exec.indexClause = process(array (index), null)
			else
				if (is SymbolExp index && !index.isAtom)
					exec.indexClause = new AtomLiteralExec(index.loc, index.content)
				else
					failedAt = index.loc

			if (failedAt)
				parser.error(failedAt, "Assigned name must be alphanumeric")
			else
				exec.valueClause = process(right, tracker)

				new ProcessResult(null, exec, null)				

# do (statements)
export DoMacro = inherit SeqMacro
	progress = ProgressBase.parser + 400
	symbol = "do"

	method construct = function(parser, seq)
		parser.makeSequence(seq.loc, seq.statements, true)

# function(args) (body)
export FunctionMacro = inherit OneSymbolMacro
	progress = ProgressBase.parser + 400
	symbol = "function"

	method apply = function(parser, left, node, right, _)
		let getNextGroup = getNextExp(parser, node.loc, true)

		let argExp = getNextGroup(this.symbol, right)
		if (is InvalidExec argExp)
			argExp
		else
			let bodyExp = getNextGroup(this.symbol + " (args)", right)
			if (is InvalidExec bodyExp)
				bodyExp
			else
				let args = array()
				let argError = null
				if (!argExp.empty)
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

						if (!stm.nodes.length)
							failBecause "blank"
						elif (stm.nodes.length !=  1)
							failBecause "an expression"
						elif (!is SymbolExp (stm.nodes 0))
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

export IfMacro = inherit OneSymbolMacro
	field loop = false
	
	progress = ProgressBase.parser + 400

	method symbol = if (this.loop) ("while") else ("if")

	method apply = function(parser, left, node, right, tracker)
		let getNext = getNextExp(parser, node.loc)

		let condExp = getNext(false, this.symbol, right)
		if (is InvalidExec condExp)
			condExp
		else
			let seqExp = getNext(true, this.symbol + " (group)", right)

			if (is InvalidExec seqExp)
				seqExp
			else
				let condExec = parser.process(condExp.loc, array(condExp), null)
				let seqExec = parser.makeSequence(seqExp.loc, seqExp.statements, not (this.loop))
				let elseExec = null

				if (not (this.loop))
					if (!nonempty right && tracker)
						right = tracker.steal "else"
					if (!nonempty right && tracker)
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

export MatchCase = inherit Object
	field targetExe = null
	field unpacks = null
	field statement = null

export MatchMacro = inherit OneSymbolMacro
	progress = ProgressBase.parser + 400
	symbol = "match"

	method apply = function (parser, left, node, right, tracker)
		let exp = getNextExp(parser, node.loc, true, this.symbol, right)

		if (is InvalidExec exp)
			exp
		else
			let result = array()
			let iStm = exp.statements.iter
			let foundError = null

			while (iStm.more)
				let stm = iStm.next
				if (stm.nodes.length)
					let eqNode = null
					let eqLeft = array()
					let eqRight = array()

					let iNode = stm.nodes.iter
					while (iNode.more)
						let node = iNode.next
						if (eqNode)
							eqRight.append node
						elif (isSymbol node "=")
							eqNode = node
						else
							eqLeft.append node

					if (not eqNode)
						foundError = parser.error(stm.nodes(0).loc, "Match line does not have an =")
					elif (not (eqLeft.length))
						foundError = parser.error(eqNode.loc, "Left of = in match line is blank")
					elif (eqLeft.length > 2)
						foundError = parser.error(eqLeft(2).loc, "Left of = in match line has too many symbols. Try adding parenthesis?")
					elif (!eqRight.length)
						foundError = parser.error(eqNode.loc, "Right of = in match line is blank")
					else
						let targetExp = popLeft eqLeft
						let unpacksExp = null
						let unpacks = array()

						if (eqLeft.length) # There is an unpack list
							let garbled = false

							unpacksExp = eqLeft 0
							if (is SymbolExp unpacksExp)
								unpacks.append
									new AtomLiteralExec(unpacksExp.loc, unpacksExp.content)
							elif (is ExpGroup unpacksExp)
								let iUnpack = unpacksExp.statements.iter
								while (!garbled && iUnpack.more)
									let unpackStatement = iUnpack.next
									if (!unpackStatement.nodes.length)
										garbled = true
									else
										let unpackSymbol = unpackStatement.nodes 0
										if (!is SymbolExp unpackSymbol)
											garbled = true
										else
											unpacks.append(new AtomLiteralExec(unpackSymbol.loc, unpackSymbol.content))
							else
								garbled = true # Technically redundant

							if (garbled || !unpacks.length)
								foundError = parser.error(unpacksExp.loc, "In match line, variable unpack list on left of = is garbled")

						if (!foundError)
							if (isSymbol targetExp "_")
								if (unpacksExp)
									foundError = parser.error(unpacksExp.loc, "In match line, variable unpack list used with _")
								else
									targetExp = null # Null denotes wildcard match
							elif (isSymbol targetExp "array")
								if (!unpacksExp)
									foundError = parser.error(unpacksExp.loc, "In match line, variable unpack list missing after \"array\"")
								else
									targetExp = null

						if (!foundError)
							let targetExec = 
								targetExp && parser.process (targetExp.loc, array (targetExp), null)
							let tempStatement = parser.process(eqRight(0).loc, eqRight, tracker)
							result.append(new MatchCase(targetExec, unpacks, tempStatement))

			if (foundError)
				foundError
			else
				new ProcessResult (left, new MakeMatchExec(node.loc, result), right)

export ArrayMacro = inherit SeqMacro
	progress = ProgressBase.parser + 500
	symbol = "array"

	method construct = function(parser, seq)
		new MakeArrayExec(seq.loc, parser.makeArray(seq))

export ObjectMacro = inherit OneSymbolMacro
	field instance = false

	progress = ProgressBase.parser + 500
	
	method symbol = if (this.instance) ("new") else ("inherit")

	method apply = function(parser, left, node, right, _)
		let getNext = getNextExp(parser, node.loc)
		let baseExp = getNext(false, this.symbol, right)
		if (is InvalidExec baseExp)
			baseExp
		else
			let baseExec = parser.process(baseExp, array(baseExp), null)

			let seqExp = if (!right.length)
				new ExpGroup(baseExp.loc)
			else
				getNext(true, this.symbol + " [base]", right)

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
				while (!foundError && i.more)
					let assign = i.next

					if (is SetExec assign)
						foundSet = true
						if (assign.targetClause)
							foundError = parser.error(assign.loc, "Assignment inside object literal was not of form key=value")
						elif (assign.isLet || assign.isExport)
							foundError = parser.error(assign.loc, "Found a stray value expression inside an object literal")
						else
							assign.isLet = true
							assigns.append assign
					elif (is ImportAllExec assign)
						foundSet = true
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

export ValueMacro = inherit Macro
	progress = ProgressBase.parser + 900

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
					value = value + "."
				if (decimal)
					value = value + decimal
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

export standardMacros = array
	new MacroMacro (false)
	new MacroMacro (true)
	ImportMacro
	SetMacro
	DoMacro
	FunctionMacro
	new IfMacro( false )
	new IfMacro( true )
	MatchMacro
	ArrayMacro
	new ObjectMacro ( false )
	new ObjectMacro ( true )
	ValueMacro

# Parser

export ProcessResult = inherit Object
	field left = null
	field at = null
	field right = null

export Parser = inherit Object
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
				if (!customMacros)   # TODO: Only dupe after descending levels
					parser = cloneParser(parser)
					customMacros = true
				m.loadAll(exe.contents)
			else
				execs.append(exe)

		let i = execs.iter
		while (!hasLets && i.more)
			let exe = i.next
			if ( is SetExec exe && (exe.isLet || exe.isExport) )
				hasLets = true

		new SequenceExec(loc, shouldReturn, hasLets, execs)

	method makeArray = function(expGroup)
		let result = array()
		if (!expGroup.empty)
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
		if (node.progress < ProgressBase.executable)
			this.error(node.loc, "Macro malfunctioned and left an unfinished node here at end of processing")
		else
			null

	method process = function(loc, nodes, tracker)
		# Callers must define the semantics of empty lists themselves.
		if (nodes.length == 0)
			this.error(loc, "Internal error: Parser attempted to evaluate an empty statement. This is a bug in the interpreter.")
		else
			# First, apply all macros to the statement.
			let macroNode = this.macros
			let foundError = null
			while (macroNode && !foundError)
				let m = macroNode.value
				let left = array()
				let right = nodes

				# One by one move the items out of right into left and macro-filter along the way
				
				while (
						(right && right.length) && !foundError # FIXME: shouldn't be needed with right associativity
					)
					let at = popLeft right

					# FIXME: Need to bring in "macro levels" concept from parser.py
					# So that same-priority keys get processed in a single sweep
					if (at.progress <= m.progress && m.matches(left, at, right))
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

						if (!left)
							left = array()
						if (at)
							left.append at
					else
						left.append at

					nodes = left

				macroNode = macroNode.next

			if (foundError)
				foundError
			elif (!nodes.length)
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
						result = new UnitExec (firstNode.loc)
					elif (firstNode.statements.length == 1) # (arg)
						result = this.process (firstNode.loc, firstNode.statements(0).nodes, null)
					else                                   # (arg1, arg2, ...)
						tracker = new SequenceTracker(firstNode.statements)
						result = this.process (firstNode.loc, tracker.next.nodes, tracker)
						if (tracker.more)
							resultError = this.error (firstNode.loc, "Line started with a multiline parenthesis group. Did you mean to use \"do\"?")
		
				else
					result = popLeft(nodes)
					resultError = this.checkComplete result # First write so this is safe

				# For each item in the list, apply result = result(b)
				while (!resultError && nodes.length)
					let arg = popLeft(nodes)

					if (is ExpGroup arg)
						if (arg.empty) # ()
							result = new ApplyExec(arg.loc, result, new UnitExec(arg.loc))
						else                    # (arg1, arg2, ...)
							let argIdx = 0 # Used by error message
							let tracker = new SequenceTracker(arg.statements)
							while (tracker.more)
								let statement = tracker.next
								argIdx = argIdx + 1
								if (!statement.nodes.length)
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
						if (!resultError)
							result = new ApplyExec(arg.loc, result, arg)

				if resultError
					resultError
				else
					result

export cloneParser = function(parser)
	new Parser(cloneLinked (parser.macros), parser.errors)

export exeFromAst = function(ast)
	let parser = new Parser
	parser.loadAll standardMacros
	let result = parser.makeSequence(ast.loc, ast.statements, false)

	checkErrors(parser.errors)

	result
