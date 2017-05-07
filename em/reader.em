# Reader: Turns unicode string iterator into parse tree

from project.util import *
from project.core import *

export ExpGroup = inherit Node
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

export StringContentExp = inherit Node
	field content = ""

export SymbolExp = inherit StringContentExp
	field isAtom = false

	method toString = +
		if (this.isAtom) (".") else ("")
		this.content

export QuoteExp = inherit StringContentExp
	method toString = quotedString (this.content)

export NumberExp = inherit Node
	field integer = ""
	field dot = null
	field decimal = null

	method appendDot = function()
		this.dot = true
		this.decimal = ""

	method toString = nullJoin array
		"#"
		this.integer
		if (this.dot) (".") else ("")
		if (this.decimal) (this.decimal) else ("")

export Statement = inherit Object
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

export StatementKind = inherit Object
let StatementKind.Outermost = inherit StatementKind
let StatementKind.Indent = inherit StatementKind
let StatementKind.Parenthesis = inherit StatementKind

export makeAst = function(i)
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

	let State = inherit Object
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
				new NumberExp(loc)
		subHandle = function(ch)
			let e = lastExp
			if (char.isSpace ch)
				nextState Scanning
				state.handle ch
			elif (char.isDigit ch)
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
				new SymbolExp(loc)
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
				new QuoteExp(loc)
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
