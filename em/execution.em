# Execution tree classes, code for values and default scope

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

	method evalSequence = function (scope, exportScope)
		let exportList = null
		if (this.hasScope)
			scope = new ObjectValue(scope)
			if exportScope
				exportList = array()
				scope.atoms.set scopeExportList exportList
		let i = this.execs.iter
		let result = null
		while (i.more)
			result = i.next.eval(scope)
		if (!= exportList null)
			let i = exportList.iter
			while (i.more)
				let key = i.next
				exportScope.atoms.set key (scope.atoms.get key)
		if (this.shouldReturn)
			result
		else
			NullValue

	method eval = function (scope)
		this.evalSequence scope null

	method toString = do
		let tags = array()
		if (this.hasScope)
			tags.append "Scoped"
		if (this.shouldReturn)
			tags.append "Returning"
		nullJoin array
			"[Sequence"
			if (tags.length)
				nullJoin array("(", join ", " tags, ")")
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

	# For questionable reasons, AtomLiteralExec (and no other Executable) doubles as its own value
	method apply = makePrototypeApply(atomValuePrototype, this)

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
	field isExport = false
	field targetClause = null
	field indexClause = null
	field valueClause = null

	method toString = nullJoin array
		"["
		if (this.isExport) ("Export") elif (this.isLet) ("Let") else ("Set")
		" "
		if (this.targetClause) (this.targetClause) else ("Scope")
		" "
		this.indexClause
		" "
		this.valueClause
		"]"


	method setEval = function (scope, target, index)
		let value = if (this.isMethod)
			new FunctionMethodPseudoValue(scope, target, this.valueClause)
		else
			this.valueClause.eval scope

		target.assign(or (this.isLet) (this.isExport), index, value)

	method eval = function (scope)
		let target = do
			if (this.targetClause)
				this.targetClause.eval scope
			else
				scope
		let index = this.indexClause.eval scope
		
		this.setEval(scope, target, index)			
		
		if (this.isExport)
			if (not (scope.atoms.has scopeExportList))
				this.fail "\"export\" in unexpected place"
			else
				let exportList = scope.atoms.get scopeExportList
				exportList.append (index.value) # Just assume it's an atom

		NullValue

let ImportAllExec = inherit Executable
	field sourceClause = null

	method toString = nullJoin array
		"[ImportAll "
		this.sourceClause
		"]"

	method setEval = function (scope, targetOverride, _)
		let source = this.sourceClause.eval(scope)

		if (not (is ObjectValue source))
			this.fail "Attempted to import * from something other than an object"

		let target = null
		if (targetOverride)
			target = targetOverride
		else
			target = scope

		let i = source.atoms.iter
		while (i.more)
			let key = i.next
			let value = source.lookup(key)
			target.atoms.set key value # FIXME: Should this be done via a method on target?

		NullValue

	method eval = function (scope)
		this.setEval(scope, null, null)

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
			let index = null
			if (is SetExec exe)
				index = exe.indexClause.eval(scope) # do this early for field handling
				if (exe.isField)
					if (not (is AtomLiteralExec index))
						this.fail "Objects have atom keys only"
					if (not (result.fields))
						result.fields = copyArgsWithAppend(infields, index)
					else
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

let MakeMatchExec = inherit Executable
	field matches = null

	method toString = do
		let result = "[Match"
		let i = s.matches.iter
		while (i.more)
			let m = i.next
			result = + result 
				nullJoin array
					" [Case "
					m.targetExe
					" ["
					join  ", " (m.unpacks)
					"] "
					m.statement.toString
					"]"
		result = + result ("]")

	method eval = function(scope)
		new MatchFunctionValue(this.matches, scope)

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
				NullValue
		else
			while (isTrue(this.condClause.eval(scope)))
				this.ifClause.eval(scope)
			NullValue

let UnitExec = NullLiteralExec # Just an alias

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

# Util function
let toBoolValue = function(x)
	if (x)
		TrueValue
	else
		NullValue

# Util function
let isChild = function(parent,child)
	if (== parent child)
		true
	elif (is NumberValue child)
		== parent numberValuePrototype
	elif (is StringValue child)
		== parent stringValuePrototype
	elif (is ArrayValue child)
		== parent arrayValuePrototype
	elif (is ObjectValue child)
		if (== parent rootObject)
			true
		else
			let result = false
			while (and (not result) (child.parent))
				child = child.parent
				if (== parent child)
					result = true
			result
	else
		false

# Method constructor function
let makePrototypeApply = function(prototype, this, value)
	with value match
		AtomLiteralExec(_, key) = resolveMethod(prototype, key, this)
		_ = fail "Object has atom keys only"

let Value = inherit Object
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
		resolveMethod this key this

	# "True" assign function: keys are strings, key must exist
	method innerAssign = function (key, value)
		if (this.atoms.has key)
			this.atoms.set key value
		elif (this.parent)
			this.parent.innerAssign key value
		else
			fail (+ "Tried to assign nonexistent key " key)

	method key = function (value)
		with value match
			AtomLiteralExec(_, key) = key
			NumberValue idx = (this.fields idx).value # FIXME: Wait why are atoms stored in here?
			_ = fail "Object has atom or number keys only"

	# "External" assign function: key has no known type
	method assign = function (isLet, index, value)
		# TODO: Sanitize for atom here
		let key = this.key index
		if (isLet)
			this.atoms.set key value
		else
			this.innerAssign(key, value)

	method apply = function(index)
		this.lookup
			this.key index

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
			AtomLiteralExec(_, key) = resolveMethod(arrayValuePrototype, key, this)
			_ = fail "Only number or atom keys allowed on array"

	method assign = function(_, index, value)
		with index match
			NumberValue number = (this.values number = value)
			_ = fail "Tried to write non-number index on array"

	method length = this.values.length # Stdlib convenience

let NullValue = inherit Value
	method apply = makePrototypeApply(nullValuePrototype, this)

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
	method apply = function(value)
		with value match
			NumberValue number = new StringValue(this.value number)
			AtomLiteralExec(_, key) = resolveMethod(stringValuePrototype, key, this)
			_ = fail "Only number or atom keys allowed on string"

	method length = this.value.length # Stdlib convenience

let NumberValue = inherit LiteralValue
	method apply = makePrototypeApply(numberValuePrototype, this)

let TrueValue = new NumberValue(1)

let SuperValue = inherit Value
	field parent = null
	field target = null

	method apply = function(index)
		if (not (is AtomLiteralExec index))
			fail "Objects have atom keys only"
		resolveMethod(this.parent, index.value, this.target)

let MethodPseudoValue = inherit Object

let FunctionMethodPseudoValue = inherit MethodPseudoValue
	field scope = null
	field owner = null
	field exe = null

	method call = function(target)
		let scope = new ObjectValue(this.scope)
		scope.atoms.set "this" target
		scope.atoms.set "current" (this.owner)
		scope.atoms.set "super" new SuperValue(this.owner.parent, target)
		this.exe.eval(scope)

let LiteralMethodPseudoValue = inherit MethodPseudoValue
	field fn = null

	method call = function(target)
		this.fn.apply(target)

let resolveMethod = function(source, key, thisValue)
	let value = source.innerLookup(key)
	if (is MethodPseudoValue value)
		value.call(thisValue)
	else
		value

let MatchFunctionValue = inherit Value
	field matches = null
	field scope = null

	method apply = function(value)
		let iMatch = this.matches.iter
		let found = null
		while (and (not found) (iMatch.more))
			let m = iMatch.next
			if (
				do # FIXME: IF I REMOVE THIS "DO" AND JUST SAY "IF", STUFF BREAKS. SOMETHING'S WRONG IN THE READER
					if (m.targetExe)
						do
							let targetValue = m.targetExe.eval(this.scope)
							if (isChild(targetValue, value))
								true
							else
								== (equalityFilter targetValue) (equalityFilter value)
					else
						true
			)
				let scope = this.scope
				if (m.unpacks)
					scope = new ObjectValue(scope)
					let unpackIdx = 0
					while (< unpackIdx (m.unpacks.length))
						let atom = m.unpacks unpackIdx
						scope.atoms.set (atom.value) (value.apply(new NumberValue(unpackIdx)))
						unpackIdx = + unpackIdx 1
				# FIXME: Interesting little quirk here: if due to a bug elsewhere this eval
				# returns a raw Python None or 0.0, very bad things will happen
				found = m.statement.eval(scope)
		if (found)
			found
		else
			fail "No match clause was met"

let globalPackageCache = new Dict

let PackageValue = inherit Value
	field base = null
	field method loaded = new Dict

	method subpath = function(component)
		file.path.normalize
			file.path.join(this.base, component)

	method apply = function(key)
		if (not (is AtomLiteralExec key))
			fail "Package has atom keys only"
		key = key.value
		if (this.loaded.has key)
			this.loaded.get key
		else
			let isDir = false
			let filename = this.subpath(+ key ".em")

			if (not (file.path.isFile filename))
				let dirname = this.subpath(key)
				if (file.path.isDir dirname)
					isDir = true
					filename = dirname
				else
					fail 
						nullJoin array
							"Could not find file \""
							filename
							"\" or directory \""
							dirname
							"\""

			let value = null

			if (globalPackageCache.has filename)
				value = globalPackageCache.get filename

				if (== value null)
					fail
						nullJoin array
							"File \""
							filename
							"\" attempted to recursively load itself while it was still executing"
			else
				if (isDir)
					value = new PackageValue(filename)
				else
					globalPackageCache.set filename null

					let ast = makeAst (file.in filename)
					let exe = exeFromAst(ast)
					value = new ObjectValue()
					exe.evalSequence(defaultScope, value)

				globalPackageCache.set filename value

			this.loaded.set key value
			value

# Stdlib

# Util function
let literalMethod = function(f, n)
	new LiteralMethodPseudoValue(new LiteralFunctionValue(f,n))

# Used by SequenceExec

let scopeExportList = new ObjectValue

# Stdlib: Builtin types

let nullValuePrototype = new ObjectValue

let numberValuePrototype = new ObjectValue

numberValuePrototype.atoms.set "toString"
	literalMethod
		function (this)
			new StringValue(this.value.toString)
		1

numberValuePrototype.atoms.set "toNumber"
	literalMethod
		function (this) (this)
		1

let stringValuePrototype = new ObjectValue

stringValuePrototype.atoms.set "length"
	literalMethod
		function (this)
			new NumberValue(this.value.length)
		1

stringValuePrototype.atoms.set "toNumber"
	literalMethod
		function (this)
			new NumberValue(this.value.toNumber)
		1

stringValuePrototype.atoms.set "toString"
	literalMethod
		function (this) (this)
		1

let atomValuePrototype = new ObjectValue

atomValuePrototype.atoms.set "toString"
	literalMethod
		function (this)
			new StringValue (this.value)
		1

# Stdlib: Arrays

let arrayValuePrototype = new ObjectValue

arrayValuePrototype.atoms.set "length"
	literalMethod
		function (this)
			new NumberValue(this.values.length)
		1

arrayValuePrototype.atoms.set "append"
	literalMethod
		function (this, value)
			this.values.append value
		2

arrayValuePrototype.atoms.set "pop"
	literalMethod
		function (this)
			this.values.pop
		1

# Stdlib: Iterators

let iteratorPrototype = new ObjectValue

let IteratorObjectValue = inherit ObjectValue
	parent = iteratorPrototype
	field source = null
	field idx = 0

iteratorPrototype.atoms.set "more"
	literalMethod
		function (this)
			toBoolValue
				< (this.idx) (this.source.length)
		1

iteratorPrototype.atoms.set "next"
	literalMethod
		function (this)
			let value = this.source.apply(new NumberValue(this.idx))
			this.idx = + (this.idx) 1
			value
		1

let installIter = function(prototype)
	prototype.atoms.set "iter"
		literalMethod
			function (this)
				new IteratorObjectValue(source = this)
			1
installIter arrayValuePrototype
installIter stringValuePrototype

# Stdlib: File I/O

let infilePrototype = new ObjectValue

let outfilePrototype = new ObjectValue

let FileObjectValue = inherit ObjectValue
	field handle = null

let fileObject = new ObjectValue
do
	let makeFileConstructor = function(fn, prototype)
		fileObject.atoms.set (fn.toString)
			new LiteralFunctionValue
				function (path)
					new FileObjectValue(prototype, handle = file fn (path.value))
				1

	makeFileConstructor(.in,     infilePrototype)
	makeFileConstructor(.out,    outfilePrototype)
	makeFileConstructor(.append, outfilePrototype)

let closeWrapper = literalMethod
	function(this)
		this.handle.close
	1

do
	let addWrapper = function(fn)
		outfilePrototype.atoms.set (fn.toString)
			literalMethod
				function (this)
					wrapPrintRepeat (this.handle fn)
				1

	addWrapper .write
	addWrapper .print
	addWrapper .println

outfilePrototype.atoms.set "flush"
	literalMethod
		function(this)
			this.handle.flush
		1

outfilePrototype.atoms.set "close" closeWrapper

# Util function
let newString = function (x) (new StringValue (x))

do
	let addWrapper = function(fn, constructor)
		infilePrototype.atoms.set (fn.toString)
			literalMethod
				function (this)
					constructor(this.handle fn)
				1

	addWrapper .more toBoolValue
	addWrapper .peek newString
	addWrapper .next newString

infilePrototype.atoms.set "close" closeWrapper

# Stdlib: Paths

let pathObject = new ObjectValue
fileObject.atoms.set "path" pathObject

pathObject.atoms.set "join"
	new LiteralFunctionValue
		function(x,y)
			new StringValue( file.path.join( x.value, y.value ) )
		2

do
	let addWrapper = function(fn, constructor)
		pathObject.atoms.set (fn.toString)
			new LiteralFunctionValue
				function (str)
					constructor( file.path fn (str.value) )
				1

	addWrapper .isDir toBoolValue
	addWrapper .isFile toBoolValue
	addWrapper .normalize newString
	addWrapper .dir newString

# Stdlib: Dict

let dictObjectDataKey = new ObjectValue
let dictPrototype = new ObjectValue

# FIXME: Not tolerant to subclassing
let dictData = function (dict)
	if (not (dict.atoms.has dictObjectDataKey))
		dict.atoms.set dictObjectDataKey
			new Dict
	dict.atoms.get dictObjectDataKey

dictPrototype.atoms.set "get"
	literalMethod
		function(dict, index)
			(dictData dict).get
				equalityFilter index
		2

dictPrototype.atoms.set "set"
	literalMethod
		function(dict, index, value)
			(dictData dict).set
				equalityFilter index
				value
		3

dictPrototype.atoms.set "has"
	literalMethod
		function(dict, index)
			toBoolValue
				(dictData dict).has
					equalityFilter index
		2

dictPrototype.atoms.set "del"
	literalMethod
		function(dict, index)
			(dictData dict).del
				equalityFilter index
			NullValue
		2

dictPrototype.atoms.set "iter"
	literalMethod
		function (dict) # Inefficient, it is not necessary to flatten the array
			let result = new ArrayValue
			let i = (dictData dict).iter
			while (i.more)
				let key = i.next

				# Reverse equalityFilter
				if (is Number key)
					key = new NumberValue(key)
				elif (is String key)
					key = new StringValue(key)

				result.values.append(key)
			new IteratorObjectValue(source = result)
		1

# Stdlib: "String garbage"

let charObject = new ObjectValue
do
	let charFunctions = array
		.isNonLineSpace
		.isLineSpace
		.isSpace
		.isQuote
		.isOpenParen
		.isCloseParen
		.isParen
		.isDigit

	let iFn = charFunctions.iter
	while (iFn.more)
		let fn = iFn.next
		charObject.atoms.set (fn.toString)
			new LiteralFunctionValue
				function(x)
					toBoolValue(char fn (x.value))
				1

# Stdlib: Scope

let equalityFilter = function (value)
	with value match
		NumberValue x = x
		StringValue x = x
		_ = value

let wrapBinaryNumber = function(f)
	new LiteralFunctionValue
		function(x,y)
			new NumberValue(f (x.value) (y.value))
		2

let wrapBinaryBool = function(f)
	new LiteralFunctionValue
		function(x,y)
			toBoolValue
				f (x.value) (y.value)
		2

let wrapBinaryEquality = function(f)
	new LiteralFunctionValue
		function(x,y)
			toBoolValue
				f (equalityFilter x) (equalityFilter y)
		2

let wrapBinaryBoolToBool = function(f)
	new LiteralFunctionValue
		function(x,y)
			toBoolValue
				f (isTrue x) (isTrue y)
		2

let printable = function(x)
	with x match
		StringValue v = v
		NumberValue v = v
		AtomLiteralExec = x.value
		NullValue = "null"
		_ = "[Unprintable]"

let wrapPrintRepeat = function(f)
	let repeat = new LiteralFunctionValue
		function (x)
			f
				printable x
			repeat
	repeat

let setEntryFile = function(filename)
	defaultScope.atoms.set "project"
		new PackageValue(file.path.dir(file.path.normalize(filename)))

let rootObject = new ObjectValue

let defaultScope = new ObjectValue

defaultScope.atoms.set "Object" rootObject
defaultScope.atoms.set "String" stringValuePrototype
defaultScope.atoms.set "Number" numberValuePrototype
defaultScope.atoms.set "Array"  arrayValuePrototype

defaultScope.atoms.set "null"   NullValue
defaultScope.atoms.set "argv"   NullValue
defaultScope.atoms.set "ln"     new StringValue(ln)

defaultScope.atoms.set "+" 
	new LiteralFunctionValue
		function (x, y)
			let v = + (x.value) (y.value)
			with x match
				StringValue = new StringValue(v)
				NumberValue = new NumberValue(v)
		2

defaultScope.atoms.set "-" (wrapBinaryNumber -)
defaultScope.atoms.set "*" (wrapBinaryNumber *)
defaultScope.atoms.set "/" (wrapBinaryNumber /)
defaultScope.atoms.set "%" (wrapBinaryNumber %)

defaultScope.atoms.set "<"  (wrapBinaryBool <)
defaultScope.atoms.set "<=" (wrapBinaryBool <=)
defaultScope.atoms.set ">"  (wrapBinaryBool >)
defaultScope.atoms.set ">=" (wrapBinaryBool >=)

defaultScope.atoms.set "==" (wrapBinaryEquality ==)
defaultScope.atoms.set "!=" (wrapBinaryEquality !=)

defaultScope.atoms.set "and" (wrapBinaryBoolToBool and)
defaultScope.atoms.set "or"  (wrapBinaryBoolToBool or)
defaultScope.atoms.set "xor" (wrapBinaryBoolToBool xor)

defaultScope.atoms.set "bool"
	new LiteralFunctionValue
		function (x)
			toBoolValue
				isTrue x
		1

defaultScope.atoms.set "not"
	new LiteralFunctionValue
		function (x)
			toBoolValue
				not
					isTrue x
		1

defaultScope.atoms.set "nullfn"
	new LiteralFunctionValue
		function (x) (NullValue)
		1

defaultScope.atoms.set "with"
	new LiteralFunctionValue
		function (x, y) (y.apply x)
		2

defaultScope.atoms.set "is"
	new LiteralFunctionValue
		function (x,y)
			toBoolValue(isChild(x,y))
		2

defaultScope.atoms.set "exit"
	new LiteralFunctionValue
		function (x)
			exit(x.value)
		1

defaultScope.atoms.set "fail" # Same as exit?
	new LiteralFunctionValue
		function (x)
			fail(x.value)
		1

defaultScope.atoms.set "char" charObject

defaultScope.atoms.set "print"   (wrapPrintRepeat print)
defaultScope.atoms.set "println" (wrapPrintRepeat println)
defaultScope.atoms.set "file"    fileObject
defaultScope.atoms.set "Dict"    dictPrototype
defaultScope.atoms.set "stdout"  new FileObjectValue(outfilePrototype, handle = stdout)
defaultScope.atoms.set "stderr"  new FileObjectValue(outfilePrototype, handle = stderr)
defaultScope.atoms.set "stdin"   new FileObjectValue(infilePrototype, handle = stdin)

defaultScope.atoms.set "package" new ObjectValue()
defaultScope.atoms.set "project" new ObjectValue()

# Dubious, intentionally "undocumented"
defaultScope.atoms.set "DEBUG"
	new LiteralFunctionValue
			function (x)
				stdout.write "----\nDEBUG: "
				with x match
					NumberValue x = stdout.write("number ", x, ln)
					StringValue x = stdout.write("string ", x, ln)
					NullValue = println "null"
					_ = do
						println "object"
						DEBUG x
				print "----\n"
			1

# This is supposed to print the keys sorted, but instead relies on dict iter incidentally sorting things
let debugScopeDump = function(obj)
	let i = obj.atoms.iter
	while (i.more)
		let key = i.next
		let value = obj.atoms.get key
		stdout.write
			key
			": "
			if (is MethodPseudoValue value)
				"[Method]"
			else
				printable value
			ln