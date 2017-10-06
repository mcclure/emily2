# Simple compiler. Currently supports only c#

profile experimental

from project.util import *
from project.compiler.util import *
from project.execution import
	SequenceExec, SetExec, IfExec, VarExec, ApplyExec, ImportAllExec
	LiteralExec, NullLiteralExec, StringLiteralExec, AtomLiteralExec
from project.type import # FIXME
	TypedNode, ReferType, UnitType, BoolType, NumberType, StringType, AtomType, UnknowableType
	Val, KnownTypeVal, functionTypeVal

# Notice use of current vs this; the current version is used when matching; the this version, when constructing

export UnitVal = inherit Val
export KnownVal = inherit Val
	field value = null

export TemplateVal = inherit Val
	field arity = 0
	field fn = null # Currently assume binary
	field prefix = null

export invokeTemplate = function(name)
	function(a)
		name + "(" + join(", ", a) + ")"

export upgradeTemplateVal = function(dict, name, arity, fn, prefix)
	let val = dict.get name
	if (val == chainNotFound)
		fail
			"Internal error: Tried to upgrade nonexistent val " + name
	dict.set name new TemplateVal
		val.loc, val.type, arity, fn, prefix

export PartialApplyVal = inherit Val
	field fnVal = null
	field args = null

# Unsure
export AddressableVal = inherit Val
	field id = null

	method label = "v" + this.id.toString

export Chunk = inherit Object
	field method lines = array()

export IndentChunk = inherit Chunk

let chunkBuildImpl = function (chunk, depth)
	let result = ""
	let i = chunk.lines.iter
	while (i.more)
		let line = i.next
		result = result +
			with line match
				String = indentPrefix depth + line + "\n"
				IndentChunk = chunkBuildImpl line (depth + 1)
				Chunk = chunkBuildImpl line depth
	result

export chunkBuild = function (chunk)
	chunkBuildImpl chunk 0

# BaseCompiler covers that would apply to any possible compiler
export BaseCompiler = inherit Object
	# Note: When values here are overridden, they must preserve the same types
	scope = do
		let dict = new ChainedDict
		dict.set "ln" new KnownVal
			null, StringType, "\n"
		dict.set "println"
			functionTypeVal array(NumberType, UnitType)
		dict

	# FIXME: This object seems overloaded. What is this for?
	# Hypothesis: Once this is all well typed, a Block will be something that can register variables
	Block = inherit Object

	UnitBlock = inherit (current.Block)
		field compiler = null
		field method errors = array()
		field method source = new Chunk
		field method globalScope = new ChainedDict
		field method names = new NumGenerator
		field defsChunk = null
		field mainChunk = null
		field mainFunction = null

		method main = do
			if (this.mainChunk)
				fail "Tried to generate main chunk twice"
			this.buildMain
			this.mainFunction = new (this.compiler.Function) (this, this.mainChunk)
			this.mainFunction.appendBlock

		method addVar = function(loc, type, description)
			let value = new AddressableVal
				loc = loc
				type = type
				id = this.names.next
			this.compiler.buildVarInto (this.defsChunk) value description
			value

		method addLiteral = function(exe)
			new KnownVal(exe.loc, exe.type, exe.value)

		method addRawGlobal = function(s)
			this.defsChunk.lines.append(s)

	# FIXME: Rename this.
	Function = inherit Object
		field unit = null # All populated by UnitBlock
		field source = null
		# Overload: firstBlock

	# FIXME: Absolutely rename this
	BlockBlock = inherit (current.Block)
		field source = null # Populated by creator

	method buildBlockImpl = function(block, scope, exe)
		with exe match
			SequenceExec(_, shouldReturn, hasScope, execs) = do
				if (hasScope)
					let newScope = new ChainedDict
					newScope.set chainParent (scope)
					scope = newScope

				let finalResult = null
				let i = execs.iter
				while (i.more)
					finalResult = this.buildBlockImpl(block, scope, i.next)
					if (is PartialApplyVal finalResult)
						block.buildStatement(finalResult)

				if (shouldReturn)
					finalResult
				else
					UnitVal
			SetExec(_, isLet) = do
				let dataVal = this.buildBlockImpl(block, scope, exe.valueClause)

				if (is UnitVal (dataVal.type.resolve))
					fail "Cannot assign unit to variable" # This message sucks

				let name = exe.indexClause.value
				let assignVal = if (isLet)
					let newTarget = block.addVar (exe.loc, dataVal.type, name)
					scope.set name newTarget
					newTarget
				else
					scope.get name

				block.buildVal(assignVal, dataVal)

				UnitVal
			VarExec(_, symbol) = do
				let val = scope.get symbol
				if (val == chainNotFound)
					# Promote a "known" value
					let knownVal = this.scope.get symbol
					if (knownVal == chainNotFound)
						fail
							"Variable name not known: " + symbol # Message should include name
					block.unit.globalScope.set symbol knownVal
					if (is TemplateVal knownVal && knownVal.prefix)
						block.addRawGlobal(knownVal.prefix)
					val = knownVal
				val
			LiteralExec = block.addLiteral exe
			ApplyExec(_, fn, arg) = do # TODO: This only works with non-curried templates now, basically
				let fnVal = this.buildBlockImpl(block, scope, fn)
				let argVal = this.buildBlockImpl(block, scope, arg)

				with fnVal match
					TemplateVal = new PartialApplyVal
						type = exe.type
						fnVal=fnVal
						args=array(argVal)
					PartialApplyVal = do
						if (fnVal.args.length > fnVal.fnVal.arity)
							fail "Too many applications on function for current compiler"
						new PartialApplyVal
							type = exe.type
							fnVal = fnVal.fnVal
							args = catArrayElement(fnVal.args, argVal)
					_ = fail "Don't know how to apply this yet"
			ImportAllExec = UnitVal # TODO
			NullLiteralExec = new KnownVal (value = null)

	method buildBlock = function(seqExe)
		let unit = new (this.UnitBlock) (compiler = this)
		let scope = new ChainedDict
		scope.set chainParent (unit.globalScope)

		this.buildBlockImpl (unit.main) scope seqExe

		chunkBuild (unit.source)

	method build = function(exe)
		exe.check (this.scope)
		this.buildBlock exe

# ClikeCompiler is for algol-y looking languages that have a switch statement
export ClikeCompiler = inherit BaseCompiler
	scope = do
		let dict = new ChainedDict
		dict.set chainParent (current.scope)
		dict

	Function = inherit (current.Function)
		method field cases = new NumGenerator

		method appendBlock = do
			let block = new (this.unit.compiler.SwitchBlock)
				id = this.cases.next.toString
				unit = this.unit
			this.source.lines.append
				block.buildEntryChunk
			block

	SwitchBlock = inherit (current.BlockBlock)
		field id = null

		method buildVal = function(assignVal, dataVal)
			if (!assignVal)
				assignVal = this.addVar(null, exp.type, null)
			let compiler = this.unit.compiler
			appendArray (this.source.lines) array
				compiler.valToString(assignVal) + " = " + compiler.valToString(dataVal) + ";"

		method buildStatement = function(val)
			appendArray (this.source.lines) array
				this.unit.compiler.valToString(val) + ";"

		method addVar = this.unit.addVar
		method addLiteral = this.unit.addLiteral
		method addRawGlobal = this.unit.addRawGlobal

		method label = this.id.toString

	method valToString = function(val)
		with val match
			AddressableVal = val.label
			KnownVal = this.literalToString (val.value)
			PartialApplyVal = do
				let fnVal = val.fnVal
				with fnVal match
					TemplateVal = do
						if (val.args < fnVal.arity)
							fail "No currying yet"
						fnVal.fn (map (this.valToString) (val.args))


	method literalToString = function(value)
		with value match
			String = "\"" + value + "\"" # NO!
			Number = value.toString
			none = "null"
			_ = fail "Can't translate this literal"


# Mass-install binary operators which conveniently are the same in Emily + All targets
do
	let bScope = BaseCompiler.scope
	let cScope = ClikeCompiler.scope
	let install = function (ops, returnType)
		let i = ops.iter
		while (i.more)
			let name = i.next
			let loc = null
			bScope.set name
				functionTypeVal array(NumberType, NumberType, returnType)
			upgradeTemplateVal
				cScope, name, 2
				function(a)
					"(" + a 0 + ") " + name + " (" + a 1 + ")"
				null
	install array("+", "-", "*", "/", "%") NumberType
	install array("<=", ">=", "<", ">", "==") BoolType

# CtypedCompiler is methods common to C and C# (ie explicitly typed languages) but not JavaScript
export CtypedCompiler = inherit ClikeCompiler
	UnitBlock = inherit (current.UnitBlock)
		method buildMain = do
			this.defsChunk = new Chunk
			this.mainChunk = new IndentChunk
	