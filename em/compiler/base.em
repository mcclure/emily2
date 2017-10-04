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

export upgradeTemplateVal = function(dict, name, arity, fn)
	let val = dict.get name
	if (val == chainNotFound)
		fail "Internal error: Tried to upgrade nonexistent val"
	dict.set name new TemplateVal
		val.loc, val.type, arity, fn

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

# BaseCompiler mostly serves as a network of interfaces
export BaseCompiler = inherit Object
	scope = do
		let dict = new ChainedDict
		dict.set "ln" new KnownVal
			null, StringType, "\n"
		dict.set "+"
			functionTypeVal array(NumberType, NumberType, NumberType)
		dict

	# FIXME: This object seems overloaded. What is this for?
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
					this.scope.set symbol knownVal
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

# SharedCompiler is the methods that are LIKELY but not certain to be shared among all branches
# FIXME: Could this be merged with Compiler? FIXME: Put fields into base version, separate strings over here
# "Type signatures" of objects should be the same
export SharedCompiler = inherit BaseCompiler
	# TODO

# ClikeCompiler is methods common to C and C# (ie explicitly typed languages) but not JavaScript
export ClikeCompiler = inherit SharedCompiler
	# TODO
	