# Simple compiler. Currently supports only c#

profile experimental

from project.util import *
from project.compiler.util import *
from project.execution import
	SequenceExec, SetExec, IfExec, VarExec, ApplyExec, ImportAllExec
	LiteralExec, NullLiteralExec, StringLiteralExec, AtomLiteralExec
from project.type import # FIXME
	TypedNode, ReferType, UnitType, BoolType, NumberType, StringType, AtomType, UnknowableType, Val

# Notice use of current vs this; the current version is used when matching; the this version, when constructing

export KnownVal = inherit Val
	field value = null

#export KnownVal = inherit Val
#export ConstVal = inherit KnownVal
#export TemplateVal = inherit KnownVal

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
			SequenceExec(_, shouldReturn, hasScope) = ()
			SetExec(_, isLet) = do
				if (isLet)
					this.defsChunk
			ApplyExec = ()
			ImportAllExec = () # TODO

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
	