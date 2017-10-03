# C# version of compiler

profile experimental

from project.util import *
from project.compiler.util import *
from project.compiler.base import
	ClikeCompiler, Chunk, IndentChunk, AddressableVal, KnownVal
from project.type import
	UnitType, BoolType, NumberType, StringType

export CsCompiler = inherit ClikeCompiler
	scope = do
		let dict = new ChainedDict
		dict.set chainParent (ClikeCompiler.scope)
		dict

	UnitBlock = inherit (current.UnitBlock)
		method buildMain = do
			this.defsChunk = new Chunk
			this.mainChunk = new IndentChunk

			appendArray (this.source.lines) array
				"using System;"
				"public class Program"
				"{"
				new IndentChunk
					array
						this.defsChunk
						""
						"public static void Main()"
						"{"
						this.mainChunk
						"}"
				"}"

	Function = inherit (current.Function)
		method field cases = new NumGenerator

		method appendBlock = do
			let block = new (this.unit.compiler.SwitchBlock)
				id = this.cases.next.toString
				unit = this.unit
			appendArray (this.source.lines) array
				"uint i = 0;"
				"switch (i) {"
				block.buildContentChunk
				"}"
			block

	SwitchBlock = inherit (current.BlockBlock)
		field id = null

		method buildContentChunk = do
			this.source = new Chunk
			new IndentChunk
				lines = array
					"case " + this.label + ": {"
					new IndentChunk
						lines = array
							this.source
							"break;"
					"}"

		method buildVal = function(assignVal, dataVal)
			if (!assignVal)
				assignVal = this.addVar(null, exp.type, null)
			let compiler = this.unit.compiler
			appendArray (this.source.lines) array
				compiler.valToString(assignVal) + " = " + compiler.valToString(dataVal) + ";"

		method addVar = this.unit.addVar
		method addLiteral = this.unit.addLiteral

		method label = this.id.toString
	
	method buildVarInto = function(defsChunk, value, description)
		appendArray (defsChunk.lines) array
			"static " + this.typeToString (value.type) + " " + this.valToString(value) + ";" +
				if (description)
					" // " + description
				else
					""

	method typeToString = function(type)
		with (type.resolve) match
			BoolType = "bool"
			NumberType = "float"
			StringType = "string"
			UnitType = "void"
			_ = fail "Can't translate this type"

	method valToString = function(val)
		with val match
			AddressableVal = val.label
			KnownVal = this.literalToString (val.value)

	method literalToString = function(value)
		with value match
			String = "\"" + value + "\"" # NO!
			Number = value.toString
			none = "null"
			_ = fail "Can't translate this literal"

