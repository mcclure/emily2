# C# version of compiler

profile experimental

from project.util import *
from project.compiler.util import *
from project.compiler.base import
	ClikeCompiler, Chunk, IndentChunk, AddressableVal, KnownVal, TemplateVal, PartialApplyVal, upgradeTemplateVal
from project.type import
	UnitType, BoolType, NumberType, StringType

let invokeTemplate = function(name)
	function(a)
		name + "(" + join(", ", a) + ")"

export CsCompiler = inherit ClikeCompiler
	scope = do
		let dict = new ChainedDict
		dict.set chainParent (ClikeCompiler.scope)
		upgradeTemplateVal
			dict, "+", 2
			function(a)
				a 0 + " + " + a 1
			null
		upgradeTemplateVal
			dict, "println", 1
			invokeTemplate "Println"
			"public static void Println<T>(T x) { Console.WriteLine(x); }"
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

		method buildStatement = function(val)
			appendArray (this.source.lines) array
				this.unit.compiler.valToString(val) + ";"

		method addVar = this.unit.addVar
		method addLiteral = this.unit.addLiteral
		method addRawGlobal = this.unit.addRawGlobal

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
			(project.type.UnknowableType) = fail "Can't translate unknowable type" # DON'T CHECK THIS LINE IN
			_ = fail "Can't translate this type"

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

