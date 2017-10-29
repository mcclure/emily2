# C# version of compiler

profile experimental

from project.util import *
from project.compiler.util import *
from project.compiler.base import
	CtypedCompiler, Chunk, IndentChunk, AddressableVal, KnownVal, TemplateVal, PartialApplyVal
	upgradeTemplateVal, invokeTemplate
from project.type import
	UnitType, BooleanType, NumberType, StringType

export CsCompiler = inherit CtypedCompiler
	scope = do
		let dict = new ChainedDict
		dict.set chainParent (current.scope)
		upgradeTemplateVal
			dict, "println", 1
			invokeTemplate "Println"
			"static void Println<T>(T x) { Console.WriteLine(x); }"
		dict

	UnitBlock = inherit (current.UnitBlock)
		method buildMain = do
			super.buildMain

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
		method buildEntryChunk = new Chunk
			lines = array
				"uint i = 0;"
				"bool run = true;"
				"while (run) {"
				new IndentChunk
					lines = array
						"switch (i) {"
						this.caseChunk
						"}"
				"}"

	SwitchBlock = inherit (current.SwitchBlock)
		method jump = function(block) # Assume goto/branchGoto are called at most once
			this.exitChunk.lines = array
				"goto case " + block.label + ";"

		method condJump = function(condVal, trueBlock, falseBlock) # Assume jump/branchJump are called at most once
			this.exitChunk.lines = array
				"if (" + this.unit.compiler.valToString condVal + ")"
				new IndentChunk
					lines = array
						"i = " + trueBlock.label + ";"
				"else"
				new IndentChunk
					lines = array
						"i = " + falseBlock.label + ";"
				"break;"
	
	method buildVarInto = function(defsChunk, value, description)
		appendArray (defsChunk.lines) array
			"static " + this.typeToString (value.type) + " " + this.valToString(value) + ";" +
				if (description)
					" // " + description
				else
					""

	method typeToString = function(type)
		with (type.resolve) match
			BooleanType = "bool"
			NumberType = "float"
			StringType = "string"
			UnitType = "void"
			(project.type.UnknowableType) = fail "Can't translate unknowable type" # DON'T CHECK THIS LINE IN
			_ = fail "Can't translate this type"

	method literalToString = function(value)
		with value match
			String = "\"" + value + "\"" # NO!
			Number = value.toString
			Boolean = value.toString
			null = "null"
			_ = fail "Can't translate this literal"

