# C# version of compiler

profile experimental

from project.util import *
from project.compiler.util import *
from project.compiler.base import
	CtypedCompiler, Chunk, IndentChunk, AddressableVal, KnownVal, TemplateVal, PartialApplyVal
	upgradeTemplateVal, invokeTemplate
from project.type import
	UnitType, BoolType, NumberType, StringType

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

	SwitchBlock = inherit (current.SwitchBlock)
		method buildEntryChunk = new Chunk
			lines = array
				"uint i = 0;"
				"switch (i) {"
				this.buildContentChunk
				"}"

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

	method literalToString = function(value)
		with value match
			String = "\"" + value + "\"" # NO!
			Number = value.toString
			none = "null"
			_ = fail "Can't translate this literal"

