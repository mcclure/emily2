# C# version of compiler

profile experimental

from project.util import *
from project.compiler.util import *
from project.compiler.base import
	ClikeCompiler, Chunk, IndentChunk

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
				label = this.cases.next.toString
			appendArray (this.source.lines) array
				"switch (" + block.label + ") {"
				block.buildContentChunk
				"}"
			block.source

	SwitchBlock = inherit (current.BlockBlock)
		field label = null

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
