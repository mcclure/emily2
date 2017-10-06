from project.util import *
from project.compiler.util import *
from project.compiler.base import
	ClikeCompiler, Chunk, IndentChunk, upgradeTemplateVal, invokeTemplate

profile experimental

export JsCompiler = inherit ClikeCompiler
	scope = do
		let dict = new ChainedDict
		dict.set chainParent (current.scope)
		upgradeTemplateVal
			dict, "println", 1
			invokeTemplate "console.log"
			null
		dict

	UnitBlock = inherit (current.UnitBlock)
		method buildMain = do
			this.defsChunk = new Chunk
			this.mainChunk = new Chunk

			appendArray (this.source.lines) array # Javascript: It Apparently Has No Syntax
				this.defsChunk
				this.mainChunk

	SwitchBlock = inherit (current.SwitchBlock)
		method buildEntryChunk = new Chunk
			lines = array
				"let i = 0;"
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
			"let" + " " + this.valToString(value) + ";" +
				if (description)
					" // " + description
				else
					""


