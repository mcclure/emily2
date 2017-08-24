from project.util import *
from project.compiler.util import *
from project.compiler.base import BaseCompiler
from project.execution import
	InvalidExec, SequenceExec, LiteralExec, NullLiteralExec, VarExec, ApplyExec, SetExec
	ImportAllExec, MakeFuncExec, MakeObjectExec, MakeArrayExec, MakeMatchExec, IfExec,
	AtomLiteralExec

profile experimental

let unwrapApply = function(exe)
	if (is ApplyExec exe)
		catArray (unwrapApply (exe.fn)) array(exe.arg)
	else
		array(exe)

export TypePrinter = inherit Object
	method printDepth = function(depth, exe)
		let descend = function (distance) (this.printDepth (depth + distance))
		let sequence = function (distance, nodes)
			nullJoin
				map function (x) ("\n" + descend distance x) nodes

		indentPrefix depth + with exe match
			InvalidExec = "[INVALID]"
			SequenceExec = "Sequence:" + exe.type.toString +
				if (exe.hasScope)
					let indented = indentPrefix (depth+1)
					let indented2 = indentPrefix (depth+2)
					nullJoin array
						"\n", indented, "Vars"
						foldIter \+ ""
							mapIter
								function(x)
									array
										"\n" + indented2 + x + ":" + exe.typeScope.get(x).type.toString
								exe.typeScope.shallowIter
						"\n", indented, "Statements"
						sequence 2 (exe.execs)
				else
					sequence 1 (exe.execs)
			LiteralExec = "Literal:" + exe.type.toString
			NullLiteralExec = "NullLiteral:" + exe.type.toString
			VarExec = "Var:" + exe.type.toString + " " + exe.symbol
			ApplyExec = "Apply:" + exe.type.toString + sequence 1 (unwrapApply exe)
			SetExec = "Set" + 
				if (!exe.targetClause && is AtomLiteralExec (exe.indexClause))
					" " + exe.indexClause.value + " =\n" + descend 1 (exe.valueClause)
				else
					"[SET INDEXED]"
			ImportAllExec = "[IMPORTALL]"
			MakeFuncExec = "[CLOSURE]"
			MakeObjectExec = "[NEW OBJECT]"
			MakeArrayExec = "[NEW ARRAY]"
			MakeMatchExec = "[MATCH]"
			IfExec = "[IF]"
			_ = "[UNKNOWN]"

	method build = function(exe)
		exe.check (BaseCompiler.scope)
		
		this.printDepth 0 exe
