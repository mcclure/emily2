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

# BaseCompiler mostly serves as a network of interfaces
export BaseCompiler = inherit Object
	scope = do
		let dict = new ChainedDict
		dict.set "ln" new KnownVal
			null, StringType, "\n"
		dict

	Var = inherit Object

	Exp = inherit Object

	# Types of expression, statement etc
	# String generation for individual items goes here

	ValueExp = inherit (current.Exp)

	VarExp = inherit (current.Exp)
		field var = null

		method toString = this.var.name

	ApplyExp = inherit (current.Exp)

	Statement = inherit Object

	ExpStatement = inherit (current.Statement)

	AssignStatement = inherit (current.Statement)

	SeqStatement = inherit (current.Statement)
		field frame = null

	# FIXME: fields can't be moved into SharedCompiler because no mixed inheritance oh well
	IfBase = inherit (current.SeqStatement) # Note argument order is like: x if y else z
		field condExp = null
		field nextExp = null

	IfExp = inherit (current.IfBase)

	IfStatement = inherit (current.IfBase)

	WhileStatement = inherit (current.SeqStatement)

	# Track a { } here
	Frame = inherit Object
		field nested = 0
		field method vars = array()
		field method statements = array()

		method indent = this.nested + 2

	# Turn expression Execs to Exps here
	method buildExp = function(exe, names)
		with exe match
			VarExec = do
				let symbol = exe.symbol
				with symbol match
					"+" = "Add"
					"%" = "Mod"
					"==" = "Eq"
					"<=" = "Geq"
					"println" = "Println"
					_ = new (this.VarExp)
						names.get symbol
			StringLiteralExec = new (this.ValueExp)
				"\"" + exe.value + "\""
			LiteralExec = new (this.ValueExp)
				exe.value
			NullLiteralExec = new (this.ValueExp)
				"false"
			ApplyExec = do
				let fnExp = this.buildExp(exe.fn, names)
				let exeExp = this.buildExp (exe.arg, names)
				if (is (current.ApplyExp) fnExp)
					fnExp.args.append exeExp
					fnExp
				else
					new (this.ApplyExp)
						fnExp
						array (exeExp)
			IfExec = new (this.IfExp)
				this.buildExp (exe.ifClause) names
				this.buildExp (exe.condClause) names
				if (exe.elseClause)
					this.buildExp (exe.elseClause) names
				else
					new (this.ValueExp) ("false")
			_ = fail
				"Unrecognized execution node: " + exe.toString

	# Turn statement Execs to Statements here
	method buildFrame = function(seqExe, nested, parentNumGenerator, parentNames)
		let frame = new (this.Frame)
		let numGenerator = parentNumGenerator || new NumGenerator
		let names = if (parentNames) (cloneDict(parentNames)) else (new Dict)

		let pushStatement = function (exe)
			let buildLocalFrame = function(exe)
				this.buildFrame
					exe, nested+1, numGenerator, names

			with exe match
				SequenceExec(_, shouldReturn, hasScope) = do
					if (shouldReturn)
						fail "Can't handle returning sequences"
					if (!hasScope)
						fail "Can't handle scopeless sequences"
					frame.statements.append
						buildFrame exe
				SetExec(_, isLet) = do
					let var = null
					let name = if (exe.targetClause || !is AtomLiteralExec (exe.indexClause))
						fail "Can only assign to local variables right now"
					else
						exe.indexClause.value

					if (isLet)
						var = new (this.Var)(numGenerator.next)
						frame.vars.append var
						names.set name var
					else
						var = names.get name

					frame.statements.append
						new (this.AssignStatement)(var, this.buildExp (exe.valueClause, names))
				ApplyExec =
					frame.statements.append
						new (this.ExpStatement)
							this.buildExp exe names
				IfExec =
					frame.statements.append
						if (exe.loop)
							new (this.WhileStatement)
								buildLocalFrame (exe.ifClause)
								this.buildExp (exe.condClause) names
						else
							new (this.IfStatement)
								buildLocalFrame (exe.ifClause)
								this.buildExp (exe.condClause) names
								if (exe.elseClause)
									buildLocalFrame (exe.elseClause)
				ImportAllExec = () # TODO
				_ = fail
					"Unrecognized statement node: " + exe.toString

		# Assume exe is a scoped sequence to start
		if (is SequenceExec seqExe)
			let i = seqExe.execs.iter
			while (i.more)
				pushStatement(i.next)
		else
			pushStatement(seqExe)
			
		frame

# SharedCompiler is the methods that are LIKELY but not certain to be shared among all branches
# FIXME: Could this be merged with Compiler? FIXME: Put fields into base version, separate strings over here
# "Type signatures" of objects should be the same
export SharedCompiler = inherit BaseCompiler
	Var = inherit (BaseCompiler.Var)
		field num = 0

		method name = "_" + (this.num.toString)

	ValueExp = inherit (BaseCompiler.ValueExp)
		field value = null

		method toString = this.value.toString

	ApplyExp = inherit (BaseCompiler.ApplyExp)
		field target = null # String or Var
		field method args = array()

		method toString = do
			if (this.target == null)
				fail "Badly formed Exp"
			let targetString = with (this.target) match
				String = this.target
				VarExec = this.target.name
				_ = fail "Malformed ApplyExp node"

			targetString + "(" + (join ", " (this.args)) + ")"

	ExpStatement = inherit (BaseCompiler.ExpStatement)
		field exp = null

		method toString = this.exp.toString + ";"

	AssignStatement = inherit (BaseCompiler.AssignStatement)
		field var = null
		field exp = null

		method toString = nullJoin array
			this.var.name
			" = "
			this.exp
			";"

	SeqStatement = inherit (BaseCompiler.SeqStatement)
		method toString = "{\n" + this.frame.toString + "\n}"

	IfExp = inherit (BaseCompiler.IfExp)
		method toString = nullJoin array
			"("
			this.condExp
			" ? "
			this.frame # Note: Type of frame overloaded/broken here
			" : "
			this.nextExp
			")"

	IfStatement = inherit (BaseCompiler.IfStatement)
		method toString = nullJoin array
			"if ("
			this.condExp
			") "
			this.frame
			if (this.nextExp)
				" else " + this.nextExp.toString
			else
				""

	WhileStatement = inherit (BaseCompiler.WhileStatement)
		field condExp = null

		method toString = nullJoin array
			"while (true) {\n"
			"    if (!("
			this.condExp
			"))\n"
			"        break;\n"
			this.frame
			"}"

	Frame = inherit (BaseCompiler.Frame)
		method toString =
			join "\n\n" array
				join "\n" (indent (this.indent) (this.vars))
				join "\n" (indent (this.indent) (this.statements))

# ClikeCompiler is methods common to C and C# (ie explicitly typed languages) but not JavaScript
export ClikeCompiler = inherit SharedCompiler
	Var = inherit (SharedCompiler.Var)
		type = "double"

		method toString = nullJoin array
			this.type
			" "
			this.name
			";"
	