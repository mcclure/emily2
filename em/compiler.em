# Simple compiler. Currently supports only c#

profile experimental

from project.util import *
from project.execution import
	SequenceExec, SetExec, VarExec, ApplyExec, LiteralExec, AtomLiteralExec, ImportAllExec

let indent = function (count)
	let prefix = ""
	while (count > 0)
		prefix = prefix + "    "
		count = count - 1
	map function(s)
		prefix + (s.toString)

let arrayIndented = function (a, i) (join "\n" (indent i a))

let Var = inherit Object
	field num = 0

	type = "double"
	method name = "_" + (this.num.toString)
	method toString = nullJoin array
		this.type
		" "
		this.name
		";"

let Exp = inherit Object

let ValueExp = inherit Exp
	field value = null

	method toString = this.value.toString

let VarExp = inherit Exp
	field var = null

	method toString = this.var.name

let ApplyExp = inherit Exp
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

let Statement = inherit Object

let ExpStatement = inherit Statement
	field exp = null

	method toString = this.exp.toString + ";"

let AssignStatement = inherit Statement
	field var = null
	field exp = null

	method toString = nullJoin array
		this.var.name
		" = "
		this.exp
		";"

let SeqStatement = inherit Statement
	field frame = null

	method toString = "{\n" + this.frame.toString + "\n}"

let NumGenerator = inherit Object
	field generator = 0

	method next = do
		this.generator = this.generator + 1
		this.generator

let Frame = inherit Object
	field nested = 0
	field method vars = array()
	field method statements = array()

	method indent = this.nested + 2

	method toString =
		join "\n\n" array
			join "\n" (indent (this.indent) (this.vars))
			join "\n" (indent (this.indent) (this.statements))

let buildExp = function(exe, names)
	with exe match
		VarExec = do
			let symbol = exe.symbol
			with symbol match
				"+" = "Add"
				"println" = "Println"
				_ = new VarExp
					names.get symbol
		LiteralExec = new ValueExp
			exe.value
		ApplyExec = do
			let fnExp = buildExp(exe.fn, names)
			let exeExp = buildExp (exe.arg, names)
			if (is ApplyExp fnExp)
				fnExp.args.append exeExp
				fnExp
			else
				new ApplyExp
					fnExp
					array (exeExp)
		_ = fail
			"Unrecognized execution node: " + exe.toString

let buildFrame = function(seqExe, nested, parentNumGenerator, parentNames)
	let frame = new Frame
	let numGenerator = parentNumGenerator || new NumGenerator
	let names = if (parentNames) (cloneDict(parentNames)) else (new Dict)

	# Assume exe is a scoped sequence to start
	let i = seqExe.execs.iter
	while (i.more)
		let exe = i.next

		with exe match
			SequenceExec(_, shouldReturn, hasScope) = do
				if (shouldReturn)
					fail "Can't handle returning sequences"
				if (!hasScope)
					fail "Can't handle scopeless sequences"
				frame.statements.append
					buildFrame
						exe, nested+1, numGenerator, names
			SetExec(_, isLet) = do
				let var = null
				let name = if (exe.targetClause || !is AtomLiteralExec (exe.indexClause))
					fail "Can only assign to local variables right now"
				else
					exe.indexClause.value

				if (isLet)
					var = new Var(numGenerator.next)
					frame.vars.append var
					names.set name var
				else
					var = names.get var

				frame.statements.append
					new AssignStatement(var, buildExp (exe.valueClause, names))
			ApplyExec =
				frame.statements.append
					new ExpStatement
						buildExp exe names
			ImportAllExec = () # TODO
			_ = fail
				"Unrecognized statement node: " + exe.toString
	frame

export build = function(exe)
	join "\n" array
		"public class Program"
		"{"
		"    public static void Println(float x) { Console.WriteLine(x); }\n"
		"    public static void Add(float x, float y) { return x + y }\n"
		"    public static void Main()"
		"    {"
		buildFrame exe 0 null null
		"    }"
		"}"
