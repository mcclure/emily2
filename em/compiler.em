# Simple compiler. Currently supports only c#

profile experimental

from project.util import *
from project.execution import
	SequenceExec, SetExec, IfExec, VarExec, ApplyExec, ImportAllExec
	LiteralExec, NullLiteralExec, StringLiteralExec, AtomLiteralExec

# Helpers

let indent = function (count)
	let prefix = ""
	while (count > 0)
		prefix = prefix + "    "
		count = count - 1
	map function(s)
		prefix + (s.toString)

let arrayIndented = function (a, i) (join "\n" (indent i a))

# Building blocks

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

# Types of expression, statement etc
# String generation for individual items goes here

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

let IfBase = inherit SeqStatement # Note argument order is like: x if y else z
	field condExp = null
	field nextExp = null

let IfExp = inherit IfBase
	method toString = nullJoin array
		"("
		this.condExp
		" ? "
		this.frame # Note: Type of frame overloaded/broken here
		" : "
		this.nextExp
		")"

let IfStatement = inherit IfBase
	method toString = nullJoin array
		"if ("
		this.condExp
		") "
		this.frame
		if (this.nextExp)
			" else " + this.nextExp.toString
		else
			""

let WhileStatement = inherit SeqStatement
	field condExp = null

	method toString = nullJoin array
		"while (true) {\n"
		"    if (!("
		this.condExp
		"))\n"
		"        break;\n"
		this.frame
		"}"

# Code generator driver

let NumGenerator = inherit Object
	field generator = 0

	method next = do
		this.generator = this.generator + 1
		this.generator

# Track a { } here
let Frame = inherit Object
	field nested = 0
	field method vars = array()
	field method statements = array()

	method indent = this.nested + 2

	method toString =
		join "\n\n" array
			join "\n" (indent (this.indent) (this.vars))
			join "\n" (indent (this.indent) (this.statements))


# Turn expression Execs to Exps here
let buildExp = function(exe, names)
	with exe match
		VarExec = do
			let symbol = exe.symbol
			with symbol match
				"+" = "Add"
				"%" = "Mod"
				"==" = "Eq"
				"<=" = "Geq"
				"println" = "Println"
				_ = new VarExp
					names.get symbol
		StringLiteralExec = new ValueExp
			"\"" + exe.value + "\""
		LiteralExec = new ValueExp
			exe.value
		NullLiteralExec = new ValueExp
			"false"
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
		IfExec = new IfExp
			buildExp (exe.ifClause) names
			buildExp (exe.condClause) names
			if (exe.elseClause)
				buildExp (exe.elseClause) names
			else
				new ValueExp ("false")
		_ = fail
			"Unrecognized execution node: " + exe.toString

# Turn statement Execs to Statements here
let buildFrame = function(seqExe, nested, parentNumGenerator, parentNames)
	let frame = new Frame
	let numGenerator = parentNumGenerator || new NumGenerator
	let names = if (parentNames) (cloneDict(parentNames)) else (new Dict)

	# Assume exe is a scoped sequence to start
	let i = seqExe.execs.iter
	while (i.more)
		let exe = i.next
		let buildLocalFrame = function(exe)
			buildFrame
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
					var = new Var(numGenerator.next)
					frame.vars.append var
					names.set name var
				else
					var = names.get name

				frame.statements.append
					new AssignStatement(var, buildExp (exe.valueClause, names))
			ApplyExec =
				frame.statements.append
					new ExpStatement
						buildExp exe names
			IfExec =
				frame.statements.append
					if (exe.loop)
						new WhileStatement
							buildLocalFrame (exe.ifClause)
							buildExp (exe.condClause) names
					else
						new IfStatement
							buildLocalFrame (exe.ifClause)
							buildExp (exe.condClause) names
							if (exe.elseClause)
								buildLocalFrame (exe.elseClause)
			ImportAllExec = () # TODO
			_ = fail
				"Unrecognized statement node: " + exe.toString
	frame

export build = function(exe)
	join "\n" array
		"using System;"
		"public class Program"
		"{"
		"    public static void Println<T>(T x) { Console.WriteLine(x); }\n"
		"    public static double Add(double x, double y) { return x + y; }\n"
		"    public static double Mod(double x, double y) { return x % y; }\n"
		"    public static bool Eq (double x, double y) { return x == y; }\n"
		"    public static bool Geq(double x, double y) { return x <= y; }\n"
		"    public static void Main()"
		"    {"
		buildFrame exe 0 null null
		"    }"
		"}"
