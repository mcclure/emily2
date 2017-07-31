from project.util import *
from project.compiler.util import *
from project.compiler.base import SharedCompiler

export JsCompiler = inherit SharedCompiler
	Var = inherit (SharedCompiler.Var)
		method toString = nullJoin array
			"let "
			this.name
			";"

	method build = function(exe)
		join "\n" array
			"function Println(x) { console.log(x); }\n"
			"function Add(x, y) { return x + y; }\n"
			"function Mod(x, y) { return x % y; }\n"
			"function Eq (x, y) { return x == y; }\n"
			"function Geq(x, y) { return x <= y; }\n"
			this.buildFrame exe 0 null null
