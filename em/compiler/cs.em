from project.util import *
from project.compiler.util import *
from project.compiler.base import
	ClikeCompiler, ChainedDict, chainParent

export CsCompiler = inherit ClikeCompiler
	scope = do
		let dict = new ChainedDict
		dict.set chainParent (ClikeCompiler.scope)
		dict

	method build = function(exe)
		exe.check (this.scope)
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
			this.buildFrame exe 0 null null
			"    }"
			"}"