from project.util import *
from project.compiler.util import *
from project.compiler.base import ClikeCompiler

export CppCompiler = inherit ClikeCompiler

	method build = function(exe)
		join "\n" array
			"#include <iostream>\n"
			"#include <math.h>\n"
			"template <class T> static void Println(T x) { std::cout << x << std::endl; }\n"
			"static double Add(double x, double y) { return x + y; }\n"
			"static double Mod(double x, double y) { return fmod(x, y); }\n"
			"static bool Eq(double x, double y) { return x == y; }\n"
			"static bool Geq(double x, double y) { return x <= y; }\n"
			"int main(int argc, char **argv)"
			"{"
			this.buildFrame exe 0 null null
			"	return 0;"
			"}"
