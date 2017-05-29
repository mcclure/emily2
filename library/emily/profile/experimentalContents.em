# Manually loaded	 from minimal profile

export ! = not
export ~ = neg
export && = and
export || = or
export ^^ = xor

export macro
	splitMacro(611, "+")
	splitMacro(611, "-")

	splitMacro(621, "*")
	splitMacro(621, "/")
	splitMacro(621, "%")

	splitMacro(631, "<")
	splitMacro(631, ">")
	splitMacro(631, "<=")
	splitMacro(631, ">=")

	splitMacro(641, "==")
	splitMacro(641, "!=")

	splitMacro(665, "^^")

	unaryMacro(671, "!")
	unaryMacro(671, "~")

export macro shortCircuitBoolean # 661, 663
