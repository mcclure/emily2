# Test no garbage allowed before macro
# Expect failure

let macro
	splitMacro(600, "+")
