# Takes input and turns () [] into indentation

let left = null
let right = null
let indentBy = "    "

with (stdin.peek) match
	"(" = do
		left = "("
		right = ")"
	"[" = do
		left = "["
		right = "]"
	"{" = do
		left = "{"
		right = "}"
	_ = do
		stderr.write
			"Error: First character of input is " 
			stdin.peek
			", but only ( [ { recognized"
			ln
		exit 1

let Linked = inherit object
	field value = null
	field next = null
	method more = (!= (this.next) null)

let TreeNode = inherit Linked

let stack = null
let root = null
let current = ""
let failed = null

let flush = function()
	if (current.length)
		stack.value.append current
		current = ""

let push = function()
	flush()
	let node = new TreeNode(array(), stack)
	if (stack)
		stack.value.append node
	else
		root = node
	stack = node

let pop = function()
	stack = stack.next

push()

while (and (not failed) (stdin.more))
	let ch = stdin.next

	if (== ch right)
		if (stack)
			pop()
		else
			failed = "Too many right parenthesis, halting"
	elif (char.isSpace ch)
		flush()
	else
		if (not stack)
			failed = "Extra junk after final closing parenthesis"
		else
			if (== ch left)
				push()
			else
				current = (+ current ch)

if (stack.more)
	failed = "Input ended but left parenthesis are still unclosed"

let format = function (content, indent)
	with content match
		TreeNode ary = do
			let result = ""
			let i = ary.iter
			while (i.more)
				if (result.length)
					result = (+ result " ")
				result = (+ result (format (i.next, + indent indentBy)))
			result
		_ =
			(+ indent content)

stdout.write
	format (root, "")

if (failed)
	stderr.write
		"Error: "
		failed
		ln
	exit 1