# Misc helper functions/classes
# (Most of this should probably be in the stdlib)

export false = null
export true = 1

# Array utilities

export lastFrom = function(a)
	a (- (a.length) 1)

export popLeft = function(a)
	let left = a 0
	let idx = 0
	while (< idx (- (a.length) 1))
		a idx = a (+ idx 1)
		idx = + idx 1
	a.pop
	left

export appendArray = function (a, b)
	let i = b.iter
	while (i.more)
		a.append (i.next)

export cloneArray = function(a)
	let result = array()
	appendArray(result, a)
	result

export catArray = function (a, b)
	let result = array()
	appendArray(result, a)
	appendArray(result, b)
	result

# Linked list / stack object
export Linked = inherit Object
	field value = null
	field next = null # This looks like an iterator but is immutable. Is this bad
	method more = (!= (this.next) null)

export cloneLinked = function(list)
	if (list)
		let head = new Linked
		let node = head
		while (list)
			let next = new Linked(list.value)
			node.next = next
			list = list.next
		head.next
	else
		null

export cmp = function (x, y)
	if (< x y)
		-1
	elif (> x y)
		1
	else
		0

export insertLinked = function(cmp, list, value)
	let worse = function(node)
		if (not node)
			true
		else
			> 0 (cmp value (node.value))
	let insert = new Linked(value)
	if (worse list)
		insert.next = list
		insert
	else
		let node = list
		let done = false
		while (and (not done) node)
			if (worse (node.next))
				insert.next = node.next
				node.next = insert
				done = true
			node = node.next
		list

export foldl = function(default, f, ary)
	let i = ary.iter
	if (not (i.more))
		default
	else
		let value = i.next
		while (i.more)
			value = f(value, i.next)
		value

export checkErrors = function(errors)
	if (< 0 (errors.length))
		let i = errors.iter
		stderr.println "Compilation failed:"
		while (i.more)
			let error = i.next
			stderr.println
				nullJoin array (error.loc, ": ", error.msg)
		exit 1

export nonempty = function (ary)
	if ary (ary.length)

# String ops

export join = function(joiner)
	foldl "" function (x,y) ( +( +(x.toString, joiner), y.toString) )
export nullJoin = join ""
export startsWith = function(x, y)
	let idx = 0
	let valid = (<= (y.length) (x.length)) # Don't bother if x is shorter
	while (and valid (< idx (y.length)))   # Iterate until difference found
		if (!= (x idx) (y idx))
			valid = false
		idx = + idx 1
	valid

export quotedString = function(s)
	let result = "\""
	let i = s.iter
	while (i.more)
		result = + result do
			let ch = i.next
			with (ch) match
				"\n" = "\\n"
				"\r" = "\\r"
				"\"" = "\\\""
				_ = ch
	result = + result "\""
	result