let a = inherit  object
	field x = 1
	field y = 2
	method x = this.x

let b = new a
	x = 3
	y = 4

println
	a.x
	a.y
	b.x
	b.y
	b.z
