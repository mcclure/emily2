# Goal for pass 1 is to execute this

# Expect: 3628800

let.x 10
let.y 1

while (> x 0)
	set.y
		* y x
	set.x
		- x 1

print y

