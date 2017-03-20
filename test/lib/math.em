# Test arithmetic stuff in library

let x = 1
let y = "okay"

print

# Expect: 1.0 1.0 null null 1.0 1.0 null null null
	== 1 1
	== 1 x
	== 2 1
	== 2 x
	== "okay" "okay"
	== "okay" y
	== "bad" "okay"
	== "bad" y
	== 3 "3"
	ln

# Expect: null null 1.0 1.0 null null 1.0 1.0 1.0
	!= 1 1
	!= 1 x
	!= 2 1
	!= 2 x
	!= "okay" "okay"
	!= "okay" y
	!= "bad" "okay"
	!= "bad" y
	!= 3 "3"
	ln

# Expect: 1.0 null null
	bool "true"
	bool 0
	bool null
	ln

# Expect: null 1.0 1.0
	not "true"
	not 0
	not null
	ln

# Expect: 4.0 1.0 1.0 6.0 2.0 2.0
	+ 3 1
	== 4 (+ 3 1)
	- 3 2
	* 3 2
	/ 4 2
	% 5 3
	ln

# Expect: okayokay 1.0
	+ y y
	== "okayokay" (+ y y)
	ln

# Expect: 1.0 null null null 1.0 1.0 null 1.0
	> 3 1
	> 3 3
	< 3 1
	< 3 3
	>= 3 1
	>= 3 3
	<= 3 1
	<= 3 3
	ln

# Expect: null null 1.0
	and null null
	and null 1
	and 1 1
	ln

# Expect: null 1.0 1.0
	or null null
	or null 1
	or 1 1
	ln

# Expect: null 1.0 null
	xor null null
	xor null 1
	xor 1 1
	ln

# Expect: null
	nullfn 3
	ln
