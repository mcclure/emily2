# Test grouping, order of operations, etc

profile experimental

let true = 1

# Expect: null 1 1 null 1 null null null null null

print
	null && true
	true || null
	true ^^ null
	true ^^ true
	!null
	null  || null  && true
	!true || !true && !null
	true  && null  || null
	!null && !true || !true
	!null && !true || true ^^ true
	ln

# Expect: 7 12 23 4 -19 2

print
	3 + 4
	3 * 4
	3 + 4 * 5
	3 + 4 - 2 * 3 / 6 * 3
	3 + ~4 * 5 + 1 * ~2
	~3 * ~4 % 5
	ln

# Expect: 1 null 1 null 1 null

print
	4 - 3 < 2 + 3     # 4-3 < 2+3 : 1 < 5 : true
	2 * 4 > 18 / 2    # 2*4 > 18/2 : 8 > 9 : null
	4 + 4 <= 16 / 2   # 2+4 <= 16/2 : 8 <= 8 : true
	9 / 4 >= 9 - 4    # 9/4 >= 9-4 : about 2 >= 5 : null
	15 - 5 ==  20 / 2 # 15-5 == 20/2 : 10 == 10 : true
	3 * 4  ==  3 + 4  # 3*4 == 3+4 : 12 == 7 : null
	ln

# Expect: null 1 1

print
	4 - 3 < 2 + 3     && 2 * 4 > 18 / 2
	4 + 4 <= 16 / 2   || 9 / 4 >= 9 - 4
	15 - 5 ==  20 / 2 ^^ 3 * 4  ==  3 + 4
	ln

# Whatever the ! grouping behavior is, verify it hasn't changed:
# Expect: 1

println
	! not 1

# Associativity/chaining

# Expect:
# 3
# 5
# 7
# 5
# -
# 1
# 2
# null
# null
# -
# null
# null
# -

let por = function (x)
	println x
	x

println
	por 3 - por 5 + por 7
	"-"
	por 1    && por 2 && por null
	"-"
	por null && por 1 && por 2
	"-"
