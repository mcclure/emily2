# Test dict ops

# Expect:
# null
# null
# 1.0
# 5.0
# 1.0
# word
# null
# null

let d = new Dict

println
	d.has "ok"
	d.has 3

d.set "ok" 5
d.set 3 "word"

println
	d.has "ok"
	d.get "ok"
	d.has 3
	d.get 3

d.del "ok"
d.del 3

println
	d.has "ok"
	d.has 3
