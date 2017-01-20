# Test stdout/stderr

# Expect:
# 1.0 2.0 3.0 4.0 5.0
# Next writing
# 6.07.08.0

print 1 2
stdout.print 3 4
stderr.print 10 11 12 ln # FIXME: Can this be confirmed?
print 5 ln
stdout.print "Next writing\n"
stdout.write 6 7 8 ln
stdout.flush # FIXME: Is this testable?

# Verify that a weird behavior is stable:
# Because print does not automatically blackhole, the + will be applied once and not afterward.
# Expect: 2.0 2.0 3.0 100.0
let deformedStdout = inherit stdout
	method print = function(x)
		super.print (+ x 1)

deformedStdout.print 1.0 2.0 3.0
stdout.print 100.0 ln
