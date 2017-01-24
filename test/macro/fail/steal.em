# Make sure stealing only works across newlines, not commas
# FIXME: Rewrite this test to use --ast2 instead of using execute success/fail
# Expect failure

if 1 (3), else (4)
