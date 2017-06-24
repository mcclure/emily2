# One trailing comma at the end of a line may be forgiven

# Arg: --ast
# Expect: (a, a, a, b, b, b, , c, c, c)

a, a, a,
b, b, b,, # FIXME: Wait, this feels unexpected?
c, c
c,