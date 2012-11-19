require "narray_miss"

ndim = 10

x = NArrayMiss.sfloat(ndim).indgen!
x.invalidation(3)
y = NArrayMiss.sfloat(ndim).indgen!(10)
y.invalidation(5)

z = x+y
p z
