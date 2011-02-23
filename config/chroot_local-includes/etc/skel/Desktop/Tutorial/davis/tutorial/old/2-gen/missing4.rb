require "narray_miss"

ndim = 10

x = NArrayMiss.sfloat(ndim).indgen!
x.invalidation(3)

for method in ["sum","min","max","mean","stddev"]
  str = "print \"#{method} = \",x.#{method},\"\n\""  # print "sum = ",x.sum,"\n"
  eval(str)
end
