require "narray"
n=100000
x = NArray.float(100000).indgen
y = NArray.float(100000).indgen(10)
p x.mul_add(y,0)/(n-1)
