require "narray"
n=100000
x = NArray.float(100000).indgen
y = NArray.float(100000).indgen(10)
sum = 0
for i in 0...n
  sum += x[i]*y[i]
end
p sum/(n-1)
