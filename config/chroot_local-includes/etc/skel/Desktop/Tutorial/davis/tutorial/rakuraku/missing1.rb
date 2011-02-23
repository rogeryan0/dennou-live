require "narray"

ndim = 10
rmiss = -999.0

x = NArray.sfloat(ndim).indgen
x[3] = rmiss
y = NArray.sfloat(ndim).indgen(10)
y[5] = rmiss

z = NArray.sfloat(ndim)
for n in 0..ndim-1
  if x[n]!=rmiss && y[n]!=rmiss then
     z[n] = x[n]+y[n]
  else
     z[n] = rmiss
  end
end
p z
