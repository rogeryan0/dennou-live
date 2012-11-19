require "narray"

ndim = 10
rmiss =-999.0

x = NArray.sfloat(ndim).indgen
x[3] = rmiss
y = NArray.sfloat(ndim).indgen(10)
y[5] = rmiss

z = NArray.sfloat(ndim).fill(rmiss)
mask = x.ne(rmiss)&y.ne(rmiss)
z[mask] = x[mask]+y[mask]
p z
