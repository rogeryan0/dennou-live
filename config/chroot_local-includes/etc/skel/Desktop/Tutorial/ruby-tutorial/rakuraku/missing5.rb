require "numru/dcl"
require "narray_miss"

include NumRu

rmiss = -9.99e10

nt = 50
nz = 50
tmin, tmax = 0.0, 5.0
zmin, zmax = 20.0,50.0
t = NArray.sfloat(nt+1,   1).indgen! * (tmax-tmin)/nt
z = NArray.sfloat(   1,nz+1).indgen! * (zmax-zmin)/nz
t[10..12,0] = rmiss
z[0,5..10] = rmiss
t = NArrayMiss.to_nam(t,t.ne(rmiss))
z = NArrayMiss.to_nam(z,z.ne(rmiss))
uz = NMMath::exp(-0.2*z)*(z**0.5)
tz = -2.0*NMMath::exp(-0.1*z)
u = uz*NMMath::sin(3.0*(tz+t))

DCL::gropn(1)
DCL::gllset("lmiss",true)
DCL::glrset("rmiss",rmiss)
DCL::grfrm
DCL::grswnd(tmin, tmax, zmin, zmax)
DCL::uspfit
DCL::grstrf
DCL::usdaxs
DCL::udcntr(u.to_na(rmiss))
DCL::grcls
