require "numru/netcdf"
require "numru/dcl"
require "narray_miss"
include NumRu

file = NetCDF.open("./u.199901.nc")
ntime = file.dim("time").length
lon = file.var("lon").get
lat = file.var("lat").get
u = file.var("dat").get
rmiss = file.var("dat").att("missing_value").get[0]
file.close

if rmiss<0 then
  mask = u.gt(rmiss/2)
else
  mask = u.lt(rmiss/2)
end
u = NArrayMiss.to_nam(u,mask)
mean = u.mean(2,"min_count"=>ntime*0.7)

DCL::gropn(1)
DCL::gllset("lmiss",true)
DCL::glrset("rmiss",rmiss)

DCL::grfrm
DCL::grstrn(10)
DCL::grswnd(0.0,360.0,-90.0,90.0)
DCL::umpfit
DCL::grstrf
DCL::uwsgxa(lon)
DCL::uwsgya(lat)
DCL::uelset("ltone",true)
DCL::uetone(mean.to_na(rmiss))
DCL::umpmap("coast_world")
DCL::umplim

DCL::grcls
