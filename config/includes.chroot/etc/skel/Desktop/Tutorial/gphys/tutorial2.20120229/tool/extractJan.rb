# Extract data only of January from NCEP/NCEP2's daily data

require "numru/gphys"
include NumRu
var = ARGV[0] || "air"
year = ARGV[1] || "2012"
dir = ARGV[2] || "."
f = NetCDF.open "#{dir}/#{var}.#{year}.nc"
gp = GPhys::IO.open(f,var)[false,0..30]
ofl = NetCDF.create "#{dir}/#{var}.#{year}-01.nc"
f.each_att{|a| ofl.put_att(a.name,a.get)}
GPhys::IO.write(ofl,gp)
ofl.close
