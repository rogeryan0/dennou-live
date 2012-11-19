# Derive the 30-year climatology from 1981 to 2010 from 
# NCEP/NCEP2's monthly data.

require "numru/gphys"
include NumRu

var = ARGV[0] || "air"
dir = ARGV[1] || "."

f = NetCDF.open "#{dir}/#{var}.mon.mean.nc"
gp = GPhys::IO.open(f,var)

ts = Date.new(1981,1,1)
te = Date.new(2010,12,31)
gp = gp.cut("time"=>ts..te)
gp12 = gp[false,0..11].copy
for mon in 0..11
  gp12[false,mon] = gp[false,{mon..-1=>12}].mean(-1)
end

time = gp12.coord(-1)
time.put_att("avg_period","0030-00-00 00:00:00")
time.put_att("ltm_range", NArray[time[0].val, time[-1].val])
["axis","coordinate_defines","bounds","prev_avg_period"].each do |s|
  time.del_att(s)
end

ofl = NetCDF.create "#{dir}/#{var}.clim.1981-2010.nc"
f.each_att{|a| ofl.put_att(a.name,a.get)}
GPhys::IO.write(ofl,gp12)
ofl.close
