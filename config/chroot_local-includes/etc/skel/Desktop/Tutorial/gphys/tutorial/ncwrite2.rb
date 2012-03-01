require "numru/gphys"
include NumRu

nlon = 36
nlat = 18

lon_a = VArray.new( NArray.sfloat(nlon).indgen(0,360.0/nlon),
                    {"long_name"=>"longitude", "units"=>"degrees_east"},
                    "lon" )
lon = Axis.new.set_pos(lon_a)

lat_a = VArray.new( NArray.sfloat(nlat).indgen(0,180/nlat),
                    {"long_name"=>"latitude","units"=>"degrees_north"},
                    "lat" )
lat = Axis.new.set_pos(lat_a)

data = VArray.new( NArray.sfloat(nlon,nlat).indgen,
                   {"long_name"=>"temperature", "units"=>"K"},
                   "T" )
gphys = GPhys.new( Grid.new(lon,lat), data )

file = NetCDF.create("tmp.nc")
GPhys::NetCDF_IO.write(file,gphys)
file.close
