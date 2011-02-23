require "numru/gphys"
include NumRu
gphys = GPhys::NetCDF_IO.open('T.jan.nc', 'T')
outfile = NetCDF.create('tmp.nc')
GPhys::NetCDF_IO.write( outfile, gphys.cut('level'=>1000..250).mean(0) )
GPhys::NetCDF_IO.write( outfile, gphys.cut('level'=>1000..250).mean(0,1).rename('T00') )
outfile.close
