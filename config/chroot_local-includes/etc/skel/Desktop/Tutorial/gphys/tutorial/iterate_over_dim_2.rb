require "numru/gphys"
include NumRu
gphys = GPhys::NetCDF_IO.open('T.jan.nc', 'T')

# < all at once >
ofile1 = NetCDF.create('tmp1.nc')
gp10 = gphys*10
GPhys::NetCDF_IO.write(ofile1, gp10)
ofile1.close

#< iterate over the last dimension >
ofile2 = NetCDF.create('tmp2.nc')
GPhys::NetCDF_IO.each_along_dims_write(gphys, ofile2, -1) do |sub|
  sub10 = sub*10
  [sub10]
end
ofile2.close
