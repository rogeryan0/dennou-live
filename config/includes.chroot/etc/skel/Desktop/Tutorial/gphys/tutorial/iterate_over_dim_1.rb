require "numru/gphys"
include NumRu

gphys = GPhys::NetCDF_IO.open('T.jan.nc', 'T')

#< all at once >
print "Case 1: all at once\n"
p gphys.mean(0,1).val

#< iterate over the last dimension >
print "\nCase 2: iterated\n"
nz = gphys.axis(2).length
for i in 0...nz
  p gphys[false,i].mean(0,1).val
end
