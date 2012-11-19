require "numru/gphys"
include NumRu
gp = GPhys::IO.open("T.jan.nc","T")
p gp.class
p gp.name
p gp.rank
p gp.shape
print gp.coord(2).name,"\n"
