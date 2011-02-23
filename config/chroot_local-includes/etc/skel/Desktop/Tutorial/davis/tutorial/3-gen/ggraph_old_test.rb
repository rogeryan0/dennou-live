require "numru/ggraph"
include NumRu
file = NetCDF.open("../testdata/T.jan.nc")
temp = GPhys::NetCDF_IO.open(file,"T")
DCL.uzfact(0.6)
GGraph.open(1)
GGraph.contour(temp.mean(0))
GGraph.contour(temp[true,true,5])
GGraph.close
