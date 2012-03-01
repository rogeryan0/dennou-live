require "numru/ggraph"
include NumRu
file = NetCDF.open("uwnd.mon.ltm.nc")
uwnd = GPhys::NetCDF_IO.open(file,"uwnd")
DCL.uzfact(0.6)
GGraph.open(1)
GGraph.contour(uwnd.mean(0))
GGraph.contour(uwnd[true,true,5,0])
GGraph.close
