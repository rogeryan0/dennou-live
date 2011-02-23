require "numru/ggraph"
include NumRu
gphys = GPhys::NetCDF_IO.open('T.jan.nc', 'T')
xlen = gphys.coord(0).length
modulo = 360
gpcyc = gphys[true, true, 0].copy
x = gpcyc.coord(0).val[[0...xlen,0]]
x[-1] += modulo
gpcyc.coord(0).val= x
#gpcyc.data = gpcyc.data[[0..xlen,0],false]
#DCL.gropn(1)
#DCL.sgpset('lcntl', false) ; DCL.uzfact(0.7)
#GGraph.contour( gpcyc )
#DCL.grcls
