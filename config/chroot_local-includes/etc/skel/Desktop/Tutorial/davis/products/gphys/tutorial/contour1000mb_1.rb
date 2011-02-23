require "numru/ggraph"
include NumRu
gphys = GPhys::IO.open('T.jan.nc', 'T')
DCL.gropn(1)
DCL.sgpset('lcntl', false) ; DCL.uzfact(0.7)
GGraph.contour( gphys )
DCL.grcls
