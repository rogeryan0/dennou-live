require "numru/ggraph"
include NumRu
gphys = GPhys::IO.open('T.jan.nc', 'T')
DCL.gropn(1)
DCL.sgpset('lcntl', false)
DCL.sgpset('lclip', true)
DCL.uzfact(0.7)
DCL.sgpset('lfull',true)
GGraph.set_fig 'itr'=>10, 'viewport'=>[0.15,0.85,0.1,0.6]
GGraph.set_map 'coast_japan'=>true
GGraph.tone( gphys.cut('lon'=>120..150,'lat'=>20..50), true, 'map_axes'=>true )
DCL.grcls
