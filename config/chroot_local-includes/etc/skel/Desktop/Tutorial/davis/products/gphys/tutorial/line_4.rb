require "numru/ggraph"
include NumRu
gphys = GPhys::IO.open('T.jan.nc', 'T')
DCL.gropn(1)
DCL.uzfact(0.7)
GGraph.set_fig( 'itr'=> 2 )
GGraph.line( gphys.cut(135,35,false), true, 'exchange'=>true, 'annot'=>false )
DCL.grcls
