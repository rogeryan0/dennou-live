require "numru/ggraph"
include NumRu
gphys = GPhys::IO.open('T.jan.nc', 'T')
DCL.gropn(1)
DCL.sgpset('isub', 96)   # control character of subscription: '_' --> '`'
DCL.sgpset('lfull',true) ; DCL.uzfact(0.6)
GGraph.set_fig( 'itr'=> 2, 'viewport'=>[0.25,0.7,0.15,0.6] )
GGraph.line( gphys.cut(135,35,false), true, 'exchange'=>true )
DCL.grcls
