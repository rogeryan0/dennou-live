require "numru/ggraph"
include NumRu
gphys = GPhys::IO.open('T.jan.nc', 'T')
DCL.gropn(1)
DCL.sgpset('lfull',true) ; DCL.uzfact(0.6)
GGraph.set_fig( 'itr'=> 2, 'viewport'=>[0.25,0.7,0.15,0.6] )
GGraph.set_axes( 'xside'=>'b', 'yside'=>'l' )
GGraph.line( gphys.mean(0,1), true, 'exchange'=>true, 'annot'=>false )
DCL.grcls
