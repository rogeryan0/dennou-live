itr = ARGV[0] ? ARGV[0].to_i : 10

require "numru/ggraph"
include NumRu
gphys = GPhys::IO.open('T.jan.nc', 'T')
DCL.gropn(1)
DCL.sgpset('lcntl', false)
DCL.uzfact(0.7)
DCL.sgpset('lfull',true)
GGraph.set_fig 'itr'=>itr, 'viewport'=>[0.15,0.85,0.1,0.6],
               'window'=>[60,180,20,70], 'map_window'=>[60,180,20,70]
GGraph.set_map 'coast_world'=>true
GGraph.tone( gphys.cut(60..180,20..70,false) )
DCL.grcls
