itr = ARGV[0] ? ARGV[0].to_i : 10

require "numru/ggraph"
include NumRu
gphys = GPhys::IO.open('T.jan.nc', 'T')
DCL.gropn(1)
DCL.sgpset('lcntl', false)
DCL.sgpset('lclip', true)
DCL.uzfact(0.7)
DCL.sgpset('lfull',true)
GGraph.set_fig 'itr'=>itr, 'viewport'=>[0.15,0.85,0.1,0.6]
GGraph.set_map 'coast_world'=>true
GGraph.tone( gphys )
DCL.grcls
