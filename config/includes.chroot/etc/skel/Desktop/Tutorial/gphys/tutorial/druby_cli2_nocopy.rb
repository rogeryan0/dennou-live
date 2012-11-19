require "drb/drb"
require "numru/ggraph"
include NumRu

class NArray
  def self._load(o) to_na(*Marshal::load(o)).ntoh end
end

DRb.start_service
uri = ARGV.shift || raise("Usage: % #{$0} uri")
gp = DRbObject.new(nil, uri)
DCL.gropn(1)
DCL.sldiv('y',2,1)
DCL.sgpset('lcntl',false)
DCL.uzfact(0.7)
GGraph.set_fig('viewport'=>[0.15,0.75,0.2,0.8])
GGraph.contour( gp.cut('level'=>100) )
GGraph.next_fig('itr'=>2)
GGraph.contour( gp.mean(0) )
DCL.grcls
