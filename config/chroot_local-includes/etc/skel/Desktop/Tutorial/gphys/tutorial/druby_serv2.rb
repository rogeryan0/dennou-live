require "drb/drb"
require "numru/ggraph"
include NumRu

class NArray
  def self._load(o); to_na(*Marshal::load(o)).ntoh; end
  def _dump(limit); Marshal::dump([hton.to_s, typecode, *shape]); end
end

gp = GPhys::IO.open("T.jan.nc","T")
DRb.start_service(nil, gp)
puts 'URI: '+DRb.uri
puts '[return] to exit'
gets
