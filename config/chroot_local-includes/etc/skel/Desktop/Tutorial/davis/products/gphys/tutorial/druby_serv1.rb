require "drb/drb"
require "numru/gphys"
include NumRu
gp = GPhys::IO.open("T.jan.nc","T")
DRb.start_service(nil, gp)
puts 'URI: '+DRb.uri
puts '[return] to exit'
gets
