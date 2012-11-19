require "drb/drb"
DRb.start_service
uri = ARGV.shift || raise("Usage: % #{$0} uri")
gp = DRbObject.new(nil, uri)
p gp.class
p gp.name
p gp.rank
p gp.shape
print gp.coord(2).name,"\n"
