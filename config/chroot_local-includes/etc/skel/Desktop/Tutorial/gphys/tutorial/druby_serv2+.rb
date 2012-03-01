require "drb/drb"
require "numru/gphys"
include NumRu

def usage
  print <<-EOS
  USAGE:
    % ruby #{$0} [port]

    Here, port (optional) is the port number to assign (integer).
  EOS
  raise RuntimeError
end

port = ARGV.shift
usage if port && port.to_i.to_s != port    # 数字であることを確かめる
usage if ARGV.length > 0                   # --> 引数２個以上ならエラー

class NArray
  DUMP_SIZE_LIMIT = 40000
  def self._load(o) to_na(*Marshal::load(o)).ntoh end
  def _dump(limit) 
    if size <= DUMP_SIZE_LIMIT
      Marshal::dump([hton.to_s, typecode, *shape]) 
    else
      raise "size of the NArray (#{size}) is too large to dump "+
            "(limit: #{DUMP_SIZE_LIMIT})"
    end
  end
end

gp = GPhys::IO.open("T.jan.nc","T")

uri_seed = ( port ? 'druby://:'+port : nil )
DRb.start_service(uri_seed, gp)
puts 'URI: '+DRb.uri
puts '[return] to exit'
gets
