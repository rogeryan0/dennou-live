#!/usr/bin/env ruby
require "optparse"
require "numru/gphys"
include NumRu

opt = OptionParser.new
OPTS = {}
ARGV.options{|opt|
  opt.on( '-o=VAL', '--output=VAL',
          "Output file"
          ){|v| OPTS[:output] = v.gsub(/^=/, '')}

  opt.on( '-s=VAL', '--skip=VAL',
          "Skip interval 
                                     (ex. \"10,5,8,0\". 0 is not skipped)"
          ){|v| OPTS[:skip] = v.gsub(/^=/, '')}

  opt.on( '--copy-dims=VAL',
          "Copy dimensions additionally
                                     (ex. \"ref_time,lon_weight\")"
          ){|v| OPTS[:copy_dims] = v.gsub(/^=/, '')}

  opt.on_tail('-h', '-H', '--help', 
              "This help message is output"
              ){|v| OPTS[:help] = v}
  opt.parse!
}

if OPTS[:help] || !(OPTS[:output]) || ARGV.size < 1 || !(OPTS[:skip])
  print <<-"EOF"

  #{File.basename($0.to_s)}:

    USAGE: #{File.basename($0.to_s)} [options] input_url -o output_file -s skip_interval

    OPTION: \n#{opt.to_a[1..-1].join("")}
    EOF
    exit
end

#=== 各種設定
input_url = ARGV[0]
output_file = OPTS[:output]
skip_intervals = OPTS[:skip].split(',')
skip_intervals.collect!{|skipchar|
  skipint = skipchar.to_i
  if skipint > 0
    skipint
  else
    1
  end
}
copy_dims = []
copy_dims = OPTS[:copy_dims].split(',') if OPTS[:copy_dims]

#=== 読み込み, 書き込みファイルのオープン
gphys = GPhys::IO.open_gturl(input_url)
outfile = NetCDF.create(output_file)

#=== 大域属性コピー
gphys.data.file.each_att{|att|
  outfile.put_att(att.name, att.get)
}

#=== データの間引き (深いことは考えず)
reduced_size_array = Array.new(gphys.rank)
gphys.rank.times{ |dim_index|
  dim_size = gphys.coord(dim_index).val.size
  interval = skip_intervals[dim_index] || 1
  if dim_size < interval
    reduced_size_array[dim_index] = 0
    next
  elsif interval == 1
    reduced_size_array[dim_index] = 0..-1
    next
  end
  reduced_size = dim_size / interval
  reduced_size_array[dim_index] = Array.new(reduced_size)
  reduced_size.times{|n| reduced_size_array[dim_index][n] = n*interval}
}

gphys = gphys[*reduced_size_array]

#=== ファイル出力
if File.exist?(output_file)
  print "  *** MESSAGE *** \"#{output_file}\" is overwritten.\n"
end
GPhys::NetCDF_IO.write(outfile, gphys)

#=== 追加の次元変数書き出し
copy_dims.each{|dim|
  gphys_dim = GPhys::IO.open_gturl(input_url.sub(/@\w+$/, "@#{dim}"))
  GPhys::NetCDF_IO.write_grid(outfile, gphys_dim)
}

outfile.close
