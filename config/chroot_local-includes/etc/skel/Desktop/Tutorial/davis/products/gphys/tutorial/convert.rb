require "numru/gphys"
include NumRu

usage = "\nUSAGE:\n % #{$0} in_file out_file\n"
ifpath = ARGV.shift || raise(usage)
ofpath = ARGV.shift || raise(usage)

raise "File #{ofpath} present. Delete it if needed." if File.exist?(ofpath)
case ofpath
when /\.nc$/
  ofile = NetCDF.create(ofpath)
when /\.grib$/
  ofile = Grib.create(ofpath)
when /\.ctl$/
  ofile = GrADS_Gridded.create(ofpath)
else
  raise "unsupported file type (judged by suffix): "+ofpath
end

GPhys::IO.var_names_except_coordinates( ifpath ).each do |varname|
  GPhys::IO.write(ofile, GPhys::IO.open(ifpath, varname) )
end

ofile.close
