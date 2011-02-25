#!/usr/bin/env ruby

##############################################################
=begin
=grads2nc.rb
GrADS to NetCDF converter

WARNING
* Not efficient if the GrADS file has many variables.

USAGE
 % grads2nc.rb infilename outfilename [-v varname1 [varname2 ...]]

EXAMPLE
 % grads2nc.rb ../testdata/T.jan.ctl tmp.nc
 % grads2nc.rb ../testdata/T.jan.ctl tmp.nc -v T
=end
##############################################################

usage = "\n\n"+<<EOS
#{$0}: GrADS to NetCDF converter.

USAGE:
  % #{$0} infilename outfilename [-v varname1 [varname2 ...]]
  Here, the -v option is used to limit variables to copy
  (all variables are copied by default). Note that to copy many
  variables may not be effifient.
EOS

# < interpret command-line arguments >

varnames = nil
if ARGV[2] == '-v'
  varnames = []
  (ARGV.length-3).times{ varnames.unshift( ARGV.pop ) }
  ARGV.pop
end

if ARGV.length != 2
  raise usage
else
  infilename, outfilename = ARGV[0..2]
end

# < convert >

require "numru/gphys"
include NumRu
grfile = GrADS_Gridded.open(infilename)
varnames = grfile.varnames if !varnames
out_file = NetCDF.create(outfilename)
varnames.each do |varname|
  temp = GPhys::GrADS_IO.open(grfile,varname)
  GPhys::NetCDF_IO.write(out_file,temp)
end
out_file.close