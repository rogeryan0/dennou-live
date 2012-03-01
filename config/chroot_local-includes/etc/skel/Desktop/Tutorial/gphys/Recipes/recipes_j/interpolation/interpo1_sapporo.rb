require "numru/ggraph"
include NumRu

#< open data / select a date for conciseness >

temp = GPhys::IO.open("air.2010.nc","air").cut(
          "time"=>Date.parse("2010-01-04")..Date.parse("2010-01-9"))

#< prepare new coordinates and interpolate >

lon = VArray.new(NArray[141.0],{"units"=>"degree_east"},"lon")
lat = VArray.new(NArray[43.0],{"units"=>"degree_north"},"lat")
tsapporo = temp.interpolate(lon,lat)[0,0,false]

#< graphics >

iws = (ARGV[0] || 1).to_i
DCL.swpset('ldump',true) if iws==4
DCL.swpset('iwidth',700)
DCL.swpset('iheight',700)
DCL.sgpset('isub', 96)      # control character of subscription: '_' --> '`'
DCL.glpset('lmiss',true)
DCL.gropn(iws)
GGraph::tone_and_contour tsapporo, true, "exchange"=>true
DCL.grcls
