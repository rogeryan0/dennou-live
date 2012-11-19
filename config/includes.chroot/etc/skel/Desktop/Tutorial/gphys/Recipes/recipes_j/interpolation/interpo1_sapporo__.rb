require "numru/ggraph"
include NumRu

#< open data / select a date for conciseness >

temp = GPhys::IO.open("air.2010.nc","air").cut(
          "time"=>Date.parse("2010-01-04")..Date.parse("2010-01-9"))

#< interpolate >

tsapporo = temp.interpolate({"lon"=>141.0,"dummy"=>nil},{"lat"=>43})[0,0,false]

  # # in a future revision, it will be sufficient to call as
  # tsapporo = temp.interpolate({"lon"=>141.0,"lat"=>43})[0,0,false]

#< graphics >

DCL.swpset('iwidth',700)
DCL.swpset('iheight',700)
DCL.sgpset('isub', 96)      # control character of subscription: '_' --> '`'
DCL.glpset('lmiss',true)
DCL.gropn(1)
GGraph::tone_and_contour tsapporo, true, "exchange"=>true
DCL.grcls
