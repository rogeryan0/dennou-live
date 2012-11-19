require "numru/ggraph"
include NumRu

#< open data / select a date for conciseness >

temp = GPhys::IO.open("air.2010.nc","air")[false,3..8]  # 3..8 => Jan 4-9

#< prepare new coordinates and interpolate >

lon = VArray.new( NArray.float(5).indgen!+135, {"units"=>"degree_east"},"lon")
lat = VArray.new( NArray.float(5).indgen!+34, {"units"=>"degree_north"},"lat")
t_grid = temp.interpolate(lon,lat)
t_slice = temp.interpolate([lon,lat])

#< print >
p "t_grid", t_grid
p "t_slice", t_slice

#< graphics >

iws = (ARGV[0] || 1).to_i
DCL.swpset('ldump',true) if iws==4
DCL.swpset('iwidth',800)
DCL.swpset('iheight',400)
DCL.sgpset('isub', 96)      # control character of subscription: '_' --> '`'
DCL.glpset('lmiss',true)
DCL.gropn(iws)
DCL.sldiv('y',2,1)
GGraph.set_fig "itr"=>10
GGraph.set_map "coast_japan"=>true
GGraph.tone_and_contour t_grid
GGraph.set_fig "itr"=>1
GGraph.tone_and_contour t_slice.cut('level'=>850)
DCL.grcls
