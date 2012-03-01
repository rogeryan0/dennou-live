require "numru/ggraph"
include NumRu

#< interpret command-line arguments > 

iws = (ARGV[0] || 1).to_i

#< open data > 

temp = GPhys::IO.open("air.2010.nc","air")[false,2..-1,{0..20,10}]
uwnd = GPhys::IO.open("uwnd.2010.nc","uwnd")[false,2..-1,{0..20,10}]

#< calculate potential temperature (theta) > 

prs = temp.axis("level").to_gphys
p00 =  UNumeric[1000.0, "millibar"]
kappa = 2.0 / 7.0
pfact = (prs/p00)**(-kappa)
theta = temp * pfact
theta.name = "theta"
theta.long_name = "potential temperature"

#< set theta as an associated coordinate >

uwnd.set_assoc_coords([theta])
p "uwnd:", uwnd

#< prepare a theta coordinate variable >

tht_crd = VArray.new( NArray[300.0,350.0, 400.0, 500.0, 700.0, 800.0], 
                      {"units"=>"K"}, "theta")

#< transform the vertical coordinate to theta >

uwnd_ontht = uwnd.interpolate("level"=>tht_crd)

#< graphics >

DCL.swpset('iwidth',800)
DCL.swpset('iheight',400)
DCL.swpset('ldump',true) if iws==4
DCL.gropn(iws)
DCL.sldiv('y',2,1)
DCL.sgpset('isub', 96)      # control character of subscription: '_' --> '`'
DCL.glpset('lmiss',true)

GGraph::set_fig "itr"=>2,"viewport"=>[0.16,0.73,0.2,0.8]
GGraph::tone_and_contour uwnd.mean(0),true
GGraph::color_bar

GGraph::set_fig "itr"=>1
GGraph::tone_and_contour uwnd_ontht.mean(0),true
GGraph::color_bar

DCL.grcls
