require "numru/ggraph"

iws = (ARGV[0] || 1).to_i

module NumRu
  class GPhys
    def variance(*dims)
      dev = self - self.mean(*dims)
      variance = (dev**2).mean(*dims)
      variance.long_name = "variance: "+long_name if variance.is_a?(GPhys)
      variance
    end
  end
end

include NumRu

u = GPhys::IO.open("uwnd.2010.nc","uwnd")[false,{0..30,3}]  # Jan 1,4,..,31

p "variance (all data points):",u.variance

DCL.swpset('iwidth',900)
DCL.swpset('iheight',450)
DCL.swpset('ldump',true) if iws==4
DCL.gropn(iws)
DCL.sldiv('y',2,1)
DCL.sgpset('isub', 96)      # control character of subscription: '_' --> '`'
DCL.glpset('lmiss',true)

GGraph.set_fig "itr"=>2, "viewport"=>[0.15,0.75,0.2,0.8]
GGraph.tone u.variance(0).mean(-1), true, "int"=>50
GGraph.color_bar

GGraph.set_fig "itr"=>10, "viewport"=>[0.05,0.8,0.2,0.8]
GGraph.set_map "coast_world"=>true
GGraph.tone u.cut("level"=>850..200).variance("level").mean("time")
GGraph.color_bar

DCL.grcls
