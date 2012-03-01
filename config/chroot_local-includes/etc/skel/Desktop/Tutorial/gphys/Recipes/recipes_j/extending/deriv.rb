require "numru/ggraph"
iws = (ARGV[0] || 1).to_i

module NumRu
  class GPhys
    def deriv(dim)
      dim = dim_index(dim)    # input dim can be a String or an Integer
      x = axis(dim).to_gphys
      a = [true]*dim + [1..-1,false]
      b = [true]*dim + [0..-2,false]
      dydx = ( self[*a] - self[*b] ) / ( x[1..-1] - x[0..-2] )
      xi = ( x[1..-1] + x[0..-2] ) / 2
      dydx.axis(dim).set_pos(xi.coord(0))
      dydx.long_name = "d #{name}/d #{x.name}"
      dydx
    end
  end
end

include NumRu

u = GPhys::IO.open("uwnd.2010.nc","uwnd")[false,0].cut("lon"=>140)

ushear_p = u.deriv("level")

H = UNumeric[8e3,"m"]
z = -H * ( u.axis("level").pos / 1000.0 ).log
z.name = "z"
z.long_name = "log-p height"
z.set_att("positive","up")
u.axis("level").set_pos(z)

ushear_z = u.deriv(-1)

DCL.swpset('iwidth',800)
DCL.swpset('iheight',400)
DCL.swpset('ldump',true) if iws==4
DCL.gropn(iws)
DCL.sldiv('y',2,1)
DCL.sgpset('isub', 96)      # control character of subscription: '_' --> '`'
DCL.glpset('lmiss',true)
GGraph.set_fig "itr"=>2, "viewport"=>[0.15,0.75,0.2,0.8]
GGraph.tone_and_contour ushear_p
GGraph.color_bar
GGraph.set_fig "itr"=>1
GGraph.tone_and_contour ushear_z
GGraph.color_bar
DCL.grcls
