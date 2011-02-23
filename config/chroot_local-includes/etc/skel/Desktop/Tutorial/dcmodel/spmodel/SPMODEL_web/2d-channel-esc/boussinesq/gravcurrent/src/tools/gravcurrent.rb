=begin
  $BI=Bj(B  gravcurrent.rb

  $B;H$$J}(B: ruby gravcurrent.rb 

  $BMzNr(B  2004/05/27  $B>.9b@5;L(B: gave $B$h$j:n@.(B
=end

require "numru/gphys"
require "numru/dcl"
require "colorbar" 


include NumRu
include Math

filename="gravcurrent1.nc"
varname ="rho"

# $B%+%i!<%P!<$N@_Dj(B ###
$cbar_conf = {
  "vx0"=>0.1,            # $B%+%i!<%P!<$N:82<3Q$N(B x $B:BI8(B
  "vy0"=>0.2,            # $B%+%i!<%P!<$N:82<3Q$N(B y $B:BI8(B
  "vxlength"=>0.3,       # $B%+%i!<%P!<$N(B $B2#$ND9$5(B
  "vylength"=>0.02,      # $B%+%i!<%P!<$N(B $B=D$ND9$5(B
  "tick1"  => 4,         # $BL\@9(B($B>.(B)$B$rIU$1$k4V3V(B
  "tick2"  => 20,        # $BL\@9(B($BBg(B)$B$rIU$1$k4V3V(B
  "eqlev"  => false,     # 
  "nobound"=> false      # 
  }
#######################


iws = (ARGV[0] || (puts ' WORKSTATION ID (I)  ? ;'; DCL::sgpwsn; gets)).to_i
DCL::gropn(-iws)
DCL::sldiv('T', 2, 4)

xmin = 0.0
xmax = 60.0
ymin = 0.0
ymax = 20.0

vxmin = 0.1
vxmax = 0.85
vymin = 0.3
vymax = 0.6

x_title = ""
x_unit = "x[cm]"
y_title = ""
y_unit = "y[cm]"

DCL::sglset("LCNTL", false )
DCL::sglset("LCORNER", false )
DCL::gllset("LMISS", true )
DCL::sglset('LFULL', true)

for j in [0,2,4,6,8,10,12,14]

  gphys = GPhys::NetCDF_IO.open(filename,varname)
  gphys = gphys[0..-1,0..-1,j]

  DCL::grfrm
  DCL::grswnd(xmin,xmax,ymin,ymax)
  DCL::grsvpt(vxmin,vxmax,vymin,vymax)
  DCL::grstrn(1)
  DCL::grstrf

  DCL::sglset("LCLIP", true )
  DCL::uwsgxa(gphys.coord(0).val)
  DCL::uwsgya(gphys.coord(1).val)
  DCL::ueitlv
  DCL::uegtla(0.9995, 1.0015, 0.000025)
  DCL::uetonf(gphys.val)
  DCL::ussttl(x_title, x_unit, y_title, y_unit)
  DCL::usdaxs
  DCL::sglset("LCLIP", false )
  #i=0
  #DCL::sgtxzv(vxmax+0.01,vymax-0.03*i,"t=15sec",0.02,0,-1,1)
  DCL::uzrset("ROFFXT", 0.06)
#  title = "density (g/cm3)"
#  DCL::uxsttl("t", title, 0 )

  DCL::Util::color_bar($cbar_conf)

end 

DCL::grcls

