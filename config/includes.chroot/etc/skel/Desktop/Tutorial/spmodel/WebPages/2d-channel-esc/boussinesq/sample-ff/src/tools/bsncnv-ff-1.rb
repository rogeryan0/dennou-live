=begin
表題   bsncnv-ff-1.rb

使い方 ruby bsncnv-ff-1.rb

履歴   2004/06/01  小高正嗣: gave より作成
履歴   2004/06/17  小高正嗣
=end 
require "numru/gphys"
require "numru/dcl"
require "colorbar" 

include NumRu


filename = "bsncnv-ff-1.nc"

iws = (ARGV[0] || (puts ' WORKSTATION ID (I)  ? ;'; DCL::sgpwsn; gets)).to_i
DCL::gropn(-iws)
DCL::sldiv('Y', 2, 5)

xmin = 0.0
xmax = 7.9375
ymin = 0.0
ymax = 1.0

vxmin = 0.1
vxmax = 0.9
vymin = 0.35
vymax = 0.45

# カラーバーの設定 ###################################################
$cbar_conf = {
  "vx0"=>0.1,            # カラーバーの左下角の x 座標
  "vy0"=>0.25,            # カラーバーの左下角の y 座標
  "vxlength"=>0.3,      # カラーバーの 横の長さ
  "vylength"=>0.02,      # カラーバーの 縦の長さ
  "tick1"  => 1,         # 目盛(大)を付ける間隔
  "tick2"  => 2,        # 目盛(小)を付ける間隔
  "eqlev"  => false,     # 
  "nobound"=> false      # 
  }
#########################################################################

x_title = ""
x_unit = "x"
y_title = ""
y_unit = "y"

DCL::sglset("LCORNER", false )
DCL::sglset("LCNTL", false )
DCL::udlset("LMSG", false )
DCL::gllset("LMISS", true )
DCL::sglset('LFULL', true)

for j in [2, 6, 10, 14, 18]

  varname = "temp"
  Temp = GPhys::NetCDF_IO.open(filename,varname)
  Temp = Temp[0..-1,0..-1,j]

  DCL::grfrm
  DCL::grswnd(xmin,xmax,ymin,ymax)
  DCL::grsvpt(vxmin,vxmax,vymin,vymax)
  DCL::grstrn(1)
  DCL::grstrf

  DCL::sglset("LCLIP", true )
  DCL::uwsgxa(Temp.coord(0).val)
  DCL::uwsgya(Temp.coord(1).val)
  DCL::ueitlv
  DCL::uegtla(0.0, 1.0, 0 )
  DCL::uetonf(Temp.val)
  DCL::udgcla(0.0, 1.0, 0 )
  DCL::udcntz(Temp.val)
  DCL::ussttl(x_title, x_unit, y_title, y_unit)
  DCL::usdaxs
  DCL::sglset("LCLIP", false )
  DCL::uzrset("ROFFXT", 0.06)
  DCL::Util::color_bar($cbar_conf)

# カラーバーの設定 ###################################################
$cbar_conf = {
  "vx0"=>0.1,            # カラーバーの左下角の x 座標
  "vy0"=>0.25,            # カラーバーの左下角の y 座標
  "vxlength"=>0.3,      # カラーバーの 横の長さ
  "vylength"=>0.02,      # カラーバーの 縦の長さ
  "tick1"  => 1,         # 目盛(大)を付ける間隔
  "tick2"  => 2,        # 目盛(小)を付ける間隔
  "eqlev"  => false,     # 
  "nobound"=> false      # 
  }
#########################################################################

  varname = "psi"
  Psi = GPhys::NetCDF_IO.open(filename,varname)
  Psi = Psi[0..-1,0..-1,j]

  DCL::grfrm
  DCL::grswnd(xmin,xmax,ymin,ymax)
  DCL::grsvpt(vxmin,vxmax,vymin,vymax)
  DCL::grstrn(1)
  DCL::grstrf

  DCL::sglset("LCLIP", true )
  DCL::uwsgxa(Psi.coord(0).val)
  DCL::uwsgya(Psi.coord(1).val)
  DCL::ueitlv
  DCL::uegtla(-12.86741061, 12.86741061, 0 )
  DCL::uetonf(Psi.val)
  DCL::udgcla(-12.86741061, 12.86741061, 0 )
  DCL::udcntz(Psi.val)
  DCL::ussttl(x_title, x_unit, y_title, y_unit)
  DCL::usdaxs
  DCL::sglset("LCLIP", false )
  DCL::uzrset("ROFFXT", 0.06)
  DCL::Util::color_bar($cbar_conf)

end

DCL::grcls

