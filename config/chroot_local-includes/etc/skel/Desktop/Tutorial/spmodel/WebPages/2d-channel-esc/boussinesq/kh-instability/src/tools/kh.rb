=begin
  表題  kh.rb

  使い方: ruby kh.rb 

  履歴  2004/05/27  小高正嗣: gave より作成
=end
require "numru/gphys"
require "numru/dcl"
require "colorbar" 

include NumRu


filename = "kh1.nc"
varname = "rho"

# カラーバーの設定 ###
$cbar_conf = {
  "vx0"=>0.1,            # カラーバーの左下角の x 座標
  "vy0"=>0.15,            # カラーバーの左下角の y 座標
  "vxlength"=>0.3,       # カラーバーの 横の長さ
  "vylength"=>0.02,      # カラーバーの 縦の長さ
  "tick1"  => 10,         # 目盛(小)を付ける間隔
  "tick2"  => 20,        # 目盛(大)を付ける間隔
  "eqlev"  => false,     # 
  "nobound"=> false      # 
  }
######################

iws = (ARGV[0] || (puts ' WORKSTATION ID (I)  ? ;'; DCL::sgpwsn; gets)).to_i
DCL::gropn(-iws)
DCL::sldiv('T', 2, 5)

xmin = 0.0
xmax = 17.859375
ymin = 0.0
ymax = 6.0

vxmin = 0.1
vxmax = 0.85
vymin = 0.25
vymax = 0.5

x_title = ""
x_unit = "x[cm]"
y_title = ""
y_unit = "y[cm]"

DCL::sglset("LCNTL", false )
DCL::sglset("LCORNER", false )
DCL::udlset("LMSG", false )
DCL::gllset("LMISS", true )
DCL::sglset('LFULL', true)

for j in [41, 42, 43, 44, 45, 46, 47, 48, 49, 50]

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
  DCL::uegtla(0.95, 1.01, 0.001)
  DCL::uetonf(gphys.val)
  DCL::ussttl(x_title, x_unit, y_title, y_unit)
  DCL::usdaxs
  DCL::sglset("LCLIP", false )
  i=0
  DCL::uzrset("ROFFXT", 0.06)
  title = "density (g/cm3)"
  DCL::uxsttl("t", title, 0 )
  DCL::Util::color_bar($cbar_conf)

end

DCL::grcls

