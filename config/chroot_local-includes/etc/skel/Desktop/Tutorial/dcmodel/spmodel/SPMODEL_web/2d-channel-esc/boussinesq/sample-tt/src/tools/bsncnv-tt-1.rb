=begin
表題   bsncnv-tt-1.rb

使い方 ruby bsncnv-tt-1.rb

履歴   2004/05/28  小高正嗣: gave より作成
=end 
require "numru/gphys"
require "numru/dcl"
require "colorbar" 

include NumRu


filename = "bsncnv-tt-1.nc"
varname = "temp"
Temp = GPhys::NetCDF_IO.open(filename,varname)
Temp = Temp[0..-1,0..-1,20]

varname = "psi"
Psi = GPhys::NetCDF_IO.open(filename,varname)
Psi = Psi[0..-1,0..-1,20]

iws = (ARGV[0] || (puts ' WORKSTATION ID (I)  ? ;'; DCL::sgpwsn; gets)).to_i
DCL::gropn(iws)

xmin = 0.0
xmax = 1.96875
ymin = 0.0
ymax = 1.0

vxmin = 0.2
vxmax = 0.8
vymin = 0.6
vymax = 0.9

# カラーバーの設定 ###################################################
$cbar_conf = {
  "vx0"=>0.1,            # カラーバーの左下角の x 座標
  "vy0"=>0.52,            # カラーバーの左下角の y 座標
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
i=0
DCL::sgtxzv(vxmax+0.01,vymax-0.03*i,"t=0.5",0.02,0,-1,1)
DCL::uzrset("ROFFXT", 0.06)
title = "T"
#DCL::uxsttl("t", title, 0 )

DCL::Util::color_bar($cbar_conf)

DCL::grfig

vxmin = 0.2
vxmax = 0.8
vymin = 0.15
vymax = 0.45

# カラーバーの設定 ###################################################
$cbar_conf = {
  "vx0"=>0.1,            # カラーバーの左下角の x 座標
  "vy0"=>0.08,            # カラーバーの左下角の y 座標
  "vxlength"=>0.3,      # カラーバーの 横の長さ
  "vylength"=>0.02,      # カラーバーの 縦の長さ
  "tick1"  => 1,         # 目盛(大)を付ける間隔
  "tick2"  => 2,        # 目盛(小)を付ける間隔
  "eqlev"  => false,     # 
  "nobound"=> false      # 
  }
#########################################################################


DCL::grswnd(xmin,xmax,ymin,ymax)
DCL::grsvpt(vxmin,vxmax,vymin,vymax)
DCL::grstrn(1)
DCL::grstrf

DCL::sglset("LCLIP", true )
DCL::uwsgxa(Psi.coord(0).val)
DCL::uwsgya(Psi.coord(1).val)
DCL::ueitlv
DCL::uegtla(-18.3568261, 18.3568261, 0 )
DCL::uetonf(Psi.val)
DCL::udgcla(-18.3568261, 18.3568261, 0 )
DCL::udcntz(Psi.val)
DCL::ussttl(x_title, x_unit, y_title, y_unit)
DCL::usdaxs
DCL::sglset("LCLIP", false )
i=0
DCL::sgtxzv(vxmax+0.01,vymax-0.03*i,"t=0.5",0.02,0,-1,1)
DCL::uzrset("ROFFXT", 0.06)
title = "Stream function"
#DCL::uxsttl("t", title, 0 )

DCL::Util::color_bar($cbar_conf)



DCL::grcls

