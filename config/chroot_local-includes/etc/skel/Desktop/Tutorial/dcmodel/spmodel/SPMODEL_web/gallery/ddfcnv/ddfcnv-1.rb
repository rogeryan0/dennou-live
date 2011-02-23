=begin
表題   ddfcnv-1.rb

使い方 ruby ddfcnv-1.rb

履歴   2004/06/03  小高正嗣: gave より作成
=end 
require "numru/gphys"
require "numru/dcl"
require "colorbar" 

include NumRu

filename = "ddfcnv_0.nc"

iws = (ARGV[0] || (puts ' WORKSTATION ID (I)  ? ;'; DCL::sgpwsn; gets)).to_i

xmin = 0.0
xmax = 1.96875
ymin = 0.0
ymax = 1.0

vxmin = 0.2
vxmax = 0.8
vymin = 0.25
vymax = 0.55

# カラーバーの設定 ###################################################
$cbar_conf = {
  "vx0"=>0.05,            # カラーバーの左下角の x 座標
  "vy0"=>0.10,            # カラーバーの左下角の y 座標
  "vxlength"=>0.2,      # カラーバーの 横の長さ
  "vylength"=>0.02,      # カラーバーの 縦の長さ
  "tick1"  => 1,         # 目盛(大)を付ける間隔
  "tick2"  => 2,        # 目盛(小)を付ける間隔
  "eqlev"  => false,     # 
  "nobound"=> false      # 
  }
#########################################################################

x_title = "X-coordinate"
x_unit = "1"
y_title = "Y-coordinate"
y_unit = "1"

DCL::sglset("LCORNER", false )    # コーナーの図を書かない
DCL::sglset("LCNTL", false )      # 制御文字を有効にする
DCL::sglset('LFULL', true)        # 画面いっぱいに描画
DCL::gllset("LMISS", true )       # 欠損処理
DCL::udrset("RSIZET", 0.015)      # コンターメッセージ文字のサイズ
#DCL::swlset('LDUMP', true)        # ダンプイメージを作成

DCL::gropn(iws)

for j in 0..20

  varname = "comp"
  Comp = GPhys::NetCDF_IO.open(filename,varname)
  Comp = Comp[0..-1,0..-1,j]

  DCL::grfrm
  DCL::grswnd(xmin,xmax,ymin,ymax)
  DCL::grsvpt(vxmin,vxmax,vymin,vymax)
  DCL::grstrn(1)
  DCL::grstrf

  DCL::sglset("LCLIP", true )
  DCL::uwsgxa(Comp.coord(0).val)
  DCL::uwsgya(Comp.coord(1).val)
  DCL::ussttl(x_title, x_unit, y_title, y_unit)
  DCL::ueitlv
  DCL::uegtla(0.0, 1.0, 0 )
  DCL::uetonf(Comp.val)
  DCL::usdaxs
  DCL::udgcla(0.0, 1.0, 0 )
  DCL::udcntz(Comp.val)
  DCL::sglset("LCLIP", false )
  DCL::uzrset("ROFFXT", 0.06)
  title = "Salinity [1]"
  DCL::uxsttl("t", title, 0 )
  DCL::Util::color_bar($cbar_conf)


end

DCL::grcls

