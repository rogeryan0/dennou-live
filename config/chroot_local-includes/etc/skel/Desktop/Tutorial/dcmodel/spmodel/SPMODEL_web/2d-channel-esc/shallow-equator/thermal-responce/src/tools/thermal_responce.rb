#!/usr/bin/env ruby

=begin

title   : shallow_eqbeta_nonlinear.rb
history : 2004/07/04 YAMADA, YUKIKO

usage   : ruby shallow_eqbeta_nonlinear.rb

pstoeps : $ dclpsrmcm dcl.ps > tmp.ps
          $ eps2eps tmp.ps thermal_responce.eps
          $ rm tmp.ps dcl.ps
=end 


# ----------------------------------------------
# 必要なライブラリ, モジュールの読み込み
require "numru/ggraph"
require "numru/dcl"
require "colorbar" 
include NumRu

# ----------------------------------------------
# ファイル名, 変数の読み込み

filename = "thermal_responce.nc"
varname = "height"
gphys = GPhys::NetCDF_IO.open(filename,varname)


# ----------------------------------------------
# 窓開く

iws = (ARGV[0] || (puts ' WORKSTATION ID (I)  ? ;'; DCL::sgpwsn; gets)).to_i
DCL::gropn(-iws)


# -----------------------------------------------------
# お絵描き前の各種設定

# ビューポート
vxmin = 0.1   ; vxmax = 0.9
vymin = 0.045 ; vymax = 0.23
GGraph.set_fig('viewport'=>[vxmin,vxmax,vymin,vymax])

# 軸のタイトル単位を表示しない
GGraph.set_axes('xtitle'=>'','ytitle'=>'','xunits'=>'','yunits'=>'')


DCL::sldiv('T', 1, 6)          # 画面 6 分割
DCL::sglset("LCORNER", false ) # コーナマークはつけない
DCL::sglset("LCNTL", false )   # アンダーバーを表示
DCL::udlset("LMSG", false )    # グラフの下部マージンにメッセージを書かない 
DCL::sglset('LFULL', true)     # 前画面表示
DCL.uzfact(0.9)                # 座標軸の文字列サイズを定数倍
DCL.sgpset('lfprop',true)      # プロポーショナルフォントを使う
DCL.uzpset('labelxb',false)    # x 軸を表示しない


# -----------------------------------------------------
# カラー, トーンの設定

levels   = NArray[-1,-0.012,-0.006,-0.004,0.0,0.004,0.006,1]
patterns = NArray[30999, 35999, 40999, 55999, 70999, 75999, 85999]

$tone_hash = { 'patterns'=> patterns, 'levels'=>levels,'title'=>nil }
$cont_hash = { 'int' => 0.002, 'title'=>nil }

# カラーバーの設定
$cbar_conf = {
  "vx0"=>0.1,           # カラーバーの左下角の x 座標
  "vy0"=>0.15,          # カラーバーの左下角の y 座標
  "vxlength"=>0.7,      # カラーバーの 横の長さ
  "vylength"=>0.03,     # カラーバーの 縦の長さ
  "levels"=>levels, 
  "colors"=>patterns,
  "eqlev"=>true, 
  "nobound"=>0, 
  "tick1"=>7,"tick2"=>1,
  "label_size"=>0.013
  }


# -----------------------------------------------------
# お絵描きメイン

# タイトル
DCL::grfrm
DCL::sgtxzv(0.5,0.1,'Height', 1.15*DCL.uzpget('rsizec2'),0,0,3)


# 実験結果図 4 枚
4.times{ |num|
  DCL.uzpset('labelxb',true) if num == 3   # 最下段のみ x 軸表示 
  GGraph.tone( gphys[true,true,(num+1)*10],true, $tone_hash) 
  GGraph.contour( gphys[true,true,(num+1)*10], false, $cont_hash)
}

# カラーバー
DCL::grfrm
DCL::Util::color_bar($cbar_conf)


# お片付け
DCL::grcls






