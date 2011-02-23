require "numru/dcl"

include NumRu
include NMath                        # NArrayのMathモジュール

nt = 50
nz = 50
tmin, tmax = 0.0, 5.0
zmin, zmax = 20.0,50.0

t = NArray.sfloat(nt+1,   1).indgen! * (tmax-tmin)/nt # 横軸 (nt+1,1)配列
z = NArray.sfloat(   1,nz+1).indgen! * (zmax-zmin)/nz # 縦軸 (1,nz+1)配列

uz = exp(-0.2*z)*(z**0.5)
tz = -2.0*exp(-0.1*z)
u = uz*sin(3.0*(tz+t))               # (nt+1,nz+1)の配列になる。

p "umin,umax="
p u.min, u.max

=begin
コマンドラインオプションに-psが含まれていればPSファイルを作る(gropn(2))、
なければウィンドウを開く(gropn(1))
=end
if ARGV.index("-ps")
  DCL::gropn(2)     
else
  DCL::gropn(1)     
end

DCL::grfrm
DCL::grswnd(tmin, tmax, zmin, zmax)         # 左・右・下・上の座標値
DCL::uspfit                                 # あとはおまかせ
DCL::grstrf                                 # 座標の確定

DCL::sglset('lfprop',true)                  # プロポーショナルフォント
DCL::ussttl('TIME', 'YEAR', 'HEIGHT', 'km') # タイトルの設定
if ARGV.index("-color")
  DCL::uelset("ltone",true)                 # 色つき
  DCL::uetone(u)                            # ぬりわけ
end
DCL::usdaxs                                 # 座標軸を描く
DCL::udcntr(u)                              # 等値線をひく

DCL::sgstxs(0.02)                           # 文字の大きさの設定
DCL::sgtxr(0.7,0.9, "umin="+u.min.to_s+" umax="+u.max.to_s)
                                            # to_s 文字列に変換
require "date"                              # requireは途中にあっても可
DCL::sgtxr(0.75,0.85, "created by #{$0} #{DateTime.now}")

DCL::grcls
