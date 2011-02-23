require "numru/netcdf"
require "numru/dcl"
include NumRu

file1 = NetCDF.open("T.jan.nc")        # NetCDFファイルオブジェクト

var_temp = file1.var("T")              # 変数をオープン
temp = var_temp.get                    # 変数の値を取り出す

var_level = file1.var("level")
level = var_level.get

file1.close

p temp

DCL::gropn(1)

for k in 0..level.shape[0]-1           # 鉛直層数だけ繰り返し
  DCL::grfrm
  DCL::grstrn(10)                      # 正距円筒図法
  DCL::grswnd(0.0,360.0,-90.0,90.0)    # 描画する緯度経度の指定
#  DCL::grsvpt(0.2,0.8,0.25,0.7)        # ウィンドウ内の描画領域の指定
  DCL::umpfit                          # あとはおまかせ
  DCL::grstrf                          # 確定

  DCL::uelset("ltone",true)            # 色を付ける
  DCL::uetone(temp[true,-1..0,k])      # k層目の気温を描く
                                       # データは北から入っているので
                                       # -1..0 として順番をひっくり返す

  DCL::umpglb                          # 縁と緯度経度線をかく
  DCL::umpmap("coast_world")           # 地図を描く

  DCL::uxsttl("t","Temperature at #{level[k]}hPa",0.0)

end

DCL::grcls

