require "numru/ggraph"
include NumRu
file = NetCDF.open("uwnd.mon.ltm.nc")
uwnd = GPhys::NetCDF_IO.open(file,"uwnd")
DCL.uzfact(0.6)

gobjs = [
   GGraph.open(1) ,        # 実は各描画命令はオブジェクト化されて各メソッドの
   GGraph.contour(uwnd.mean(0)) ,    # 戻り値となっている。複合なら配列として
   GGraph.contour(uwnd[true,true,5,0])  ,
   GGraph.close  ]

gobjs.flatten!                 # 入れ子になってる配列をフラットに
gobjs.each{|i| p i.class}      # 各コマンドのクラスを表示してみる
gobjs.each{|i| i.exec}         # 再実行 --- 先ほど描いたものが再現される
