# GPhys, GGraph チュートリアル (ジャンプ. GPhys 編)
#
# = 履歴 (新しいものは上から追記)
#
# * 2012/02/18 納多哲史  新規作成
#

((*このドキュメントは作成中です. 学習には
((<"現行のチュートリアル"|URL:http://ruby.gfd-dennou.org/products/gphys/tutorial/>))
を参照ください.*))

== GPhys を使ってちょっと応用

GPhys ライブラリを本格的に使ってみましょう.

=== GPhys オブジェクトの操作

再度 T.nc を読み込みます.

  gp = gpopen('T.nc/T')

これまでは全世界を描いていましたが, 日本付近を切り出してみましょう.
どの場所を見ているか分かりやすいよう、海岸線を描くようにしておきます.

  set_fig('itr' => 10 )  # 正距円筒図法
  set_map('coast_world'=>true)   # 地球の海岸線を表示

切り出すためには, 切り出す軸を指定する必要があります.
gp の中に格納されている軸を見てみましょう.

  gp

"axis pos=" の後ろにあるのが軸の名前です.
# データと軸がセットになっていることも GPhys の売りとして強調すべき.

lat, lon がそれぞれ緯度経度と推測されますので, 
それで切ってみましょう.

  tone(gp.cut('lat'=>20..50, 'lon'=>120..150))  # 緯度 20-50 度, 経度 120-150 度の範囲を切り出して描画

このとき, 指定した緯度経度にデータが存在しなくても, 
GPhys 側で近い値を探して取ってきてくれます.

ちなみにこれは, 

  gp_jpn = gp.cut('lat'=>20..50, 'lon'=>120..150)
  tone(gp_jpn)

と分けて書くこともできます.
たとえば日本付近の図を何枚も描きたいときなどに入力を減らせるので楽です.

cut に値をひとつだけ入れるとその面での切り出しになります.
ためしに日本上空の鉛直プロファイルを見てみましょう.

  line(gp.cut('lon'=>135,'lat'=>35), true, 'exchange'=>true)  # 縦軸を鉛直座標にするために exchange を設定しています.

平均するためのメソッド mean もあります.
試しに帯状平均 (東西方向に平均) した温度分布を見てみましょう.

  set_fig('itr'=>1)   # 座標系を元に戻しておく.
  tone(gp.mean('lon'))

==== 演習問題

(1) 500 hPa 断面の温度分布を描いてみましょう.

=== GPhys オブジェクトの操作 その 2

軸情報が共通する GPhys オブジェクト同士では
NArray の要領で計算ができます.

  eddy = gp - gp.mean('lon')  # 帯状平均からの偏差
  tone(eddy)
  color_bar

描画以外にも活用してみましょう.
日本付近の 1000 hPa の温度を調べてみましょう.

  gp2 = gp.cut('lon'=>135,'lat'=>35,'level'=>1000))

# 何かのメソッドで "-5.5... degC" と標準出力されるはず. 方法忘れた.

GPhys オブジェクトには軸や単位の情報が入っています.
値だけを取り出したい場合は
以下のようにすると NArray 形式のデータが得られます.

  gp2.val

ちなみに軸の座標の値を取り出すにはこのようにします.

  gp.axis('level').pos.val

==== 演習問題

(1) 1000 hPa, 緯度 30 度における帯状平均温度を求めてみましょう.

(2) (やや難) 70 hPa, 経度 135 度断面で, 一番気温が低い緯度はどこでしょうか.

=== テキスト入出力

データをテキストで出力してみます.
準備として以下の内容を入力しておきます.

  na_jpn = gp2.val
  level = gp.axis('level').pos.val

後の都合のために, データを詰め替えておきます.

  n = na_jpn.size   # 配列のサイズを得る
  nary = NArray.sfloat(n,2)    # sfloat は単精度実数
  nary[true,0] = level         # true はその次元の全要素を意味する
  nary[true,1] = na_jpn.to_a   # NArrayMiss クラスなのでそのまま代入できない. サンプルの問題?

テキストデータとして書き出してみましょう.

  file = File.open("test.dat", 'w')   # test.dat という名のファイルを書き込み用に開く
  (0..n-1).each{|i| 
    file.puts nary[true, i].to_a.join(" ")  # 半角スペースを挟んで連結し, 文字列として出力
  }
  file.close    # ファイルを閉じる

端末で中を見てみる

  $ cat test.dat

今度は, 今さっき出力したファイルを読み込んでみましょう.

  file2 = File.open("test.dat", 'r')   # 読み込み用で開く
  str = file2.read     # ファイルの内容全てを文字列として str に代入
  file2.close

これだと str はまだ連結された文字列なので, 
扱いやすいように配列にし, NArray に変換します.

  ary = str.split("\n").each{|a| a = a.split(" ") }
  nary = NArray.to_na(ary)

split は, ドットの前の文字列を引数の文字列で分割し, 配列に変換するメソッドです.

=== GPhys オブジェクトの作成

自分で GPhys オブジェクトを作ってみましょう.
さっき読み込んだデータを利用します.

  level = nary[true,0]
  temp  = nary[true,1]

ついでにデータの中身を変更してみます.
今の温度データの単位は摂氏なので絶対温度にしてみます.

  temp = temp + 273.15

GPhys オブジェクトを作ります
(長いのでコピーペースト推奨).

  # 軸情報
  va_level = VArray.new( level,
                      {"long_name"=>"Level", "units"=>"hPa"},
                      "level" )
  axis_level = Axis.new.set_pos(va_level)
  
  # 温度
  data = VArray.new( temp,
                     {"long_name"=>"temperature", "units"=>"K"},  # 単位が K であることに注意
                     "T" )

  # GPhys オブジェクトの作成
  gp3 = GPhys.new( Grid.new(axis_level), data )

描画してみましょう.

  line(gp3, true, 'exchange'=>true)

温度の軸の数値や単位がさっきと変わっています.

==== 演習問題

(1) 温度データの "long_name"=>"temperature" の "temperature" を好きな文字列に変えて
    GPhys オブジェクトを作成して, 図のタイトルや軸の名前が変わることを確認してみましょう.

=== netCDF ファイルの入出力

GPhys オブジェクトを netCDF ファイルに書きだしてみます.

  outfile = NetCDF.create("test.nc")
  GPhys::IO.write(outfile, gp3)  # netCDF ファイルに書き出す
  outfile.close

ところで, これまでは

  gp = gpopen('T.nc/T')

という具合に T.nc の中に変数 T があることを知っていて
netCDF ファイルを読み込んでいましたが, 
他の人が配布しているファイルはこうなっているとは限りません.
ファイルの中にどのような変数があるか調べてみましょう.

  GPhys::IO.var_names('T.nc')

# このメソッドも alias したほうがいいかもしれない.

==== 演習問題

(1) test.nc を読み込んで, 自由に描画してみましょう.

=== スクリプトファイルで行うには

irb は試行錯誤に向いていますが, 
決まった図を大量に生成したり, 複雑な操作をするのには向いていません.
その場合はスクリプトファイルにしておくと
あとで使い回せるので便利です.

書く命令は irb のときと同じです.
単純な例を示します.

  #!/usr/bin/env ruby
  require "irb_ggraph"     # おまじない. カレントディレクトリに irb_ggraph.rb がないとエラーになるので注意
  gp = gpopen('T.nc/T')
  tone(gp)
  DCL.grcls   # スクリプトファイルにするときは必須

たとえばこれを test.rb として保存します.
実行はこのようにします.

  $ ruby test.rb

ファイルに実行権限をつければ, より短いコマンドになります.

  $ chmod u+x test.rb
  $ ./test.rb

==== 演習問題

(1) test.rb の中身を好きに書き換え, 実行してみましょう.

(2) test.rb の出力先を netCDF ファイルにしてみましょう.

(3) test.rb で使うデータをテキストファイルからの入力にしたり, 
    ファイルを読み込まずに手元で生成したデータにしてみましょう.
    (ヒント: 最初に NArray で軸とデータを作る,
    それを GPhys オブジェクトに変換する)

== 参考文献

* ((<"GPhys/GGraph チートシート"|URL:http://davis.gfd-dennou.org/rubygadgets/ja/?%28Others%29+GPhys%2FGGraph+%A5%C1%A1%BC%A5%C8%A5%B7%A1%BC%A5%C8>))
* GPhys リファレンスマニュアル
  (
  ((<"英語"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
  ((<"日本語訳"|URL:http://w.livedoor.jp/gphys/>))
  )



== 付録: 実際の使用例

=== サンプル

* ((<"RubyDCL demo programs"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/demo/>))

=== 実際の研究に使われている例
# 本当はチュートリアルには不要だが, 電脳製品の宣伝と, 
# 実際に使われている例を見せることによる参加者のモチベーション上げを狙って.

* 大気大循環モデルの例: ((<"dcpam5 を用いた計算の結果"|URL:http://www.gfd-dennou.org/library/dcpam/sample/>))
* 非静力学モデル: ((<"プログラムのテスト計算 [deepconv/arare5]"|URL:http://www.gfd-dennou.org/library/deepconv/arare/sample/>))
* (モデルだけでなく観測結果などの図集も欲しい)


