=GPhysコマンドチュートリアル
2006/03/09 中野満寿男

GPhysをインストールするとには以下のコマンドが使えるようになります。
GPhysのインストールについてはGPhys チュートリアルの((<インストール|URL:http://www.gfd-dennou.org/arch/ruby/products/gphys/tutorial/body-j.html#h2:install>))を御覧ください。

 * grads2nc_with_gphys: GrADS 形式のファイルをNerCDF形式のファイルに変換します。
 * gdir_server: GDirサーバーを立ち上げます
 * gdir_client: irb+GDirクライアント
 * gpcat: GPhysファイルの連結 
 * gpcut:  GPhysのスライスなど
 * gplist: ファイルの変数と軸を表示
 * gpmath: GPhys変数に数学関数を適用
 * gpmaxmin: GPhys変数の最大・最小値を表示
 * gpprint: GPhys変数の値を表示
 * gpview: 1次元、2次元のグラフィック生成

このチュートリアルではgpviewによる可視化を中心に解説していきます。
((:<hr>:))
Contents
<<< TOC.rd
((:<hr>:))

==知っていると便利なコマンドたち
===ファイルの中の変数名や軸を調べる 『 gplist コマンド』
可視化の前にサンプルデータ ((<T.jan.nc|URL:T.jan.nc>)) の中身がどのようなものか調べてみよう。 netCDF ファイルであれば ncdump -h することで簡単に調べられますが、今回は gplist を用いてみます。

  % gplist T.jan.nc
すると、
  T.jan.nc:
    lon   [lon=36]        'Longitude'     (degrees_east)
    lat   [lat=19]        'Latitude'      (degrees_north)
    level [level=9]       'Level' (millibar)
    T     [lon=36,lat=19,level=9] 'Temperature'   (degC)

と表示されます。これで(lon, lat, level)の３つの次元をもつデータとして T があることがわかります。

GrADS コントロールファイルや、 grib ファイルでも同様にして調べることができます。
サンプルデータ((<T.jan.grib|URL:T.jan.grib>))でためしてみよう。
  % gplist T.jan.grib
すると
  T.jan.grib:
    TMP   [lon=36,lat=19,level=9] 'Temperature'   (K)
と表示されます。

===ファイルの中の変数の値を表示する 『 gpprint コマンド』
gpprint コマンドをつかうと、変数の値の表示ができます。T.jan.nc ファイルに書かれている変数 T の値の表示するには下のように入力します。

  % gpprint T.jan.nc@T

===データの最小・最大値を知りたい『gpmaxmin コマンド』
gpmaxmin コマンドを使うことで値の最小・最大値を知ることができます。

  % gpmaxmin T.jan.nc@T
すると
  T.jan.nc@T : max=33.2092704772949, at, 
       ...(total 1 points)...
  
  T.jan.nc@T : min=-77.9977569580078, at,  
       ...(total 1 points)...
北緯40度の緯度-高度データの最大・最小値を知るには以下のようにします。
  % gpmaxmin T.jan.nc@T,lat=40
すると
  T.jan.nc@T,lat=40 : max=13.325665473938, at, 
       ...(total 1 points)...

  T.jan.nc@T,lat=40 : min=-62.9790420532227, at,  
       ...(total 1 points)...

==gpviewによるお手軽2D可視化まで
===gpviewによるおまかせ可視化
サンプルデータ ((<T.jan.nc|URL:T.jan.nc>)) には変数 T があることがわかったので早速gpviewを使って可視化してみよう。
gpview の最も簡単な使いかたは、『gpview ファイル名@変数名』です。

  % gpview T.jan.nc@T
と入力すると下のような図が表示されることでしょう。

出力結果：
((:<center><IMG SRC="T1000mb.png" width=640 height=480></center>:))

このように3次元データに対して、特に切りだし面などを指定しなかった場合、始めの2次元に対して描画が行われます。
図の右下に level=1000millibar とあることから、 1000mb 面の温度分布が描画されたことがわかります。
もちろん GrADS コントロールファイル、 grib ファイルでも同様にして描画ができます。

GrADS ファイルの場合：
  % gpview T.jan.ctl@T

grib ファイルの場合：
  % gpview T.jan.grib@TMP

===切りだし面を指定して描画
変数名に続けて,(カンマ)で区切って切りだし面や描画範囲を指定することができます。
まずはある高度を切り出してみよう。
  % gpview T.jan.nc@T,level=500

出力結果：
((:<center><IMG SRC="T500mb.png" width=640 height=480></center>:))

図の右下に level=600millibar とあることに注意しよう。
ここでは 500mb のデータがなかったので、 500mb に近い 600mb のデータが描画されました。

===描画範囲の指定 『：（コロン）』
さらに描画範囲も指定してみましょう。範囲指定には : (コロン)を使います。
例えば 0:10 は、0から10までという意味です。
次のコマンドは、さっきと同じ高度で東経0-180度、北緯0-40度だけを書くように指定しています。
  % gpview T.jan.nc@T,level=500,lon=0:180,lat=0:40

出力結果：
((:<center><IMG SRC="T500mb2.png" width=640 height=480></center>:))

===アニメーションもお手軽 『anim オプション、reverse オプション』
--anim オプションを指定することでアニメーションも可能です。次のコマンドを実行することでそれぞれの高度(level)の断面を連続して見ることができます。
  % gpview --anim level T.jan.nc@T

さらに --reverse オプションを指定することで逆再生アニメーションも可能です。次のコマンドを実行すると 10mb 面から描画されます。
  % gpview --anim level --reverse T.jan.nc@T

==もっと2D描画
===コンターだけ書いてほしい 『noshade オプション』
--noshade オプションを付けることによって色塗りを止めさせることができます。

  % gpview --noshade T.jan.nc@T
出力結果：
((:<center><IMG SRC="T1000mb_noshade.png" width=640 height=480></center>:))

===コンター間隔の指定　『cint オプション』
--cintオプションによってコンター間隔を指定することができます。
次の例ではコンター間隔を 4K に設定しています。
  % gpview --noshade --cint 4 T.jan.nc@T
出力結果：
((:<center><IMG SRC="T1000mb_noshade_int4.png" width=640 height=480></center>:))

===コンターを引く値の範囲を指定　『crange オプション』
--crange オプションによってコンターを引く値の範囲を指定することができます。次の例では 20℃ から 40℃ までしかコンターが引かれません。

  % gpview --noshade --crange 20:40 T.jan.nc@T
出力結果：
((:<center><IMG SRC="crange.png" width=640 height=480></center>:))

===色塗りだけしてほしい　『nocont オプション』
--nocont オプションを付けることによって等値線を書くのを止めさせることができます。。

  % gpview --nocont T.jan.nc@T
出力結果：
((:<center><IMG SRC="T1000mb_nocont.png" width=640 height=480></center>:))

===色塗り間隔の指定 『sint オプション』
--sint オプションを指定します。
次の例ではコンター間隔を 3K に設定しています。
  % gpview --nocont --sint 3 T.jan.nc@T
出力結果：
((:<center><IMG SRC="T1000mb_nocont_int3.png" width=640 height=480></center>:))

===塗りわけを行う値の範囲を指定　『srange オプション』
--srange オプションによって塗り分けをする値の範囲を指定することができます。次の例では 20℃ から 40℃ までしか塗りわけが行われません。

  % gpview --nocont --srange 20:40 T.jan.nc@T
出力結果：
((:<center><IMG SRC="srange.png" width=640 height=480></center>:))


===コンターの値を直接指定　『levels オプション』
--levels オプションでコンターの値を直接指定することができます。
次の例では20度から30度にかけて2.5度おきにコンターをひきます。
  % gpview --noshade  --levels -40,-30,-20,-10,0,10,20,22.5,25,27.5,30,40   T.jan.nc@T 
出力結果：
((:<center><IMG SRC="level_sirokuro.png" width=640 height=480></center>:))

このオプションは色塗りにも適用されます。--noshade オプションを外してみましょう。
  % gpview --noshade  --levels -40,-30,-20,-10,0,10,20,22.5,25,27.5,30,40   T.jan.nc@T 
出力結果：
((:<center><IMG SRC="level_color.png" width=640 height=480></center>:))
色塗りも正しく行えています。

===各レベルのパターンまで指定 『pattern オプション』
パターンの指定は --pattern オプションで行えます。パターンはコンター値の数よりもひとつ多く指定してあげるのがミソです。
謎の5桁の数字は上2桁が色番号、下3桁がトーン番号を表しています。色番号はdclclr コマンドもしくは cdclclr コマンド、トーン番号は dcltone コマンドもしくは cdcltone コマンドで参照できます。


  % gpview --levels -30,-20,-10,0,10,20,30 --pattern 30999,33999,36999,40999,70999,73999,76999,79999  T.jan.nc@T

出力結果：
((:<center><IMG SRC="color.png" width=640 height=480></center>:))

白黒だってここまでできます。今回は2桁もしくは3桁の数字がならんでいますが、これらはトーン番号です。
  % gpview --levels -30,-20,-10,0,10,20,30 --pattern 51,53,200,201,202,203,204,205  T.jan.nc@T 

出力結果：
((:<center><IMG SRC="sirokuro.png" width=640 height=480></center>:))

===地図投影　『map オプション, itr オプション』
地図投影には --itr オプションで投影の種類を, --map オプションで用いる地図情報を指定します。
itr の指定にあたっては DCL マニュアルの((<いろいろな地図投影法|URL:http://www.gfd-dennou.org/arch/ruby/products/ruby-dcl/ruby-dcl-doc/rakuraku/node61.html>))を参考にしてください。
mapに指定できるのは coast_world, border_world, plate_world, state_usa, coast_japan, pref_japan の6種類です  

次の例ではモルワイデ図法で世界の海岸線を重ねています。

  % gpview  --itr 12  --map coast_world T.jan.nc@T 

出力結果：
((:<center><IMG SRC="map.png" width=640 height=480></center>:))

===書いた絵を保存したい『wsn オプションと便利な D キー』
--wsnオプションで図の出力先を指定できます。
次の例では画面での描画は行われず、 PS ファイル( dcl.ps )が生成されます。論文などに図を貼るときは、この例が重宝するでしょう。
  % gpview  --wsn 2  T.jan.nc@T 

また、次のように wsn に 1 を指定した場合は X で描画されます（デフォルトでこの設定です）。図がでているときに D キーを押すことで xwd ファイルが生成されます。
  % gpview  --wsn 1 T.jan.nc@T 

また、次のように wsn に 4 を指定すると Gtk を用いて描画されます。図がでているときに D キーを押すことで png ファイルが生成されます。 web ページなどに図を貼り付けたい場合にはこの方法がよいでしょう。実際このチュートリアルに使われている図はこの方法で生成されました。
  % gpview  --wsn 4 T.jan.nc@T 

==1Dの描画
===おまかせ描画
3次元データで2つの次元を fix すると折れ線グラフを描いてくれます。日本付近の気温の鉛直プロファイルを描いてみましょう。

  % gpview  T.jan.nc@T,lon=140,lat=40
出力結果：
((:<center><IMG SRC="line.png" width=640 height=480></center>:))

===x軸とy軸の入れ換え 『exch オプション』
さっきの絵だと、横軸が高度方向になってしまったため、鉛直プロファイルとしては見にくいものになってしまいました。
x軸とy軸を入れ換えるために、 --exch オプションがあります。

  % gpview  --exch T.jan.nc@T,lon=140,lat=40
出力結果：
((:<center><IMG SRC="line-exch.png" width=640 height=480></center>:))

このオプションはもちろん2次元図でも有効です。

===縦横比の指定 『aspect オプション』
--aspect オプションを指定することで縦横比を指定できます。指定しない場合は縦横比が2になっています。

  % gpview  --exch --aspect 0.8 T.jan.nc@T,lon=140,lat=40

出力結果：
((:<center><IMG SRC="aspect.png" width=640 height=480></center>:))

このオプションはもちろん2次元図でも有効です。

===ラインインデックスを指定する 『index オプション』
--index オプションでラインインデックスを指定してグラフを描かせることもできます。
次の例だと赤(2)で太線(5)で線を描きます。
  % gpview --exch --index 25 T.jan.nc@T,lon=140,lat=40
出力結果：
((:<center><IMG SRC="lineindex.png" width=640 height=480></center>:))

===線種を指定する 『type オプション』
--type オプションで線種を指定してグラフを描かせることもできます。
次の例だと破線(2)で線を描きます。
  % gpview --exch --type 2 T.jan.nc@T,lon=140,lat=40
出力結果：
((:<center><IMG SRC="linetype.png" width=640 height=480></center>:))

===折れ線グラフの重ね書き 『overplot オプション』
--overplot オプションを使うことで折れ線の重ね書きも可能です。例では北緯40度、東経140度と150度の温度鉛直プロファイルを重ね書きしています。
2本の線を重ね書きするために --overplot 2 を指定します。
  %  gpview --exch --overplot 2 T.jan.nc@T,lon=140,lat=40  T.jan.nc@T,lon=150,lat=40
出力結果：
((:<center><IMG SRC="overplot.png" width=640 height=480></center>:))

===値の範囲を指定 『range オプション』
さっきの例では値の範囲が東経140度のデータをもとに自動スケーリングされたために東経150度のグラフが枠からはみでてしまいました。
値の範囲指定を行うために --range オプションがあります。
  % gpview --exch --overplot 2 --range -60:10 T.jan.nc@T,lon=140,lat=40 T.jan.nc@T,lon=150,lat=40
出力結果：
((:<center><IMG SRC="range.png" width=640 height=480></center>:))

これではみ出なくなりました。
この機能は折れ線のアニメーションをする場合にはとても重宝します。

===データの最小・最大値を知りたい『gpmaxmin コマンド』
gpmaxmin コマンドを使うことで値の最小・最大値を知ることができます。例えば北緯40度の緯度円での経度-高度データの最小・最大値を知るには、次のようにします。
  % gpmaxmin T.jan.nc@T,lat=40
すると
  T.jan.nc@T,lat=40 : max=13.325665473938, at, 
       ...(total 1 points)...

  T.jan.nc@T,lat=40 : min=-62.9790420532227, at,  
       ...(total 1 points)...

と表示されます。北緯40度の経度-高度データにおいて、最大値は13度、最小値は-63度位であることがわかります。
この結果を参考にして、さっきの例のように --range オプションで値の範囲を指定してあげると、横軸が自動的に変わることなく、かつグラフが枠からはみ出ることなくアニメーションが行えます。

  % gpview  --exch --range -65:15 --anim lon T.jan.nc@T,lat=40 

==さくっと平均図 『mean オプション』
--mean オプションを使うとある軸方向に平均をとったデータで絵を書くことができます。T.jan.ncをつかってさくっと1月の帯状平均温度を書いてみましょう。
  % gpview  --mean lon T.jan.nc@T 

出力結果：
((:<center><IMG SRC="mean.png" width=640 height=480></center>:))

2方向の平均もなんのその。緯度方向にも平均してしまうこともできます。
  % gpview  --exch --mean lon,lat T.jan.nc@T 

出力結果：
((:<center><IMG SRC="mean2.png" width=640 height=480></center>:))

==ネットワーク上のデータだって

OPeNDAP/DODS 対応版 RubyNetCDF がインストールされている場合には、DODS サーバー上のデータを使って描画することができます。ここでは((<電脳データサーバー|URL:http://davis.rish.kyoto-u.ac.jp/>))上のデータを使って描画してみます。

  % gpview http://davis-dods.rish.kyoto-u.ac.jp/cgi-bin/nph-dods/jmadata/gpv/netcdf/r1h/MSM-S/2006/0226.nc@r1h
出力結果：
((:<center><IMG SRC="dods.png" width=640 height=480></center>:))


==さいごに
このチュートリアルでは gpview の使いかたと、よく使うオプションを紹介しました。チュートリアルというよりは逆引きリファレンスっぽいものになってしまいましたが。。。
全ての gpview のオプションは((<リファレンスマニュアル|URL:http://dennou-k.gfd-dennou.org/arch/ruby/products/gphys/doc/gpview.html>))に書かれていますので参考にしてください。
また、同様の内容は --help オプションをつけることで表示することができます。

  % gpview  --help
