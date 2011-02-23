=begin
= GPhys::EP_Flux チュートリアル

塚原大輔, 堀之内武

作成開始: 2004 年 9 月

最終改訂: 2005 年 02 月 28 日


== 目次

((:<ol class="contents">:))
<<< index.hindex.rd
((:</ol>:))

==はじめに

GPhys::EP_Flux は風速および温度の GPhys オブジェクトから子午面上で定義される
Elliassen-Palm Flux (EP Flux) やその発散を計算する関数を集めたモジュ
ールです.
ここでは GPhys::EP_Flux で用いた用いた数式, 利用方法やその精度の解説を行います.
本チュートリアルでは GPhys. ver 0.3.5 を用いることを前提として話を進めます.

== 数式解説

GPhys::EP_Flux で用いる数式の定義はパッケージ付属の doc/math-doc/((<document.pdf|URL:math-doc/document.pdf>))
や doc/math-doc/document/((<document.html|URL:document/document.html>)) を参照下さい. (html 版は非常に
見苦しいですが, お許し下さい.)


==準備
GPhys::EP_Flux を利用するには GPhys が利用できる必要があります. 2005/02/12 現在の最新版は ver.0.3.5 です.
GPhys のインストール方法は((<GPhys Install|URL:http://dennou-k.gfd-dennou.org/arch/ruby/products/gphys/>))を参照ください.
( GPhys::EP_Flux は GPhys ver 0.3.4 より GPhys 本体に取り込まれました. )


==本チュートリアルで用いるデータ

本チュートリアルではテストデータとして, NCEP 再解析による 2001 年 4 月の全球
の風速および温度のデータを使います.
((<こちら|URL:./testdata.tar.gz>))
からローカルの作業用ディレクトリにダウンロードして展開してください.
チュートリアルのメニューは展開して出来たディレクトリ(testdata/)がカレントディ
レクトリにあるとして実行するものとします.

まずはテストデータの概要を見てみましょう. テストデータファイルは 4 つあり,
それぞれ東西風速(UWND_NCEP.nc), 南北風速(VWND_NCEP.nc), 鉛直風速(OMEGA_NCEP.nc)
, 気温(TEMP_NCEP.nc) のデータです. ただし, 鉛直風速は圧力速度で Pa/s を単位と
しています. ここでは NetCDF付属のコマンド ncdump を用いて UWND_NCEP.nc の
中身を見てみましょう.

   % ncdump -c UWND_NCEP.nc
                           
   netcdf UWND_NCEP {
   dimensions:
           lon = 72 ;
           lat = 37 ;
           level = 12 ;
           time = 120 ;
   variables:
           float lon(lon) ;
                   lon:units = "degrees_east" ;
                   lon:long_name = "Longitude" ;
                   lon:actual_range = 0.f, 357.5f ;
           float lat(lat) ;
                   lat:units = "degrees_north" ;
                   lat:actual_range = 90.f, -90.f ;
                   lat:long_name = "Latitude" ;
           float level(level) ;
                   level:units = "mb" ;
                   level:actual_range = 1000.f, 10.f ;
                   level:long_name = "Level" ;
                   level:positive = "down" ;
                   level:GRIB_id = 100s ;
                   level:GRIB_name = "hPa" ;
           double time(time) ;
                   time:units = "hours since 1-1-1 00:00:0.0" ;
                   time:long_name = "Time" ;
                   time:actual_range = 17531688., 17540442. ;
                   time:delta_t = "0000-00-00 06:00:00" ;
           short uwnd(time, level, lat, lon) ;
                   uwnd:long_name = "4xDaily U-wind" ;
                   uwnd:valid_range = -125.f, 160.f ;
                   uwnd:actual_range = -90.5f, 128.6f ;
                   uwnd:units = "m/s" ;
                   uwnd:add_offset = 202.66f ;
                   uwnd:scale_factor = 0.01f ;
   
   ..中略..
                     
   // global attributes:
                   :history = "2004-08-25 20:31:49 JST daktu32> NumRu::GPhys::NetCDF_IO.write uwnd" ;
                   :title = "4xDaily U-wind, NMC reanalysis (2001-04)" ;
   data:
     
    lon = 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 
       90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 
       165, 170, 175, 180, 185, 190, 195, 200, 205, 210, 215, 220, 225, 230, 
       235, 240, 245, 250, 255, 260, 265, 270, 275, 280, 285, 290, 295, 300, 
       305, 310, 315, 320, 325, 330, 335, 340, 345, 350, 355 ;
     
    lat = 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35, 30, 25, 20, 15, 10, 5, 
       0, -5, -10, -15, -20, -25, -30, -35, -40, -45, -50, -55, -60, -65, -70, 
       -75, -80, -85, -90 ;
     
    level = 1000, 925, 850, 700, 600, 500, 400, 300, 250, 200, 150, 100 ;
     
    time = 17533848, 17533854, 17533860, 17533866, 17533872, 17533878, 17533884, 
       17533890, 17533896, 17533902, 17533908, 17533914, 17533920, 17533926,
     
   ..中略..
       
       17534562 ;
   }
      
              
次元 lon, latは緯度および経度, level は高度(圧力), time は
時間(西暦 1/1/1 00:00:00 を起点とした時間)をそれぞれ表します. オリジナルの
NCEP データは水平に 2.5 度の分解能ですが, ここでは粗くしてデータサイズを押え
てあります. 時間分解能は 6 時間毎です. 

==まずは使ってみよう

以下のプログラムを実行して, EP_Flux とその発散を計算してみまし
ょう. このプログラムはテストデータを読み込んで子午面上の EP_Flux を計算し,
さらにその発散を求めて, netCDF ファイル(epflx_NCEP.nc)として保存する
ものです. 以降断りがない限り, 本チュートリアルで用いるプログラムは全てパッケ
ージに付属するデモプログラムを使用します. なお行番号は説明の便のためにつけら
れており, 実際はありません.

((<demo_NCEP_1.rb|URL:http://www.gfd-dennou.org/arch/ruby/products/gphys/tutorial/ep_flux/demo/demo_NCEP_1.rb>)):
((:<textarea cols="105" rows="20" wrap="hard" class="source">:))
01  require 'numru/gphys'
02  require 'numru/gphys/ep_flux'
03  include NumRu
04
05  datadir = "./testdata/"
06  gp_u =     GPhys::IO.open( datadir+"UWND_NCEP.nc",  "uwnd")
07  gp_v =     GPhys::IO.open( datadir+"VWND_NCEP.nc",  "vwnd")
08  gp_omega = GPhys::IO.open( datadir+"OMEGA_NCEP.nc", "omega")
09  gp_t =     GPhys::IO.open( datadir+"TEMP_NCEP.nc",  "temp")
10
11 epflx_y, epflx_z, v_rmean, w_rmean, gp_lat, = ary =
12           GPhys::EP_Flux::ep_full_sphere(gp_u, gp_v, gp_omega, gp_t, true)
13 gp_lat.rename('phi')
14
15 ary.each{|gp|                                  #  This part will not 
16   gp.data.att_names.each{|nm|                  #  be needed in future.
17     gp.data.del_att(nm) if /^valid_/ =~ nm     #  (Even now, it is not
18   }                                            #  needed if the valid
19 }                                              #  range is wide enough)
20
21 epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
22 strm_rmean = GPhys::EP_Flux::strm_rmean(v_rmean)
23
24 ofile = NetCDF.create("epflx_NCEP.nc")
25
26 ary.each{|gp|                     
27   GPhys::IO.write(ofile, gp)      
28 }                                 
29 GPhys::IO.write(ofile, epflx_div)
30 GPhys::IO.write(ofile, strm_rmean)
31
32 ofile.close
((:</textarea>:))

1-3 行目はいわゆる「お作法」部分です. 1-2 行目でGPhys::EP_Flux を利用するため
に必要なライブラリを読み込んでいます. 'numru/gphys' は 'numru/gphys/ep_flux'
の内部でも呼ばれるので, 2 行目だけでも OK です. 3 行目とすることで NumRu 以下
の名前空間に納められているモジュールを, NumRu:: を省略して呼ぶことができます. 

6-9 行目でテストデータを GPhys オブジェクトとして開いています. ただし,
実際にメモリに読み込まれるのは演算を行う部分(13-14 行目)で, この時点では指定さ
れたファイル中の変数を指しているだけです. ここでは gp_u, gp_v, gp_omega, gp_t
それぞれに東西風速, 南北風速, 鉛直風, 気温が割り当てられています.


11-12 行目で GPhys::EP_Flux::ep_full_sphere を呼んで EP Flux を計算しています.
ep_full_sphere は球面上の EP Flux を近似を用いないフルセットの式を元に計算
する関数です. 5 番目の引数は気温のデータが「温度」であることを意味しています
. ep_full_sphere は温度の代わりに温位を引数にとることが可能なのです.
その場合 false とします. すなわちこの引数は与えられたデータが
温度なのか温位なのかを指定しているわけです. 省略した場合,
デフォルトの値は true (温度) となっています. また上記では省略されていますが,
6 番目の引数には各 GPhys オブジェクトの軸の配置を指定する配列を入力します.
経度軸を 0 , 緯度軸を 1, 鉛直軸を 2 で表現する配列で, デフォルトは [0, 1, 2]
になります. すなわち 0 次元目に経度, 1 次元目に緯度, 2 次元目に鉛直軸が
入っていると思うわけです. 

11 行目についてもう少し詳しく解説します. ep_full_sphere は合計 11 個のオブジェ
クトを戻り値として持ちます(個々のオブジェクトの詳細は後述します). 11 行目では
ruby の多重代入を利用して, それら全てのオブジェクトを ary という配列に代入する
と同時に先頭の 5 個のオブジェクトをそれぞれ epflx_y, epflx_z, v_rmean, w_rmean,
gp_lat という変数に代入しています. 左辺が , で終わっていることに注意してください.
これにより ep_full_sphere の 6 番目以降の戻り値が捨てられます. もしも "," を抜か
すと gp_lat は配列として 5 - 11 番目のオブジェクトが格納されることになります.


13 行目では gp_lat のデータ部分の名前を "phi" に置き換えています(デフォルトは "lat").
先述のとおり, GPhys オブジェクトは変数の名前を属性として持つことができるのですが,
変更したい場合はこのようにすればできます.

15-19 行目はイテレータを用いて戻り値の全てのデータ部分の属性 "valid_hoge" を
削除しています. "valid_hoge" は有効範囲に関する属性で, オリジナルのデータに
あった属性です. GPhys は属性値は陽に指定しない限り, 最初に読みこんだでーたを
覚えているので, 演算した結果変化する場合(今回の有効範囲がそれにあたりますね)
に不都合が生じてしまいます. 非常に広い範囲が与えられていない限り, この値は
変更されなくてはなりません. 将来的に GPhys 自身がこの問題を対処します.

21 行目で GPhys::EP_Flux::div_sphere を呼んで EP Flux の発散を計算しています.
div_sphere は子午面上の y-z 平面における発散を計算するメソッドで, 引数は EP_Flux
以外の物理量にも利用できます. 22 行目で残差循環の南北風から質量流線関数に
比例する量を求めます. 


24-32 行目で, 計算結果を NetCDF ファイルとして保存します. 23 行目で保存ファイル
を作成し, 25-28 行目で個々の GPhys オブジェクトを書き込みます. GPhys::IO.write
は GPhys オブジェクトを NetCDF に書き込む専用のメソッドです. 最後に 30 行目で
保存ファイルをクローズしています. この行が無いと正常に保存を終了することができ
ないので, 忘れずに.

それでは実行してみてください. 計算が終了するまでにかかる時間は著者の環境
(CPU:Celeron 1.1GHz, RAM: 256 MB) で 1 分程度です. 正しく実行されたならば
カレントディレクトリに, 'epflx_NCEP.rb' というファイルが出来たことと思います.
出来たファイルのヘッダを ncdump を用いて出力してみましょう. 以下のように
なると思います(一部省略しています).

  % ruby demo_NCEP_1.rb
  ..しばし待つ..
  % ncdump -h epflx_NCEP.nc

    netcdf epflx_NCEP {
    dimensions:
            lat = 37 ;
            level = 12 ;
            time = 120 ;
    variables:
             ..中略..
            float epflx_y(time, level, lat) ;
             epflx_y:long_name = "EP flux y component" ;
             epflx_y:units = "Pascal.kg-1 m3" ;
             ..中略..
            float v_rmean(time, level, lat) ;
             v_rmean:long_name = "EP flux z component" ;
             v_rmean:units = "m/s" ;
             ..中略..
  // global attributes:
                  :history = "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write epflx_y\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write epflx_z\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write v_rmean\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write w_rmean\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write phi\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write z\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write uwnd\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write temp\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write uv_dash\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write vt_dash\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write uw_dash\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write dtemp_dz\n",
      "2004-09-20 15:36:13 JST daktu32> NumRu::GPhys::NetCDF_IO.write epflx_div" ;
  }

global attribute に書かれている history 属性は全て GPhys が自動的に付加してく
れるものです. 保存した変数全てに対して history が吐かれています. ep_full_sphere
が出力する変数は以下のとおりです.ep_full_sphere の戻り値の順番に列挙します.

  epflx_y   : EP Flux の緯度成分
  epflx_z   : EP Flux の高度成分
  v_rmean   : 残差南北流速
  w_rmean   : 残差鉛直流速
  phi       : 緯度(単位は radian)
  z         : 高度(単位は m)
  uwnd      : 帯状平均東西風速(変数名は入力データに依存)
  temp      : 帯状平均気温(変数名は入力データに依存)
  uv_dash   : 帯状平均擾乱運動量フラックス
  vt_dash   : 帯状平均擾乱熱フラックス
  dtemp_dz  : 帯状平均気温鉛直勾配

demo_NCEP_1.rb ではそれら全てをファイルに保存することにしています. 上記の変数全てが
保存されていることがヘッダに記述されています.

GPhys::EP_Flux の基本的な使い方は以上です. 計算効率の改善などを考えなければ, 上記に
従うだけで正しく EP_Flux を計算できます. 
ここではテストデータとしてnetCDF 形式の NCEP 再解析データを用いましたが, GPhys が対応する形式の
データであれば処理できます. 現行では GRADS のデータでも OK です. 以下にこの節の内容をまとめます.

((:<b>:))
((:<font color=rgb(87,153,51)>:))

まとめ - まずは使ってみよう 

(1) EP Flux を計算するには require "numru/gphys/ep_flux" して
    GPhys::EP_Flux::ep_full_sphere を呼ぶ
(2) 必要な GPhys オブジェクトは東西風速, 南北風速, 鉛直風速, 温度(温位)
(3) 5 番目の引数は温度なら true, 温位なら false. デフォルトは true.
(4) 入力オブジェクトの軸は全部同じ. デフォルトは["経度", "緯度", "鉛直軸"]の順.
(5) ep_full_sphere の戻り値の順番に注意
    * 順に
      epflx_y, epflx_z, v_rmean, w_rmean, phi, z, uwnd, theta, uv_dash,
      vt_dash, uw_dash, dtheta_dz

((:</font>:))
((:</b>:))
      
==巨大データの対処法

demo_NCEP_1.rb のコードでは元のデータ全領域を一気にメモリ上に読み込んで
から計算を行い, 終了後に結果をファイルに出力しています. この
方法ではたくさんのメモリ領域を食いつぶす他, 全ての格子における計算が終
了するまで計算結果をメモリにためておくことになり, 効率が悪いです.
また, ある変数を「全部」書き終ってから, 次の出力に取り掛かるという場合,
GPhys::IO の下で動いている NetCDF 的には裏でファイルの全コピーと作り直し
が生じます. GPhys ではそれを緩和するよう, ある程度独自に出力をバッファリ
ングするようになってるのですが, あまり大きくなるとメモリーを圧迫しますか
ら, 仕方なくバッファーをフラッシュする＝つまり本当に NetCDF に書出すとい
うことをします. こうなるとその後は上記の全コピーと作り直しが生じることに
なります.

ではどうすれば効率的なのでしょうか? それは処理するデータを適当に分割し,
逐次ファイルに出力するようにすればよいです. 
今回のテストデータは時間方向に 120 個の格子点を持っていますので, 時間軸に沿ってループを回すと良いでしょう.
そうすれば計算に必要なデータだけ適宜メモリ上に読み出し, 計算が終り次第
ファイルに書き出す. そして次のデータを読み込む...というようにしていくと
メモリの節約になります. 本節では上記の効率化を GPhys::IO のイテレータを
用いて実装する方法を紹介します. 以下のプログラムをご覧ください.

((<demo_NCEP_2.rb|URL:http://www.gfd-dennou.org/arch/ruby/products/gphys/tutorial/ep_flux/demo/demo_NCEP_2.rb>)):
((:<textarea cols="105" rows="20" wrap="hard" class="source">:))
01  require 'numru/gphys'
02  require 'numru/gphys/ep_flux'
03  include NumRu
04
05  datadir = "./testdata/"
06  gp_u =     GPhys::IO.open( datadir+"UWND_NCEP.nc",  "uwnd")
07  gp_v =     GPhys::IO.open( datadir+"VWND_NCEP.nc",  "vwnd")
08  gp_omega = GPhys::IO.open( datadir+"OMEGA_NCEP.nc", "omega")
09  gp_t =     GPhys::IO.open( datadir+"TEMP_NCEP.nc",  "temp")
10
11 ofile = NetCDF.create("epflx_NCEP.nc")
12
13 nt = gp_u.shape[-1]
14 i = 0
15 GPhys::IO.each_along_dims_write([gp_u, gp_v, gp_omega, gp_t], ofile, -1){
16   |u, v, omega, t|
17   i += 1
18   print "processing #{i} / #{nt} ..\n" if (i % (nt/20+1))==1
19
20   epflx_y, epflx_z, v_rmean, w_rmean, gp_lat, gp_z, u_mean, theta_mean,
21         uv_dash, vt_dash, uw_dash, dtheta_dz = ary =
22                 GPhys::EP_Flux::ep_full_sphere(u, v, omega, t, true)
23
24   ary.each{|gp|                                  #  This part will not   
25     gp.data.att_names.each{|nm|                  #  be needed in future. 
26       gp.data.del_att(nm) if /^valid_/ =~ nm     #  (Even now, it is not 
27     }                                            #  needed if the valid  
28   }                                              #  range is wide enough)
29
30   epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
31   strm_rmean = GPhys::EP_Flux::strm_rmean(v_rmean)
32
33   if i==1    # time independent => write only once                     
34     gp_lat.rename('phi')                                               
35     GPhys::IO.write(ofile, gp_lat)                                     
36     GPhys::IO.write(ofile, gp_z)                                       
37   end                                                                  
38   [ epflx_y, epflx_z, v_rmean, w_rmean, strm_rmean, epflx_div, u_mean, theta_mean, 
39     uv_dash, vt_dash, uw_dash, dtheta_dz ]                             
40 }                                                                      
41                                                                        
42 ofile.close                                                            
43                                                                        
((:</textarea>:))

このプログラムは前節のデモプログラム (demo_NCEP_1.rb) 同様に, テストデータから
子午面上の EP Flux 他を計算し, nc ファイルに保存するものです. ただし, 13 行目以降
が大きく異なります. 先に述べた計算の効率化を図るため, GPhys のイテレータを利用
しています. 

1-11 行目は demo1 と同じくライブラリおよびデータの読み込みと保存ファイルのオープンを
行っています.

13-14 行目は後述のイテレータ部分の処理の進行度合いを出力するための前準備です.
13 行目 (nt = gp_u.shape[-1]) は gp_u の一番最後の次元のグリッド数を返します. このテスト
データでは時間 time のグリッド数 120 を nt に代入します.

15-40 行目は demo1 の 11-28 行目をイテレータを用いて実装しています.
GPhys::IO.each_along_dims_write がそのイテレータ関数です.
詳細は
((<GPhys::NetCDF_IO のリファレンスマニュアル|URL:http://dennou-k.gfd-dennou.org/arch/ruby/products/gphys/doc/gphys_netcdf_io.html#label:6>))
をご覧下さい.   
以下, 該当部分を切り出して解説します. 

      15 GPhys::IO.each_along_dims_write([gp_u, gp_v, gp_omega, gp_t], ofile, -1){
      16   |u, v, omega, t|
      ...
      20   epflx_y, epflx_z, v_rmean, w_rmean, gp_lat, gp_z, u_mean, theta_mean,
      21         uv_dash, vt_dash, uw_dash, dtheta_dz = ary =
      22                 GPhys::EP_Flux::ep_full_sphere(u, v, omega, t, true)
      ...
      38   [ epflx_y, epflx_z, v_rmean, w_rmean, strm_vrmean, epflx_div, u_mean, theta_mean,
      39     uv_dash, vt_dash, uw_dash, dtheta_dz ]
      40 }


15 行目をご覧下さい. GPhys::IO.each_along_dims_write の第一引数 (ここでは [gp_u, ..., gp_t]) には
イテレータ中で処理する GPhys オブジェクトを要素に持つ配列, もしくは GPhys
オブジェクト自身を渡します. 第 2 引数には保存対象のファイルオブジェクトを渡します.
第 3 引数以降はループを回す次元を名前もしくは数字で指定します. ここでは一番最後の
次元である time を割り当てます. 

16 行目では gp_u, gp_v, gp_omega, gp_t を時間軸に沿ってスライスしたオブジェクトを
それぞれ u, v, omega, t としてイテレータ中に渡しています. 

20-22 行目は demo1 同様 EP_Flux::ep_full_sphere を呼び出して各変数の計算を行っています.
唯一異なるのは, 与えた引数が時間軸に沿ってスライスされたオブジェクトである点です.

38-39 行目で, 保存対象ファイル ofile に保存する変数を要素に含む配列を明記して
イテレータのブロックを閉じます. 時間に依存する変数, すなわち時間軸を持つ変数
のみ記載していることに注意してください. (時間に依存しない変数, ここでは gp_lat 等, は
ループ毎に上書きすると無駄なので, 33 - 37 行目のように 1 回だけファイルに書き込
むよう工夫しています.) 42 行目でファイルのクローズを行っています. 

それではプログラムを実行してみましょう. 実行中の標準出力には以下のようになると
思います. 18 行目にて進行状況をループの回数を用いて表現しているためです. この部
分は決して必須ではありませんが, 計算に時間のかかるプログラムにおいては実装した
いですね.

  % ruby demo_NCEP_2.rb

    processing 7 / 120 ..
    processing 14 / 120 ..
    ...
    processing 120 / 120 ..

正常に終了したら ncdump を用いてヘッダを覗いてみましょう. demo_NCEP_1.rb が出力
する nc ファイルを同じであることがわかります (変数の順番は違うかもしれませんが
, そこはお許し下さい).

以上でイテレータを用いたプログラムを紹介しました. イテレータを使えば, メモリの
節約の他, 個々の変数は時間軸に沿ってスライスしたものを並列に処理していくことに
なるので, 出力効率の改善が図れます. 裏を返せば, 巨大なデータを扱うときは, いっ
ぺんにやるとメモリーを圧迫すると同時に出力効率も下がる可能性があるので, イテレ
ーターを使えということです.

((:<b>:))
((:<font color=rgb(87,153,51)>:))

まとめ - 巨大データへの対処法

(1) 巨大データはイテレータを使って処理
    * 使い方はGPhys::NetCDF_IO.each_along_dims_write(gphyses, ofile, dims)
(2) GPhys::EP_Flux を用いる場合, 時間軸を持つデータに有効
  
((:</font>:))
((:</b>:))

    
==簡単お絵描き

本節ではいよいよ GPhys::EP_Flux で出力される物理量を図示してみましょう. EP Flux を
描く場合にはベクトル図がベストです. ここでは EP Flux をベクトル, ダイバージェンスを
コンターおよびトーンで重ねがきするプログラムを紹介します. 今までのプログラムよりも長
いですが, 01-37 行目はお作法およびデータの読み込み部分で, 重要なのは後半部分ですので
ご安心下さい.

((<demo_NCEP_3.rb|URL:http://www.gfd-dennou.org/arch/ruby/products/gphys/tutorial/ep_flux/demo/demo_NCEP_3.rb>)):
((:<textarea cols="105" rows="20" wrap="hard" class="source">:))
01  require 'numru/gphys'
02  require 'numru/gphys/ep_flux'
03  require 'numru/gphys/ggraph_on_merdional_section'
04  include NumRu
05 
06  epflx_fnm = './epflx_NCEP.nc'
07 
08  if File::exist?(epflx_fnm)
09 
10   epflx_y =   GPhys::IO.open(epflx_fnm,  'epflx_y')
11   epflx_z =   GPhys::IO.open(epflx_fnm,  'epflx_z')
12   epflx_div = GPhys::IO.open(epflx_fnm,  'epflx_div')
13   v_rmean =   GPhys::IO.open(epflx_fnm,  'v_rmean')
14   w_rmean =   GPhys::IO.open(epflx_fnm,  'w_rmean')
15   strm_rmean =   GPhys::IO.open(epflx_fnm,  'strm_rmean')
16
17 else
18
19   datadir = "./testdata/"
20   gp_u =     GPhys::IO.open( datadir+"UWND_NCEP.nc",  "uwnd")
21   gp_v =     GPhys::IO.open( datadir+"VWND_NCEP.nc",  "vwnd")
22   gp_omega = GPhys::IO.open( datadir+"OMEGA_NCEP.nc", "omega")
23   gp_t =     GPhys::IO.open( datadir+"TEMP_NCEP.nc",  "temp")
24
25   epflx_y, epflx_z, v_rmean, w_rmean= ary =
26     GPhys::EP_Flux::ep_full_sphere(gp_u, gp_v, gp_omega, gp_t, true)
27
28   ary.each{|gp|                                  #  This part will not
29     gp.data.att_names.each{|nm|                  #  be needed in future.
30       gp.data.del_att(nm) if /^valid_/ =~ nm     #  (Even now, it is not
31     }                                            #  needed if the valid
32   }                                              #  range is wide enough)
33
34   epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
35   strm_rmean = GPhys::EP_Flux::strm_rmean(v_rmean)
36
37 end
38
39
40 DCL.gropn(1)
41 DCL::sglset('LFULL', true)                       # use full area
42 DCL::slrat(1.0, 0.85)                            # set aspect ratio of drwable area
43 DCL.sgpset('lcntl', false)                       # don't rede control character.
44 DCL.sgpset('lfprop',true)                        # use prportional font
45 DCL.uzfact(0.6)                                  # set character size
46
47 ## show vector (Fy, Fz)
48 fy = epflx_y.mean('time')
49 fz = epflx_z.mean('time')
50 epdiv = epflx_div.mean('time')
51
52 GGraph::set_fig('view'=>[0.15, 0.85, 0.25, 0.55])
53 GGraph::set_fig('itr'=>2)                        # contour && tone only
54 GGraph::tone(epdiv)                              # tone
55 GGraph::contour(epdiv,false)                           # contour
56 GGraph::vector_on_merdional_section(fy, fz, false,
57    'fact'=>3.0,'xintv'=>1,'unit'=>true, 'annot'=>false
58                                     )
59
60 ## show residual mean merdional circulation (cut nearly surface and Antarctica)
61 vrm =  v_rmean.cut('lat'=>-70..90, 'level'=>850..100).mean('time')
62 wrm =  w_rmean.cut('lat'=>-70..90, 'level'=>850..100).mean('time')
63 strm = strm_rmean.cut('lat'=>-70..90, 'level'=>850..100).mean('time')
64 GGraph::contour(strm, true, 'nlev'=>25)
65 GGraph::vector_on_merdional_section(vrm, wrm, false,
66                         'fact'=>2.0,'xintv'=>1,'unit'=>true, 'annot'=>false
67                                     )
68 DCL.grcls
((:</textarea>:))

実行するとまず以下の絵(EP-Flux(ベクトル)とその発散(トーン))が画面に描画されます.

((:<center><A HREF="./epflx.png"><IMG SRC="epflx.png" HEIGHT=300 WIDTH=400></A></center>:))

スペースキーを打鍵するか絵をクリックすると次の絵(残差循環(ベクトル)と質量流線関数(コンター))
が出力されます. なお, 質量流線関数は EP_Flux::strm_rmean というメソッドによって計算したもの
です.

((:<center><A HREF="./residual.png"><IMG SRC="residual.png" HEIGHT=300 WIDTH=400></A></center>:))

1-4 行目はお約束ですね. 今までと異なるのは 3 行目(require 'numru/gphys/ggraph_on_merdional_section')
が追加されていることです. GPhys には GPhys オブジェクト専用の描画モジュール GGraph が用意されています.
GGraph を利用するには通常 'numru/ggraph' を require するわけですが, このプログラムでは子午面上のフラックス
ベクトルを描くのに特化したメソッド(GGraph::vector_on_merdional_section)を利用して EP Flux を表現します.
そのメソッドが追加定義されているファイルが 'numru/gphys/ggraph_on_merdional_section' です.

6-37 行目では, demo_NCEP_1.rb もしくは demo_NCEP_2.rb を実行した際に出力される epflx_NCEP.nc が
カレントディレクトリに存在するか否かで場合分けしています. 存在する場合はそちらから必要な変数を
オープンします. ここでは EP Flux の 緯度成分および鉛直成分(epflx_y, epflx_z)とその発散(epflx_div),
そして残差風速(v_rmean, w_rmean) を開きます. もし epflx_NCEP.nc が無い場合は demo1 or 2 同様に
テストデータを元に計算します. このチュートリアルに沿ってデモプログラムを動かした方は既に
EP Flux etc. を計算していると思いますので, 無駄を省くためにこのような場合分けをしてあります.

40-45 行目は絵を描くための下準備です. 40 行目 DCL.gropn(1) は、DCL のグラフィック装置初期化です.
引数が 1 の場合, 端末画面に表示されます. 通常必要なのはこの行のみで後はユーザの好みによります. 
41, 42 行目は全画面出力を行い, 描画領域の縦横比を1:0.85 で指定しています. 43 行目でアンダースコア
を制御文字として解釈しないようにし, 44 行目でフォントにプロポーショナルフォントを用い,
45 行目で座標軸につける文字の大きさを 0.6 倍にしています.

49-51 行目では変数を時間軸に沿って平均しています. EP_Flux::ep_full_sphere  および EP_Flux::div_sphere
が出力する変数は時間方向に元のデータと同じ格子点数のグリッドを持っているので, ここでは 2001 年 4 月平均値を
算出することになります.

53-66 行目でいよいよ絵を描きます. 53 行目で U 座標を V 座標に対応させて, 54 行目で y 軸を
ログスケールでプロットすることにします. 55, 56 行目で EP Flux の発散のトーンおよびコンターを描画します.
なお重ね描きするために GPhys::contour の 2 番目の引数では改ページしないよう false が指定されています.
57-59 行目(GGraph::vector_on_merdional_section)で EP Flux のベクトルを描きます. 同様に 61-63 行目で
残差風速および質量流線関数の平均値を求め 64-67 行目でコンターおよびベクトルを描きます.
ただし地表面付近および南極大陸に相当する領域はエラーが大きいため意図的にカットしてあります(62-63 行目).

GGraph::vector_on_merdional_section について詳しく解説します. このメソッドはベク
トルの y 成分, z 成分が「距離」に比例する時に「流れ」を表現するようになっていま
す. 例えば子午面上の速度は log-P 座標では

    [ v, w ] = [ D(a\phi)/Dt, D(-H\ln\p/\p_00)/Dt ]

と表せます. このとき [v,w] の向きを正しく表すには両者の比が重要なので
U 座標において 軸を y ≡ a\phi, z ≡ -H\ln\p/\p_00 で定義してやれば流れを
表現することになります. EP Flux の場合も

   [Fy, FZ] = \sigma\cos\phi [ ... - v'u' , ... - w'u' ]

と, 両者の共通部分を除けば [v, w] に比例していますので, 速度と同様の
軸を選択してやれば流れが表現できるわけです. GGraph::vector_on_merdional_section
では座標軸は元もとの GPhys オブジェクトが持つ軸 (Grid) を参照して書きますが,
ベクトルを書く段では y 軸を a\phi, z 軸を z として再定義します. 

((:<b>:))
((:<font color=rgb(87,153,51)>:))

まとめ - 簡単お絵描き

(1) 子午面上でベクトル描くなら GGraph::vector_on_merdional_section 
    * require "numru/ggraph_on_merdional_section" を忘れずに
  
((:</font>:))
((:</b>:))

==幅広い適応能力

前節までで GPhys::EP_Flux の基本的な使い方の解説は終りです. ここからは
モジュールの詳細を知りたい人向けに, GPhys::EP_Flux の幅広い適応能力と
計算精度の話をしたいと思います. まずは適応能力についてです.

===引数に対する柔軟性

((<まずは使ってみよう>))で GPhys::EP_Flux::ep_full_sphere は東西風速, 南北風速,
鉛直風(omega), 温度を引数にとることを説明しました. また, 温度の代わりに温位を
与えることができること, 温位を与えた場合は 5 番目の引数を false とすることを
説明しました. より正確には, 5 番目の引数が true ならばモジュールの内部で
4 番目の引数の GPhys オブジェクトを
((<ドキュメント|URL:document/document.html>)) の(1.6)式を用いて温位に変換して,
EP Flux の計算をしています.

また omega の代わりに鉛直風速 w を直接引数に与えることができます.
しかしながら温度のように引数に与えるフラグは必要有りません.
実は GPhys::EP_Flux は与えられたGPhys オブジェクトの単位を元に, そのオ
ブジェクトの物理量が何者か自動的に判別します!! (ここが GPhys::EP_Flux の素晴ら
しい所です). 以下は GPhys::EP_Flux::ep_full_sphere で呼び出す, omega を w に変
換するメソッドです.

     569       def to_w_if_omega(gp, z) # it is only for z coordinate!!!
     570         gp_units = gp.data.units
     571         if gp_units =~ Units.new("Pa/s")
     572           pr = @@p00*exp(-z/@@scale_height)
     573           gp_un = gp_units
     574           pr = pr.convert_units(gp_un*Units.new('s'))
     575           gp = gp*(-@@scale_height/pr)
     576           gp.data.rename!("wwnd")
     577           gp.data.set_att('long_name', "log-P vertical wind")
     578         elsif gp_units =~ Units.new("m/s")
     579           gp = gp.convert_units(Units.new('m/s'))
     580         else
     581           raise ArgumentError,"units of gp.data (#{gp.data.units})
     582                                must be dimention of pressure/time
     583                                                  or length/time."
     584         end
     585         return gp
     586       end

570 行目で鉛直風として与えられた GPhys オブジェクトの単位を取得し, 571-584 で判
定, 各々対処しています. 単位が"Pa/s"と等しい次元であれば
((<ドキュメント|URL:document/document.html>)) の (1.5) 式を用いて w に変換し,
"m/s"と等しい次元であれば "m/s" に換算して計算に用います. 一方どちらの次元も持
たないならば, エラー終了する仕様になっています. 本モジュールではこのように単位
を基準として各物理量の判別を行います. 温度 or 温位は双方とも "K" に等しい次元を
持つので区別できず, やむなくフラグから判断することにしていますが, それ以外は読
み込んだ GPhys オブジェクトが何者なのか, ユーザは知らなくとも GPhys::EP_Flux が
よろしく処理してくれます. 

===データの軸の自動判別

データの座標軸についても GPhys::EP_Flux は柔軟な対応を取ります.
以下は GPhys::EP_Flux::ep_full_sphere 内部で呼び出される, p を log-P に変換する
メソッドです. omega => w の場合と同様に, 単位から判別します.

     599       def to_z_if_pressure(gp_z)
     600                             # number in units is not considerd operater as log.
     601         if ( gp_z.data.units =~ Units.new('Pa') )
     602           p00 = @@p00.convert(gp_z.units)
     603           gp_z = -@@scale_height*log(gp_z/p00)
     604           gp_z.data.set_att('long_name', "z").rename!("z")
     605         elsif ( gp_z.data.units =~ Units.new('m') )
     606           gp_z = gp_z.convert_units(Units.new("m"))
     607         else
     608           raise ArgumentError,"units of gp_z (#{gp_z.data.units})
     609                                must be dimention of pressure or length."
     610         end
     611         return gp_z
     612       end

水平軸はそれぞれ緯度, 経度と角度を単位として指定されねばなりません.
角度の単位は radian, degree の双方に対応しています. 以下は
GPhys::EP_Flux::ep_full_sphere 内部で呼び出される degree を radian に変換するメソッドです.
 
     629       def to_rad_if_deg(gp)
     630         if gp.data.units =~ Units.new("degrees")
     631           gp = gp.convert_units(Units.new('rad'))
     632           gp.units = Units[""]
     633           gp
     634         elsif gp.data.units =~ Units.new('rad')
     635           gp.data = gp.data.copy
     636           gp.data.units = Units[""]
     637           gp
     638         else
     639           raise ArgumentError,"units of gp #{gp.data.units} must be equal to deg or radian."
     640         end
     641         return gp
     642       end

いずれの変換メソッドも, サポート対象外の単位を検知した場合はエラー終了します.
逆にいえばエラーを吐かない限り, ユーザは物理量の単位等を知らなくとも, モジュールに
お任せで計算出来ます.


===パラメータの変更

GPhys::EP_Flux では以下に示すモジュール変数が定義されています.

     cp             = 1004                       # 大気の等圧比熱 [J.K-1.kg-1]. 
     gas_const      = 287.0                      # 乾燥大気における単位質量あたりの気体定数 [J.K-1.kg-1]
     
     @@scale_height = UNumeric.new(7000,  "m")   # スケールハイト(log 圧力)
     @@radius       = UNumeric.new(6.37E6,"m")   # 惑星半径
     @@rot_period   = UNumeric.new(8.64E4,"s")   # 自転周期
     @@p00          = UNumeric.new(1.0E5,"Pa")   # 参照圧力(地表面大気圧を想定)
     @@g_forces     = UNumeric.new(9.81, "m.s-2")# 重力加速度

デフォルト値は全て地球の値です. 上記のパラメータは全てアクセスメソッドが
定義されており変更が可能です(詳細は ((<ドキュメント|URL:ep_flux.html>)) を参照下さい. )
これにより地球以外の惑星における EP Flux を計算することも可能になります. 

((:<b>:))
((:<font color=rgb(87,153,51)>:))

まとめ - 適応能力

* 温度 や omega だけでなく、温位 や 鉛直速度 (w = -H\omega/p) を引数に取れる
* 鉛直座標も p だけでなく z = -H logp が使える（しかも自動判別）
* 自転周期も惑星半径もスケールハイトも変えられる。
  ==> 地球以外にも適用化 （但し球形に限る）
  
((:</font>:))
((:</b>:))

((:<hr>:))


##############################################################################
         
=end
