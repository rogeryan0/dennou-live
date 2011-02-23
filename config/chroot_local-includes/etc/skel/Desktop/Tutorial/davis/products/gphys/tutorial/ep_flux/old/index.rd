=begin
= GPhys::EP_Flux チュートリアル

塚原大輔, 堀之内武

作成: 2004 年 9 月

最終改訂: 2004 年 10 月 26 日

== 目次
((:<ol class="contents">:))
<<< index.hindex.rd
((:</ol>:))

==はじめに

GPhys::EP_Flux は風速および温度の GPhys オブジェクトから子午面上で定義される
Elliassen-Palm Flux (EP Flux) やその divergence を計算する関数を集めたモジュ
ールです. 将来的には, 3 次元の Plumb Flux や Takaya-Nakamura Fluxを計算する
関数も追加される予定です. 本チュートリアルでは, GPhys::EP_Flux の使い方(利用
編)と自分で定義した関数を GPhys::EP_Flux に追加する方法やGPhys 用モジュール
の開発に関する TIPS(開発編) を紹介します.


==利用編


ここでは GPhys::EP_Flux の利用方法やその精度, および用いた数式の解説を行いま
す.

===準備
GPhys::EP_Flux は GPhys の cvs 版(2004/08/26 日現在) に依存しています.
利用される方はまずそちらをインストールしてください.

GPhys::EP_Flux を利用するには以下のパッケージを以下の資源が必要です.

* ((<numru-derivative|URL:http://www.gfd-dennou.org/arch/ruby/products/numru-derivative/numru-derivative.0.1.2.tar.gz>))
* ((<ep_flux|URL:http://www.gfd-dennou.org/arch/ruby/products/ep_flux/ep_flux.0.0.2.3.tar.gz>))
  
なお GPhys::EP_Flux は 次の GPhys のバージョンで 本体に取り込まれます.
(2004 年 9 月 7 日現在の GPhys 最新版は ver.0.3.3)
いますぐに使いたいという方は, 展開したディレクトリ内で install.rb を実行してください.

===本チュートリアルで用いるデータ

本チュートリアルではテストデータとして, NCEP 再解析による 2001 年 4 月の全球
の風速および温度のデータを使います.
((<こちら|URL:http://www.gfd-dennou.org/arch/ruby/products/ep_flux/tutorial/testdata.tar.gz>))
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

===基本的な使い方

まずは以下のプログラムを実行して, EP_Flux とその divergence を計算してみまし
ょう. このプログラムはテストデータを読み込んで子午面上の EP_Flux を計算し,
さらにその divergence を求めて, netCDF ファイル(epflx_NCEP.nc)として保存する
ものです. 以降断りがない限り, 本チュートリアルで用いるプログラムは全てパッケ
ージに付属するデモプログラムを使用します. なお行番号は説明の便のためにつけら
れており, 実際はありません.

((<demo_NCEP_1.rb|URL:http://www.gfd-dennou.org/arch/ruby/products/ep_flux/tutorial/demo/demo_NCEP_1.rb>)):
      1  require 'numru/gphys'
      2  require 'numru/gphys/ep_flux'
      3  include NumRu
      4
      5  datadir = "./testdata/"
      6  gp_u =     GPhys::IO.open( datadir+"UWND_NCEP.nc",  "uwnd")
      7  gp_v =     GPhys::IO.open( datadir+"VWND_NCEP.nc",  "vwnd")
      8  gp_omega = GPhys::IO.open( datadir+"OMEGA_NCEP.nc", "omega")
      9  gp_t =     GPhys::IO.open( datadir+"TEMP_NCEP.nc",  "temp")
      10
      11 epflx_y, epflx_z, v_rmean, w_rmean, gp_lat, = ary =
      12           GPhys::EP_Flux::ep_full_sphere(gp_u, gp_v, gp_omega, gp_t, true)
      13 gp_lat.rename('phi')
      14
      15 epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
      16
      17 ary.each{|gp|                                  #  This part will not
      18   gp.data.att_names.each{|nm|                  #  be needed in future.
      19     gp.data.del_att(nm) if /^valid_/ =~ nm     #  (Even now, it is not
      20   }                                            #  needed if the valid
      21 }                                              #  range is wide enough)
      22
      23 ofile = NetCDF.create("epflx_NCEP.nc")
      24
      25 ary.each{|gp|
      26   GPhys::IO.write(ofile, gp)
      27 }
      28 GPhys::IO.write(ofile, epflx_div)
      29
      30 ofile.close

1-3 行目はいわゆる「お作法」部分です. 1-2 行目でGPhys::EP_Flux を利用するため
に必要なライブラリを読み込んでいます. 'numru/gphys' は 'numru/gphys/ep_flux'
の内部でも呼ばれるので, 2 行目だけでも OK です. 3 行目とすることで NumRu 以下
の名前空間に納められているモジュールを, NumRu:: を省略して呼ぶことができます. 

6-9 行目でテストデータを GPhys オブジェクトとして開いています. ただし,
実際にメモリに読み込まれるのは演算を行う部分(13-14 行目)で, この時点では指定さ
れたファイル中の変数を指しているだけです. ここでは gp_u, gp_v, gp_omega, gp_t
それぞれに東西風速, 南北風速, 鉛直風速, 気温が割り当てられています.


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

15 行目で GPhys::EP_Flux::div_sphere を呼んで EP Flux の発散を計算しています.
div_sphere は子午面上の y-z 平面における発散を計算するメソッドで, 引数は EP_Flux
以外の物理量にも利用できます.

17-21 行目はイテレータを用いて戻り値の全てのデータ部分の属性 "valid_hoge" を
削除しています. "valid_hoge" は有効範囲に関する属性で, オリジナルのデータに
あった属性です. GPhys は属性値は陽に指定しない限り, 最初に読みこんだでーたを
覚えているので, 演算した結果変化する場合(今回の有効範囲がそれにあたりますね)
に不都合が生じてしまいます. 非常に広い範囲が与えられていない限り, この値は
変更されなくてはなりません. 将来的に GPhys 自身がこの問題を対処します.

23-30 行目で, 計算結果を NetCDF ファイルとして保存します. 23 行目で保存ファイル
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

まとめ - 基本的な使い方 

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
      
===より効率的な使い方

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
ではどうすれば効率的なのでしょうか? EP_Flux は, とある固定された時刻にお
けるデータに対して処理をします. 今回のテストデータは時間方向に 120 個
の格子点を持っていますので, 時間軸に沿ってループを回すと良いでしょう.
そうすれば計算に必要なデータだけ適宜メモリ上に読み出し, 計算が終り次第
ファイルに書き出す. そして次のデータを読み込む...というようにしていくと
メモリの節約になります. 本節では上記の効率化を GPhys::IO のイテレータを
用いて実装する方法を紹介します. 以下のプログラムをご覧ください.

((<demo_NCEP_2.rb|URL:http://www.gfd-dennou.org/arch/ruby/products/ep_flux/tutorial/demo/demo_NCEP_2.rb>)):
      1  require 'numru/gphys'
      2  require 'numru/gphys/ep_flux'
      3  include NumRu
      4
      5  datadir = "./testdata/"
      6  gp_u =     GPhys::IO.open( datadir+"UWND_NCEP.nc",  "uwnd")
      7  gp_v =     GPhys::IO.open( datadir+"VWND_NCEP.nc",  "vwnd")
      8  gp_omega = GPhys::IO.open( datadir+"OMEGA_NCEP.nc", "omega")
      9  gp_t =     GPhys::IO.open( datadir+"TEMP_NCEP.nc",  "temp")
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
      23   epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
      24
      25   ary.each{|gp|                                  #  This part will not
      26     gp.data.att_names.each{|nm|                  #  be needed in future.
      27       gp.data.del_att(nm) if /^valid_/ =~ nm     #  (Even now, it is not
      28     }                                            #  needed if the valid
      29   }                                              #  range is wide enough)
      30
      31   if i==1    # time independent => write only once
      32     gp_lat.rename('phi')
      33     GPhys::IO.write(ofile, gp_lat)
      34     GPhys::IO.write(ofile, gp_z)
      35   end
      36   [ epflx_y, v_rmean, w_rmean, epflx_z, epflx_div, u_mean, theta_mean,
      37     uv_dash, vt_dash, uw_dash, dtheta_dz ]
      38 }
      39
      40 ofile.close

このプログラムは前節のデモプログラム (demo_NCEP_1.rb) 同様に, テストデータから
子午面上の EP Flux 他を計算し, nc ファイルに保存するものです. ただし, 13 行目以降
が大きく異なります. 先に述べた計算の効率化を図るため, GPhys のイテレータを利用
しています. 

1-11 行目は demo1 と同じくライブラリおよびデータの読み込みと保存ファイルのオープンを
行っています.

13-14 行目は後述のイテレータ部分の処理の進行度合いを出力するための前準備です.
13 行目 (nt = gp_u.shape[-1]) は gp_u の一番最後の次元のグリッド数を返します. このテスト
データでは時間 time のグリッド数 120 を nt に代入します.

15-38 行目は demo1 の 11-28 行目をイテレータを用いて実装しています.
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
      23   epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
      ...
      36   [ epflx_y, v_rmean, w_rmean, epflx_z, epflx_div, u_mean, theta_mean,
      37     uv_dash, vt_dash, uw_dash, dtheta_dz ]
      38 }


15 行目をご覧下さい. GPhys::IO.each_along_dims_write の第一引数 (ここでは [gp_u, ..., gp_t]) には
イテレータ中で処理する GPhys オブジェクトを要素に持つ配列, もしくは GPhys
オブジェクト自身を渡します. 第 2 引数には保存対象のファイルオブジェクトを渡します.
第 3 引数以降はループを回す次元を名前もしくは数字で指定します. ここでは一番最後の
次元である time を割り当てます. 

16 行目では gp_u, gp_v, gp_omega, gp_t を時間軸に沿ってスライスしたオブジェクトを
それぞれ u, v, omega, t としてイテレータ中に渡しています. 

20-23 行目は demo1 同様 EP_Flux::ep_full_sphere を呼び出して各変数の計算を行っています.
唯一異なるのは, 与えた引数が時間軸に沿ってスライスされたオブジェクトである点です.

36-37 行目で, 保存対象ファイル ofile に保存する変数を要素に含む配列を明記して
イテレータのブロックを閉じます. 時間に依存する変数, すなわち時間軸を持つ変数
のみ記載していることに注意してください. (時間に依存しない変数, ここでは gp_lat 等, は
ループ毎に上書きすると無駄なので, 31 - 35 行目のように 1 回だけファイルに書き込むよう
工夫しています.)

40 行目でファイルのクローズを行っています. 

それではプログラムを実行してみましょう. 実行中の標準出力には以下のようになると思います.
18 行目にて進行状況をループの回数を用いて表現しているためです. この部分は決して必須では
ありませんが, 計算に時間のかかるプログラムにおいては実装したいですね.

  % ruby demo_NCEP_2.rb

    processing 7 / 120 ..
    processing 14 / 120 ..
    ...
    processing 120 / 120 ..

正常に終了したら ncdump を用いてヘッダを覗いてみましょう. demo_NCEP_1.rb が出力する nc
ファイルを同じであることがわかります (変数の順番は違うかもしれませんが, そこはお許し下さい).

以上でイテレータを用いたプログラムを紹介しました. イテレータを使えば, メモリの節約の他,
個々の変数は時間軸に沿ってスライスしたものを並列に処理していくことになるので, 出力効率
の改善が図れます. 裏を返せば,
巨大なデータを扱うときは, いっぺんにやるとメモリーを圧迫すると同時に出力効率も下がる可能性
があるので, イテレーターを使えということです. というわけで本節のまとめです. 

((:<b>:))
((:<font color=rgb(87,153,51)>:))

まとめ - より効率的な使い方 

(1) 巨大データはイテレータを使って処理
    * 使い方はGPhys::NetCDF_IO.each_along_dims_write(gphyses, ofile, dims)
(2) GPhys::EP_Flux を用いる場合, 時間軸を持つデータに有効
  
((:</font>:))
((:</b>:))

    
===簡単お絵描き

本節ではいよいよ GPhys::EP_Flux で出力される物理量を絵にしてみましょう. EP Flux を
描く場合にはベクトル図がベストです. ここでは EP Flux をベクトル, ダイバージェンスを
コンターおよびトーンで重ねがきするプログラムを紹介します. 今までのプログラムよりも長
いですが, 01-37 行目はお作法およびデータの読み込み部分で, 重要なのは後半部分ですので
ご安心下さい.

((<demo_NCEP_3.rb|URL:http://www.gfd-dennou.org/arch/ruby/products/ep_flux/tutorial/demo/demo_NCEP_3.rb>)):
      1  require 'numru/gphys'
      2  require 'numru/gphys/ep_flux'
      3  require 'numru/gphys/ggraph_on_merdional_section'
      4  include NumRu
      5
      6  epflx_fnm = './epflx_NCEP.nc'
      7 
      8  if File::exist?(epflx_fnm)
      9 
      10   epflx_y =   GPhys::IO.open(epflx_fnm,  'epflx_y')
      11   epflx_z =   GPhys::IO.open(epflx_fnm,  'epflx_z')
      12   epflx_div = GPhys::IO.open(epflx_fnm,  'epflx_div')
      13   v_rmean =   GPhys::IO.open(epflx_fnm,  'v_rmean')
      14   w_rmean =   GPhys::IO.open(epflx_fnm,  'w_rmean')
      15
      16 else
      17
      18   datadir = "./testdata/"
      19   gp_u =     GPhys::IO.open( datadir+"UWND_NCEP.nc",  "uwnd")
      20   gp_v =     GPhys::IO.open( datadir+"VWND_NCEP.nc",  "vwnd")
      21   gp_omega = GPhys::IO.open( datadir+"OMEGA_NCEP.nc", "omega")
      22   gp_t =     GPhys::IO.open( datadir+"TEMP_NCEP.nc",  "temp")
      23
      24   epflx_y, epflx_z, v_rmean, w_rmean= ary =
      25     GPhys::EP_Flux::ep_full_sphere(gp_u, gp_v, gp_omega, gp_t, true)
      26
      27   epflx_div = GPhys::EP_Flux::div_sphere(epflx_y, epflx_z)
      28
      29   ary.each{|gp|                                  #  This part will not
      30     gp.data.att_names.each{|nm|                  #  be needed in future.
      31       gp.data.del_att(nm) if /^valid_/ =~ nm     #  (Even now, it is not
      32     }                                            #  needed if the valid
      33   }                                              #  range is wide enough)
      34
      35 end
      36
      37
      38 DCL.gropn(1)
      39 DCL::sglset('LFULL', true)                       # use full area
      40 DCL::slrat(1.0, 0.85)                            # set aspect ratio of drwable area
      41 DCL.sgpset('lcntl', false)                       # don't rede control character.
      42 DCL.sgpset('lfprop',true)                        # use prportional font
      43 DCL.uzfact(0.6)                                  # set character size
      44
      45 ## show vector (Fy, Fz)
      46 # monthly mean
      47 fy = epflx_y.mean('time')
      48 fz = epflx_z.mean('time')
      49 epdiv = epflx_div.mean('time')
      50
      51 GGraph::set_fig('view'=>[0.15, 0.85, 0.25, 0.55])
      52 GGraph::set_fig('itr'=>2)                        # contour && tone only
      53 GGraph::contour(epdiv)                           # contour
      54 GGraph::tone(epdiv, false)                       # tone
      55 GGraph::vector_on_merdional_section(fy, fz, false,
      56    'fact'=>3.0,'xintv'=>1,'unit'=>true, 'annot'=>false
      57                                     )
      58
      59 ## show residual mean merdional circulation (cut nearly surface and Antarctica)
      60 vrm = v_rmean.cut('lat'=>-70..90, 'level'=>850..100).mean('time')
      61 wrm = w_rmean.cut('lat'=>-70..90, 'level'=>850..100).mean('time')
      62 GGraph::vector_on_merdional_section(vrm, wrm, true,
      63                         'fact'=>5.0,'xintv'=>1,'unit'=>true, 'annot'=>false
      64                                     )
      65 DCL.grcls

実行するとまず以下の絵が画面に描画されます.

((:<center><A HREF="./epflx.png"><IMG SRC="epflx.png" HEIGHT=300 WIDTH=400></A></center>:))

スペースキーを打鍵するか絵をクリックすると次の絵が出力します.

((:<center><A HREF="./residual.png"><IMG SRC="residual.png" HEIGHT=300 WIDTH=400></A></center>:))

1-4 行目はお約束ですね. 今までと異なるのは 3 行目(require 'numru/gphys/ggraph_on_merdional_section')
が追加されていることです. GPhys には GPhys オブジェクト専用の描画モジュール GGraph が用意されています.
GGraph を利用するには通常 'numru/ggraph' を require するわけですが, このプログラムでは子午面上のフラックス
ベクトルを描くのに特化したメソッド(GGraph::vector_on_merdional_sphere)を利用して EP Flux を表現します.
そのメソッドが追加定義されているファイルが 'numru/gphys/ggraph_on_merdional_section' です.

6-35 行目では, demo_NCEP_1.rb もしくは demo_NCEP_2.rb を実行した際に出力される epflx_NCEP.nc が
カレントディレクトリに存在するか否かで場合分けしています. 存在する場合はそちらから必要な変数を
オープンします. ここでは EP Flux の 緯度成分および鉛直成分(epflx_y, epflx_z)とその発散(epflx_div),
そして残差風速(v_rmean, w_rmean) を開きます. もし epflx_NCEP.nc が無い場合は demo1 or 2 同様に
テストデータを元に計算します. このチュートリアルに沿ってデモプログラムを動かした方は既に
EP Flux etc. を計算していると思いますので, 無駄を省くためにこのような場合分けをしてあります.

38-43 行目は絵を描くための下準備です. 38 行目 DCL.gropn(1) は、DCL のグラフィック装置初期化です.
引数が 1 の場合, 端末画面に表示されます. 通常必要なのはこの行のみで後はユーザの好みによります. 
39, 40 行目は全画面出力を行い, 描画領域の縦横比を1:0.85 で指定しています. 41 行目でアンダースコア
を制御文字として解釈しないようにし, 42 行目でフォントにプロポーショナルフォントを用い,
43 行目で座標軸につける文字の大きさを 0.6 倍にしています.

47-49 行目では変数を時間軸に沿って平均しています. EP_Flux::ep_full_sphere  および EP_Flux::div_sphere
が出力する変数は時間方向に元のデータと同じ格子点数のグリッドを持っているので, ここでは 2001 年 4 月平均値を
算出することになります.

51-64 行目でいよいよ絵を描きます. 51 行目で U 座標を V 座標に対応させて, 52 行目で y 軸を
ログスケールでプロットすることにします. 53, 54 行目で EP Flux の発散のトーンおよびコンターを描画します.
なお重ね描きするために GPhys::contour の 2 番目の引数では改ページしないよう false が指定されています.
55-57 行目(GGraph::vector_on_merdional_sphere)で EP Flux のベクトルを描きます. 同様に 60-61 行目で
残差風速の平均値を求め 62-64 行目でベクトルで表現しています. ただし地表面付近および南極大陸
に相当する領域はエラーが大きいため意図的にカットしてあります(60-61 行目). 

GGraph::vector_on_merdional_sphere について詳しく解説します. 先に述べたとおり, このメソッドは
子午面上の「流れ」を表すベクトルに対して適切なスケーリングを行い出力するものです. 

..now under construction..


===計算精度

..now under construction..

===数理モデル

..now under construction..

===F&Q

+ Q1. 出力される GPhys オブジェクトの軸はどうなっていますか?

A1.

..now under construction..


1 次元目に時間, 2 次元目に緯度, 3 次元目に高度が入っている GPhys オブジェクトを入力するにはどうすればいよいですか?
(前の質問の続き) 注意!!! 全ての GPhys オブジェクトのグリッドは同じでないといけません.
例えば, 東西風速は 1000 mb から 10 mb で定義されているけど, 鉛直風は 100 mbだと,
計算時にエラー終了します. またあるオブジェクトは 0 次元目が緯度だけど別のオブジェクトは時間だ, というのもいけません.
GPhys::EP_Flux では東西風速のグリッがを全てのオブジェクトのグリッドだと思う仕様になっているので,
計算自体はできますが値がおかしくなります. 入力オブジェクトには充分注意を払ってください.

==開発編

..now under construction.. 

=end
