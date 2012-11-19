== はじめる前に

本チュートリアルを始める前の準備です．

==== 概要

* GPhys/GGraph用スタートアップファイル ((<irbrc_ggraph.rb|URL:irbrc_ggraph.rb>)) をダウンロードし，irb を立ち上げてみます．
* サンプルとして使う気象データ
  ((<ncep2.Jan.clim.1981-2010.nc|URL:ncep2.Jan.clim.1981-2010.nc>)),
  ((<air.2012-01.nc|URL:air.2012-01.nc>)),
  ((<hgt.2012-01.nc|URL:hgt.2012-01.nc>))
  をダウンロードします．

=== _

=== インストール

GPhys は多次元データ解析可視化ライブラリです．GGraph は GPhys 付属の可視化ライブラリです．

このチュートリアルは，GPhys はインストールしてあるものとして進めます．
もしもまだなら ((<GPhysホームページ|URL:http://ruby.gfd-dennou.org/products/gphys/>))
を参考にインストールしてください．
##なお LiveDVD にはインストール済みです．

=== Ruby の実行

Ruby で書くプログラムは通常，.rb という拡張子で終わるファイルに保存し，
ruby コマンドで実行します．例えば hoge.rb という名前のファイルに保存したプログラムは，
コマンドライン端末で

  $ ruby hoge.rb

と実行します．ここではプロンプトを $ で表しました（以後も同様に表します）．

他の方法として，次のように対話的インタープリターによる実行もできます．

=== irb について

irb は Ruby の対話的インタープリターです．
本チュートリアルでは irb を多く使います．

コマンドライン端末で irb を立ち上げると,
1行入力する度にRubyコードとして実行され，結果が表示されます.
irb を立ち上げるには，で irb と入力します．
すると，入力待ちのプロンプト（(('irb(main):001:0>')) といった文字列）が表示されるので，とりあえず四則演算でもやってみましょう("(('1 + 2'))"がユーザーの入力です)．

  $ irb
  Irb(main):001:0> 1 + 2
  => 3

#(編集注) 上ではプロンプト先頭大文字化でキープ

==== irb の初期設定と立ち上げ方．

対話的可視化用設定ファイル ((<irbrc_ggraph.rb|URL:irbrc_ggraph.rb>)) をダウンロードしてホームディレクトリに置きます 
(ホームディレクトリは UNIX 系 OS では ~/ と表せます. 
Windows等では適当なフォルダーに置き，以下の ~/ はそれに読み替えてください).

この設定ファイルを読み込んで irb を立ち上げるには

  $ irb -r ~/irbrc_ggraph.rb

とします．毎回これをタイプするのは面倒ですので，次の 1, 2 のいずれかによって，もっと簡単に使えるようにしましょう．

(1) スタートアップファイルへの登録．

    ホームディレクトリに .irbrc というファイルを作り，中身に次の行を書きます．

      require "~/irbrc_ggraph.rb"

    すると，単にコマンドラインで irb と入力するだけで irbrc_ggraph.rb が読み込まれます．

(2) 別名の設定　(linuxなどUNIX系のOSのみ)

    もしも必要なときだけ irbrc_ggraph.rb を読み込むようにしたければ，別名を設定しましょう．
    例えば bash を使ってる場合，~/.bashrc に次の行を加えます．

       alias irb_ggraph="irb -r ~/irbrc_ggraph.rb"

    これで，

      $ irb_ggraph

    とすれば irbrc_ggraph.rb が読み込んで irb が始まります.
    別名を登録しておけばコマンド名のタブ補完が効きますので，名前を全部覚えてなくても大丈夫です.

本チュートリアルでは (2) の方法を使います．(1) の方法を使う場合，
(('$ irb_ggraph')) と書いてあれば (('$ irb')) と読み替えてください．

=== サンプルデータのダウンロード

本チュートリアルでは次のデータを使います．
ダウンロードして適当なディレクトリー（フォルダー）においてください．
チュートリアルはすべてそのディレクトリーで実行します．

* ((<ncep2.Jan.clim.1981-2010.nc|URL:ncep2.Jan.clim.1981-2010.nc>)) : 1981年から2010年までの30年間のデータより得られた月毎の気候値（「平年値」）です．収録物理量は次です（コロンの前はファイル中の変数名です）．
  * (('mslp')) : 海面更正気圧 (Pa)
  * (('air')) : 気温 (K)
  * (('hgt')) : 等圧面高度 (m)
  * (('uwnd')) : 東西風 (m/s)
  * (('vwnd')) : 南北風 (m/s)
* ((<air.2012-01.nc|URL:air.2012-01.nc>)) : 2012年1月の気温の日平均値(K)です．収録変数名は (('air'))
* ((<hgt.2012-01.nc|URL:hgt.2012-01.nc>)) : 2012年1月の等圧面高度の日平均値(m)です．収録変数名は (('hgt'))

これらは ((<NCEP-DOE Rananalysis2|URL:http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis2.html>))
による客観解析(再解析)データで，ファイル形式は ((<NetCDF|URL:http://www.unidata.ucar.edu/software/netcdf/>)) です．

いずれも経度緯度2.5度の等間隔データで，mslp 以外は鉛直には気圧座標のデータとなります(1000, 925, 850, 700, 600, 500, 400, 300, 250, 200, 150, 
100, 70, 50, 30, 20, 10 hPa の17層)．1000 hPa などは「地面の下」（つまり実際にはその気圧の箇所は存在しない）ところも多いですが，その場合補外となっています（だから南極やチベット高原でも 1000 hPa の「気温」が定義されています）．

これらのファイルは上記のサイトから取得できるデータを ((<こちら|URL:tool/>)) 
に置いた Ruby プログラムで加工して作りました（GPhysを使っています．なお1月の気候値のみの切り出しは対話的に行いました）．

=== NetCDF データの内容確認

コマンドラインで NetCDF データの内容を確認するには ncdump コマンドを使います
（オプション -h をつけます）．以下で2行目以降は ncdump の出力です．

  $ ncdump -h air.2012-01.nc 
  netcdf air.2012-01 {
  dimensions:
          lon = 144 ;
          lat = 73 ;
          level = 17 ;
          time = 31 ;
  variables:
          float lon(lon) ;
                  lon:units = "degrees_east" ;
                  lon:long_name = "Longitude" ;
                  lon:actual_range = 0.f, 357.5f ;
                  lon:standard_name = "longitude" ;
                  lon:axis = "X" ;
          float lat(lat) ;
                  lat:units = "degrees_north" ;
                  lat:actual_range = 90.f, -90.f ;
                  lat:long_name = "Latitude" ;
                  lat:standard_name = "latitude" ;
                  lat:axis = "Y" ;
          float level(level) ;
                  level:units = "millibar" ;
                  level:actual_range = 1000.f, 10.f ;
                  level:long_name = "Level" ;
                  level:positive = "down" ;
                  level:GRIB_id = 100s ;
                  level:GRIB_name = "hPa" ;
                  level:axis = "Z" ;
          double time(time) ;
                  time:units = "hours since 1-1-1 00:00:0.0" ;
                  time:long_name = "Time" ;
                  time:actual_range = 17628096., 17629344. ;
                  time:delta_t = "0000-00-01 00:00:00" ;
                  time:avg_period = "0000-00-01 00:00:00" ;
                  time:standard_name = "time" ;
                  time:axis = "T" ;
          short air(time, level, lat, lon) ;
                  air:long_name = "mean Daily Air temperature" ;
                  air:unpacked_valid_range = 150.f, 350.f ;
                  air:actual_range = 185.3f, 314.275f ;
                  air:units = "degK" ;
                  air:add_offset = 477.66f ;
                  air:scale_factor = 0.01f ;
                  air:missing_value = 32766s ;
                  air:precision = 2s ;
                  air:least_significant_digit = 1s ;
                  air:GRIB_id = 11s ;
                  air:GRIB_name = "TMP" ;
                  air:var_desc = "Air temperature" ;
                  air:dataset = "NCEP Reanalysis Daily Averages" ;
                  air:level_desc = "Multiple levels" ;
                  air:statistic = "Mean" ;
                  air:parent_stat = "Individual Obs" ;
                  air:valid_range = -32766s, -12766s ;
  
  // global attributes:
                  :Conventions = "COARDS" ;
                  :title = "mean daily NMC reanalysis (2012)" ;
                  :history = "created 2011/12 by Hoop (netCDF2.3)\n",
                          "2012-02-28 18:40:29 JST horinout> extractJan.rb wrote air" ;
                  :description = "Data is from NMC initialized reanalysis\n",
                          "(4x/day).  It consists of most variables interpolated to\n",
                          "pressure surfaces from model (sigma) surfaces." ;
                  :platform = "Model" ;
                  :references = "http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.html" ;
  }
