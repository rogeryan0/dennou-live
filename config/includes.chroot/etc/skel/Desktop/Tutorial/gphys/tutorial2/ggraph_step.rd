# GPhys, GGraph チュートリアル / 可視化の第一歩
#
# = 履歴 (新しいものは上から追記)
#
# * 2012/02/29 堀之内武  ggraph_hop.rd から分離，大幅改訂・増補

= GPhys, GGraphチュートリアル (その3)

== 可視化の第二歩

引き続き irb を使ってお絵描きします．
いろんな断面やズームアップ，簡単な操作（平均，引き算等）を体験します．
描画のオプションも紹介しますが，まだ見てくれの細部にはこだわらず，所望の絵を素早く描くことを重視します．

=== ディレクトリやファイルの中身を知る

改めて irb を立ち上げ (('ls')) と入力してみましょう．

  $ irb_ggraph 
  irb(main):002:0> ls
  Directories:
    'air.2012-01.nc/'
    'hgt.2012-01.nc/'
    'ncep2.Jan.clim.1981-2010.nc/'
  => nil

NetCDFファイルがディレクトリとして扱われていることがわかります．
そこで，(('ls 'air.2012-01.nc'')) や (('ls_l 'air.2012-01.nc'')) 
などと入力してみましょう．

  irb(main):005:0> ls 'air.2012-01.nc/'
  Data:
    'lon'
    'lat'
    'level'
    'time'
    'air'
  => nil
  irb(main):006:0> ls_l 'air.2012-01.nc/'
  Data:
    lon	[lon=144]	'Longitude'	(degrees_east)
    lat	[lat=73]	'Latitude'	(degrees_north)
    level	[level=17]	'Level'	(millibar)
    time	[time=31]	'Time'	(hours since 1-1-1 00:00:0.0)
    air	[lon=144,lat=73,level=17,time=31]	'mean Daily Air temperature'	(degK)
  => nil
  irb(main):007:0> ls_l 'ncep2.Jan.clim.1981-2010.nc/'
  Data:
    lon	[lon=144]	'Longitude'	(degrees_east)
    lat	[lat=73]	'Latitude'	(degrees_north)
    level	[level=17]	'Level'	(millibar)
    air	[lon=144,lat=73,level=17]	'Monthly Air Temperature on Pressure Levels'	(degK)
    hgt	[lon=144,lat=73,level=17]	'Monthly Geopotential Heights on Pressure Levels'	(m)
    mslp	[lon=144,lat=73]	'Monthly Mean Sea Level Pressure'	(Pascals)
    uwnd	[lon=144,lat=73,level=17]	'Monthly U-wind on Pressure Levels'	(m/s)
    vwnd	[lon=144,lat=73,level=17]	'Monthly V-wind on Pressure Levels'	(m/s)
  => nil

これで (ncdump を使わなくてもコンパクトな形で),
例えば air.2012-01.nc 中の変数 air は
lon, lat, level, time という名前の次元をもつ 4次元の変数であることや, 
各次元の長さ (144, 73, 17, 31) がわかります．

ls や ls_l は irb_ggraph で読み込んだ
((<irbrc_ggraph.rb|URL:irbrc_ggraph.rb>)) 
に定義されています．他に cd (カレントディレクトリの変更) や pwd (カレントディレクトリの表示)
があります．


=== いろんな断面

今度は日平均気温を tmp という変数名で開いてみましょう
（変数名は好きにつけられますが，小文字ではじめてください．-- 
大文字で始まるのは「定数」となります）．

  irb(main):001:0> tmp = gpopen 'air.2012-01.nc/air'
  => <GPhys grid=<4D grid <axis pos=<'lon' in '//home/username/air.2012-01.nc'  sfloat[144]>>
          <axis pos=<'lat' in '/home/username/air.2012-01.nc'  sfloat[73]>>
          <axis pos=<'level' in '/home/username/air.2012-01.nc'  sfloat[17]>>
          <axis pos=<'time' in '/home/username/air.2012-01.nc'  float[31]>>>
     data=<'air' in '/home/username/air.2012-01.nc'  sint[144, 73, 17, 31]>>

地図投影の設定をし，下から4番目の「高度」(700 hPaの気圧面)，
前から10番目の時刻(1月10日の日平均値)の断面を表示してみます．

  irb(main):002:0> set_fig "itr"=>32          # 正距方位図法
  irb(main):003:0> set_map "coast_world"=>true
  irb(main):006:0> tone_and_contour tmp[true,true,3,9]

ここでは (('tmp')) に (('[true,true,3,9]')) をつけています．
この四角括弧は，添字による部分「配列」の指定です．
第1,2引数の (('true')) は全選択を表します．
Ruby の配列の添字はゼロから始まりますので，
第3, 4次元の 4, 10 番の要素を指定するのにそれぞれ（1を引いて） 3, 9 と指定するのです．

結果：
((:<a href="img/ggraph_step_01.png">:))
((<"IMG:img/ggraph_step_01_s.png">))（クリックでフルサイズ表示）
((:</a>:))


今度は鉛直断面を書いてみましょう．
気圧座標の表示は対数スケールがよいですので，
投影法は2番（縦軸のみ対数座標）をとります
(参考：((<座標系の種類|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node7.html>)))．
同じ1月10日の経度140度での断面を表示しましょう．座標値での指定には次の用に
(('cut')) を使います．

  irb(main):005:0> set_fig "itr"=>2
  irb(main):006:0> tone_and_contour tmp[false,9].cut("lon"=>140)

（「ドットでつなげるってどういうこと？」と思われた方，それについては次の章で説明します．）

結果：
((:<a href="img/ggraph_step_02.png">:))
((<"IMG:img/ggraph_step_02_s.png">))（クリックでフルサイズ表示）
((:</a>:))


=== 描画オプション (2)

図のカスタマイズですが, 
描画メソッドによっては, set_fig のようなメソッドでなく, 
描画メソッドの引数によって変更することができます.
（もしもなぜそのような切り分けがあるか知りたければ次々節をご覧ください．）

  irb(main):007:0> set_fig "itr"=>32          # 再び正距方位図法に
  irb(main):008:0> tone tmp, true, "interval"=>5, "max"=>300, "min"=>230, "color_bar"=>true

tone の引数で必須なのは最初の一つ(GPhysオブジェクト)ですが，
オプションでさらに引数を与えることができます．
第2引数(ここでは true)は図の重ねがきに関する指定で，((<次節|重ねがき>))で解説します．
とりあえずはおまじないとして true を指定するもんだと思っておいてください．
第3引数からがオプション指定です（後でやりますが，実は (('"interval"=>5', ')) 
以降はすべてまとめて第3引数なのですが気にしないことにしましょう）．
オプションの interval, max, min は色付けの段階の間隔，最大値，最小値の指定です．
color_bar はこれまで tone のあと別メソッドとして呼んでましたが，上例のようにオプションにもできます．

結果：
((:<a href="img/ggraph_step_03.png">:))
((<"IMG:img/ggraph_step_03_s.png">))（クリックでフルサイズ表示）
((:</a>:))

interval の代わりに nlev でおよそのレベル数を指定することもできます：

  irb(main):008:0> tone tmp, true, "nlev"=>20   # レベル数を約20に．間隔はお任せ．


ともかくオプションを指定するときは, 第2引数は省略できないので注意してください.
忘れるとこんなエラーメッセージがでます．

  irb(main):010:0> tone tmp, "interval"=>5   # 第2引数を忘れてエラーになる例
   *** MESSAGE (SWDOPN) ***  GRPH1 : STARTED / IWS =  1.                         
  ArgumentError: 2nd arg (newframe) must be true or false
          from /usr/lib/ruby/1.8/numru/ggraph.rb:2619:in `tone'
          from (irb):10
          from /usr/bin/irb:12:in `<main>'


どのようなオプションがあるかについては，
GGraph の リファレンスマニュアル
(((<英語|URL:http://ruby.gfd-dennou.org/products/gphys/doc/ggraph.html>)),
((<日本語訳|URL:http://w.livedoor.jp/gphys/d/module%20NumRu%3a%3aGGraph>)))
を参照してください．

描画オプションは help オプションを使って対話的に調べることもできます．

 irb(main):001:0> tone nil, true, "help"=>true
  *** MESSAGE (SWDOPN) ***  GRPH1 : STARTED / IWS =  1.                         
 << Description of options >>
   option name	default value	# description:
   "title"	nil	# Title of the figure(if nil, internally
                         # determined)
   "annotate"	true	# if false, do not put texts on the right
                         # margin even when newframe==true
   "ltone"	true	# Same as udpack parameter ltone
   "auto"	false	# Swith DCL.uetone and DCL.uetonf depending on the
                         # data size
   "tonf"	false	# Use DCL.uetonf instead of DCL.uetone
   ..(後略)..

((:<div class=likepre>:))
((<((*（toneのオプションの全体をみる）*))|URL:ggraph_step_sub1.htm>))
((:</div>:))

オプションの前には常に2つ引数が必要なことに注意してください．
第一引数は通常は GPhys データですが，help オプションを使う場合ダミーで構いません．
この例では nil (なんでもないという定義済みオブジェクト) を指定しました．
第2引数は常に true または false を与える必要があります．

次に日本付近を拡大表示してみましょう．地図投影法は正距円筒図法に切り替え，
海岸線データは全球をカバーするけど分解能がやや荒い coast_world から，
日本付近を細かくカバーする coast_japan に切り替えて，
経度緯度範囲を絞って表示します．

  irb(main):004:0> set_fig "itr"=>10          # 正距円筒図法
  irb(main):005:0> set_map "coast_japan"=>true, "coast_world"=>false
  irb(main):009:0> tone tmp.cut("lon"=>120..150,"lat"=>20..50)
  irb(main):009:0> color_bar

((:<a href="img/ggraph_step_04.png">:))
((<"IMG:img/ggraph_step_04_s.png">))（クリックでフルサイズ表示）
((:</a>:))

==== 演習問題

(1) 'interval' を使ってトーン・コンターの間隔を設定してみましょう. 
    このとき, トーンの間隔とコンターの間隔は異なる値にし,
    かつ重ねがきしてみましょう. 

(2) 正距円筒図法 ("itr"=>10) を使って1月10日の 500 hPa の気温を,
    経度範囲 90～270 の範囲(緯度は南極から北極まで)で描いてみましょう
    （縦横とも180度の範囲なので正方形の図になるはずです）．

=== 重ねがき

図は重ねがきすることができます. 次を実行してみてください．

  irb(main):007:0> tone tmp[true,true,0,10] - tmp[true,true,0,9], true, "title"=>"T & its tendency"
                   #↑最下層(1000 hPa)での1月11日と10日の差を色で
  irb(main):009:0> color_bar                                     # カラーバー
  irb(main):008:0> contour tmp[true,true,0,9], false             # 1月10日の1000 hPaの気温をコンターで

GGraph の描画メソッドはオプションで第2引数を与えることができます．
上の例の3行目のように false を与えるとこれまでの描画の上に重ねて絵が書かれます．
その際，座標軸やタイトルは再描画しません．
一方，true (既定値)を与えたり省略した場合はページやフレームを改めて新たに描画します．
図のタイトルは最初の描画時 (tone ...) に書かれるので，もしも図を取っておくのであれば,
後でわかりやすいタイトルをつけるといいでしょう ("T & its tendency" としました)．

結果：
((:<a href="img/ggraph_step_05.png">:))
((<"IMG:img/ggraph_step_05_s.png">))（クリックでフルサイズ表示）
((:</a>:))

中緯度で等温線が波打っているところで気温が大きく増減していることがわかります．

==== 演習問題

(1) tone と contour の様々な重ね書きをやってみましょう．

=== GGraphの構造とオプション（難：とばして構いません）

実は 
(('set_fig')) で指定するオプションは，(('fig')) というメソッドのオプションなのです
（同様に (('set_map')) は (('map')) のオプション指定，まだ出てませんが
(('set_axes')) という (('axes')) のオプション指定もあります）．
(('contour')) 等は内部で必要に応じて (('fig')) を呼ぶのですが，(('contour')) 
には (('fig')) のオプションは直接は渡せないので前もって (('set_fig')) 
で指定するという構造になってます．これらの切り分けは，

* (('fig')) : 図の基本構造を決める
* (('axes')) : 図の縦軸横軸を司る；
* (('map')) : 地図投影に関することを司る

です．そして

* (('contour')) や (('tone')) など個別の描画メソッドのオプションはは，
  それぞれの描画に関わることを決める

というようになっています．前述の title などのオプションは 
(('fig')) や (('axes')) に関わりそうに思えますが，
陽に指定されない場合のタイトルは描画対象の名前からとりますので
個別の描画メソッドのオプションになっています．

ここで一旦 irb のセッションを一区切りしましょう．
いちおう入力内容は保存しましょう（((<参考|URL:ggraph_hop.htm>))）．

  irb(main):022:0> history_save
  irb(main):022:0> exit


=== ベクトル描画

また irb を立ち上げましょう．

  $ irb_ggraph 

今度は1月の気候値 (1981年から2010年までの30年平均値) を読み込みます．

  irb(main):008:0> tc = gpopen 'ncep2.Jan.clim.1981-2010.nc/air'    # 気温
  irb(main):009:0> zc = gpopen 'ncep2.Jan.clim.1981-2010.nc/hgt'    # 高度
  irb(main):010:0> uc = gpopen 'ncep2.Jan.clim.1981-2010.nc/uwnd'   # 東西風
  irb(main):011:0> vc = gpopen 'ncep2.Jan.clim.1981-2010.nc/vwnd'   # 南北風

正距円筒図法(番号10)で850 hPaの全球の風速を矢印で表示します．
横長の図になるので，少し "viewport" を広げましょう
（参考：DCLのマニュアルの((<正規変換パラメタ|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node9.html>))）．

  irb(main):036:0> set_fig "itr"=>10, "viewport"=>[0.05, 0.85, 0.3, 0.7]
  irb(main):037:0> set_map "coast_world"=>true
  irb(main):038:0> p = 850
  irb(main):039:0> vector uc.cut("level"=>p), vc.cut("level"=>p), true, "xintv"=>4, "yintv"=>4, "unit"=>true

ここでは共通の気圧面を取るため変数を使いました．(('p=850')) とした上で，u, v 
共通に cut しています．格子点数が多いので，オプションの "xintv", "yintv" で間引き，
オプション "unit" で欄外に矢印の長さスケールを表示することを指定しています．
なお，"unit" は，正式名称 "unit_vect" の略です．
((* GGraph のオプションは，紛れのない範囲で後ろを省略できます．*))

結果：
((:<a href="img/ggraph_step_06.png">:))
((<"IMG:img/ggraph_step_06_s.png">))（クリックでフルサイズ表示）
((:</a>:))

今度は等圧面高度を色でかいて，その上に先ほどのベクトルを重ねてみましょう．

  irb(main):025:0> tone zc.cut("level"=>p)
  irb(main):026:0> vector uc.cut("level"=>p), vc.cut("level"=>p), false, "xintv"=>4, "yintv"=>4, "unit"=>true
  irb(main):027:0> color_bar "vcent"=>0.5, "vlen"=>0.25

ここでは細かい工夫があります．デフォルトに任せてカラーバーを書くと,
ベクトルスケール表示と重なってしまいますので，
color_bar メソッドのオプションで中心の位置と長さを指定しています
（0～1でスケールされたビューポートのY軸について）．

結果：
((:<a href="img/ggraph_step_07.png">:))
((<"IMG:img/ggraph_step_07_s.png">))（クリックでフルサイズ表示）
((:</a>:))

矢印はだいたい高度の等値線に沿ってますが（地衡風）が所々大きく向きがずれています．

==== 以下やや難

今度は上空の 200 hPa 気圧面で描いてみます．
地図投影はポーラーステレオに変えてみましょう（これでベクトルが表示できるのは GPhys 1.2 以降に限られます）．
この図法では南極が無限遠に投影されます．緯度範囲を限らない（であえてやります）と画面一面に表示されます．そうならないよう，DCL でクリッピングを指定します
（参考：DCLのマニュアルの((<補間とクリッピング|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node22.html>))）．

  irb(main):006:0> p=200
  irb(main):006:0> DCL.sgpset("lclip", true)
  irb(main):013:0> set_fig "itr"=>31, "viewport"=>[0.12, 0.82, 0.15, 0.85]
  irb(main):017:0> tone zc.cut("level"=>p), true, "title"=>"Z & wind"
  irb(main):018:0> vector uc.cut("level"=>p), vc.cut("level"=>p), false, "xint"=>3, "yint"=>3, "factor"=>2, "unit"=>true 
  irb(main):027:0> color_bar "vcent"=>0.5

結果：
((:<a href="img/ggraph_step_08.png">:))
((<"IMG:img/ggraph_step_08_s.png">))（クリックでフルサイズ表示）
((:</a>:))

vector のオプション factor は，お任せできまる矢印の長さを定数倍します（実数）．
お任せでは矢印同士がほぼ重ならないスケールに調整されますが，2倍したので若干重なってます．

# はまりそうなので一旦コメントアウト.  余裕があるときに見直す.
#(2) (やや難しい) 描画メソッドの第三引数以降は複数書くことが出来ます.
#    GPhys リファレンスマニュアル
#    (
#    ((<"英語"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
#    ((<"日本語訳"|URL:http://w.livedoor.jp/gphys/>))
#    ) を参考に, (1) の結果に加えて
#    好きな最大値・最小値を設定してみましょう.
#    (ヒント: ここでも GGraph で検索し, 行った先のページで "maximum"
#    (日本語版なら"最大") で検索すると...)

# 複数時間のデータがある場合はここで何かできそう.

=== 出力先を変更する

さて, 好きな図を描いたところで, 
図をファイルに保存してみましょう.
今の出力先は X になっているので, 
postscript ファイルに出力するように切り替えてみます. 

  DCL.grcls  # 今の窓を閉じる

このままではプロンプトが帰ってこないので, 
図をクリックして消します.

出力先を postscript ファイルに変更して出力します.

  DCL.gropn(2)   # 出力装置番号を 2 番 (postscript ファイル) に変更. ファイルオープンに相当.
  tone(gp)       # 出力. この通りでなくてもよい.
  DCL.grcls      # 装置を閉じる. ファイルクローズに相当.

ファイルを見る前に, 最初の状態に戻す練習をしておきましょう.

  DCL.gropn(1)  # X は出力装置 1 番.

とします. irb を立ち上げた時のように, 白い画面が現れます.

ファイルを確認するために一旦 irb から抜けます.
別の端末を上げても構いません.

  exit

カレントディレクトリで ls すると dcl.ps というファイルができているはずです.

  $ ls

どういう絵ができているか確認してみましょう.

  $ gv dcl.ps

出力先ファイル名 dcl.ps は固定なので, 
間違えて上書きしないように好きな名前に変えておきましょう.

  $ mv dcl.ps practice01.ps


=== ドキュメントについて

ここでは練習のためにリファレンスマニュアルを参照しましたが, 
比較的単純な場合は
((<"GPhys/GGraph チートシート"|URL:http://davis.gfd-dennou.org/rubygadgets/ja/?%28Others%29+GPhys%2FGGraph+%A5%C1%A1%BC%A5%C8%A5%B7%A1%BC%A5%C8>))
を参照してもよいでしょう.

=== この章で引用した資料

* DCL : ((<座標系の種類|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node7.html>))

* GGraph の リファレンスマニュアル (GPhys のマニュアル内)
  (((<英語|URL:http://ruby.gfd-dennou.org/products/gphys/doc/ggraph.html>)),
  ((<日本語訳|URL:http://w.livedoor.jp/gphys/d/module%20NumRu%3a%3aGGraph>)))

=== さらなる参考資料

* ((<"GPhys/GGraph チートシート"|URL:http://davis.gfd-dennou.org/rubygadgets/ja/?%28Others%29+GPhys%2FGGraph+%A5%C1%A1%BC%A5%C8%A5%B7%A1%BC%A5%C8>))
* GPhys リファレンスマニュアル
  (
  ((<"英語"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
  ((<"日本語訳"|URL:http://w.livedoor.jp/gphys/>))
  )
  * その中の GGraph の リファレンスマニュアル
    (((<英語|URL:http://ruby.gfd-dennou.org/products/gphys/doc/ggraph.html>)),
    ((<日本語訳|URL:http://w.livedoor.jp/gphys/d/module%20NumRu%3a%3aGGraph>)))

* ((<"RubyDCL ドキュメント"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/>))
* ((<"いろいろな地図投影法"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/rakuraku/node61.html>)) (RubyDCL ドキュメント内)
* ((<"DCL colormaps"|URL:http://www.gfd-dennou.org/library/dcl/dcl-5.4.2/src/env1/colormap/colormap_gallery.html>)) (DCL ドキュメント内)

