# GPhys, GGraph チュートリアル / 可視化の第一歩
#
# = 履歴 (新しいものは上から追記)
#
# * 2012/02/29 堀之内武  まだまだ編集中
# * 2012/02/26 堀之内武  編集開始（大幅変更）
# * 2012/02/18 納多哲史  新規作成 (hop_ggraph.rd として)

= GPhys, GGraphチュートリアル (その2)

== 可視化の第一歩

この章は，描画ライブラリである GGraph (GPhysの付属ライブラリ)を使った気象データの可視化のチュートリアルです.
Rubyの対話的インタープリター irb を使って，気象データの絵が簡単に描けることを体験します．

前もって ((<「はじめる前に」|URL:getting_started.htm>)) の章で紹介した準備をしておいてください．

((:<div class=summary>:))
==== この章の概要

* 最速2行で絵が描けることを体験します（データオープン1行，描画1行）．
* いくつかの種類の描画をやってみます（色塗り，コンター，折れ線...）．描画設定の初歩を体験します．
* あわせて次のようなことを学びます：エラーメッセージの読み方，ヒストリー機能 (使用するirbスタートアップファイルの機能)．
((:</div>:))

=== 最速描画

端末で irb を GPhys 用のスタートアップファイルを読み込んだ形で立ち上げます（((<「はじめる前に」|URL:getting_started.htm>)) 参照; (('~/.irbrc')) の設定をしてる場合は以下で irb_ggraph でなく irb と打ち込む）．

  $ irb_ggraph

まずは以下をプロンプト（(('irb(main):001:0>'))など）に続いて入力してみましょう.
'#' 以降はコメント文なので入力する必要はありません（入力しても問題ありません）.

# 以下ではコピー＆ペーストの便を考えて，irb のプロンプト（(('irb(main):001:0>')) 等）は表示せずユーザーの入力を太文字で表すことにします．

  irb(main):001:0> gp = gpopen 'air.2012-01.nc/air'   # データを読み込み
  irb(main):002:0> tone gp                # トーンとして描画

すると，ウィンドーが立ち上がり次の画像が表示されます．

((:<a href="img/ggraph_hop_irb01.png">:))
((<"IMG:img/ggraph_hop_irb01_s.png">))（クリックでフルサイズ表示）
((:</a>:))

air.2012-01.nc の中の物理量 air (気温) のデータは
経度，緯度，気圧，時間の 4 次元ですが, GGraph がおまかせで 2 次元断面を取ります．
3次元め以降は最初の要素 (最下層＆最初の時刻) をとり，経度緯度断面を描きます
(他の断面の描き方は((<次の章|URL:ggraph_step.htm>))で紹介します)．
座標軸やタイトルはデータから読み取ってお任せで表示されます．

gp への代入は, 他の内容で上書きしたり irb を終了したりしない限り,
再度行う必要はありません.

ところで，上で gpopen を呼んだ際，次のようにメッセージが画面に表示されたはずです．

  irb(main):002:0> gp = gpopen 'air.2012-01.nc/air'   # データを読み込み
  => <GPhys grid=<4D grid <axis pos=<'lon' in '/home/username/air.2012-01.nc'  sfloat[144]>>
          <axis pos=<'lat' in '/home/username/air.2012-01.nc'  sfloat[73]>>
          <axis pos=<'level' in '/home/username/air.2012-01.nc'  sfloat[17]>>
          <axis pos=<'time' in '/home/username/air.2012-01.nc'  float[31]>>>
     data=<'air' in '/home/username/air.2012-01.nc'  sint[144, 73, 17, 31]>>

第1行目の冒頭に (('<GPhys')) とあります．これは，
gp という変数で表されるモノ (object) が，GPhys という「クラス」(「型」と思えばいいです)のモノ 
(object)であることを表します．続く内容からは，それがどのようなデータを代表しているか，ある程度想像できるでしょう．

=== エラーが出たら

コマンドを誤って打ってしまったとしましょう.

  irb(main):003:0> gp = gpopen "air.2012-01.nc/airrr"  # air と間違えて airrr と打ってしまった ^^;
  RuntimeError: variable 'airrr' not found in #<NumRu::NetCDF:0x0000000255a5d0>
        from /usr/lib/ruby/1.8/numru/gphys/gphys_netcdf_io.rb:528:in `__files2varray'
        from /usr/lib/ruby/1.8/numru/gphys/gphys_netcdf_io.rb:317:in `open'
        from /usr/lib/ruby/1.8/numru/gphys/gphys_io.rb:121:in `open'
        from /usr/lib/ruby/1.8/numru/gdir.rb:572:in `data'
        from /usr/lib/ruby/1.8/numru/gdir.rb:566:in `data'
        from /home/username/irbrc_ggraph.rb:53:in `gpopen'
        from (irb):3

英語でエラーが大量に出力されて読み飛ばしたくなるかもしれませんが,
エラーを修正するためのヒントが得られることも多いのでじっくり読みましょう.
まず見るべきは 1 行目です.
variable 'airrr' not found ですから, 「変数 `airrr' が見つからない」ですね.
2行目からはエラーが発生した箇所から呼び出しもとに遡る「トレースバック」です．
見るべき「大元」はエラーの一番下の行です．上では 
from (irb):3 すなわち，irb の 3 行目の入力となります．
from のあとの数字は行数 (irb の場合はプロンプト右のほうに表示されている番号) です.
今の場合もちろん直前の入力です．ここまで読めば恐らく何が問題であるか思い至るでしょう．
ライブラリのバグが疑われれば一行ずつ上に見て行きますが,
ほとんどの場合は自分が書いた場所 (エラーの下の方) に原因がありますので, 
むやみに上に行かないほうが賢明です.

==== 演習問題

(1) 右辺を (('gp = gpopen "airrr.2012-01.nc/air"')) 
    と変更してエラーを出してみましょう (ファイル名を間違える).
    そしてエラーメッセージを解読してみましょう.

=== いろんな種類の図を描いてみる

GGraph には様々な描画メソッドが用意されています.

==== 1次元描画

折れ線, マーク

  irb(main):004:0> line(gp)
  irb(main):004:0> mark(gp)

結果(北極での「経度断面」なので一様でつまらないですが)：

((:<a href="img/ggraph_hop_irb02.png">:))
((<"IMG:img/ggraph_hop_irb02_s.png">))（クリックでフルサイズ表示）
((:</a>:))
((:<a href="img/ggraph_hop_irb03.png">:))
((<"IMG:img/ggraph_hop_irb03_s.png">))（クリックでフルサイズ表示）
((:</a>:))


==== 2次元描画

トーン (最初の例と同じだがカラーバーをつける)

  irb(main):004:0> tone(gp)
  irb(main):004:0> color_bar

結果：
((:<a href="img/ggraph_hop_irb03.5.png">:))
((<"IMG:img/ggraph_hop_irb03.5_s.png">))（クリックでフルサイズ表示）
((:</a>:))

コンター

  irb(main):004:0> contour(gp)

結果：
((:<a href="img/ggraph_hop_irb04.png">:))
((<"IMG:img/ggraph_hop_irb04_s.png">))（クリックでフルサイズ表示）
((:</a>:))

トーンとコンターの重ね描き

  irb(main):004:0> tone_and_contour(gp)

結果：
((:<a href="img/ggraph_hop_irb05.png">:))
((<"IMG:img/ggraph_hop_irb05_s.png">))（クリックでフルサイズ表示）
((:</a>:))

==== その他

散布図 (scatter), 色つき散布図(color_scatter),
ベクトル図 (vector) があります. 
#（また，現在 ganalysis
#という GPhys 付属ライブラリに入っているヒストグラム描画も GGraph に取り込まれる予定です．）
詳しくはリファレンスマニュアル
(
((<"英語"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
((<"日本語訳"|URL:http://w.livedoor.jp/gphys/>))
)
を参照ください.

=== カッコがついたりつかなかったり...

Ruby ではメソッド（＝関数と思っていいです）の引数は丸括弧で括りますが,
紛れがなければ省略できます．例えば次の2つは同じです．

  irb(main):004:0> tone(gp)
  irb(main):004:0> tone gp

好みでどちらを使ってもよいです．irbで対話的に使う場合，タイプの少ない後者が若干やりやすいように思いますが，後で出てくるようにメソッドをつなげる場合括弧は欠かせません．

=== 描画オプション (1)

図の描き方をカスタマイズすることもできます.

座標系, 地図投影を変更してみましょう.

  irb(main):004:0> set_fig 'itr'=>10             # 正距円筒図法
  irb(main):004:0> set_map 'coast_world'=>true   # 地球の海岸線を表示
  irb(main):004:0> tone gp

結果：
((:<a href="img/ggraph_hop_irb06.png">:))
((<"IMG:img/ggraph_hop_irb06_s.png">))（クリックでフルサイズ表示）
((:</a>:))

ちなみに

  irb(main):004:0> set_fig 'itr'=>10             # 正距円筒図法

を入力した際の標準出力は, 

  => {'itr'=>1}

であるはずです.
これはそれまでに設定されていた値です.
元に戻したいときはこの値をメモしておきましょう.

何番がどの座標系に割り当てられているかは
((<"いろいろな地図投影法"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/rakuraku/node61.html>)) (らくらく DCL 内)
や 
((<DCL マニュアルの「座標系の種類」|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node7.html>))
を参考にしてください.

#さらに, カラーバーを表示し, かつ横軸の目盛を 10 度刻み,
#数字を 30 度刻みに書くようにします.
#
#  set_fig('itr' => 1 )    # 座標系を元に戻す
#  color_bar               # カラーバーの表示
#  set_axes('xtickint' => 10, 'xlabelint' => 30)  # 目盛の設定
#  tone(gp)


今度は北極からみてみましょう．

  irb(main):004:0> set_fig('itr'=>30)            # 正射図法
  irb(main):004:0> tone(gp)

結果：
((:<a href="img/ggraph_hop_irb07.png">:))
((<"IMG:img/ggraph_hop_irb07_s.png">))（クリックでフルサイズ表示）
((:</a>:))

色の塗り方 (カラーマップ) の変更はこうします.

  irb(main):004:0> DCL.sgscmn(3)   # 3 番のカラーマップ(黒-青-白のグラデーション)を使用
  irb(main):004:0> DCL.gropn(1)    # 新しい描画窓の表示 (DCL 5.4.4以降は不要になるはず)
  irb(main):004:0>  tone(gp)

結果：
((:<a href="img/ggraph_hop_irb08.png">:))
((<"IMG:img/ggraph_hop_irb08_s.png">))（クリックでフルサイズ表示）
((:</a>:))


(('DCL.')) で始まるのは ((<RubyDCL|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/>)) 
のメソッドです．RubyDCL は
((<DCL|URL:http://www.gfd-dennou.org/library/dcl/>))
を Ruby から呼べるようにしたものです．
GGraph は RubyDCL と協調して使うようにできています．
なお，irb に読み込んであるスタートアップファイル 
((<irbrc_ggraph.rb|URL:irbrc_ggraph.rb>))
にも (('DCL.')) で始まるコールがたくさんあることがわかるでしょう．

(('DCL.sgscmn')) は RubyDCL のメソッドで，カラーマップを取り替えます.
何番のカラーマップがどういう色の塗り方に対応するかは
((<「らくらく DCL」の描画例|URL:http://www.gfd-dennou.org/library/dcl/dcl-5.4.2/src/env1/colormap/colormap_gallery.html>))
を参考にしてください. インストールされている DCL がバージョンん 5.4.4 より前の場合，
カラーマップの取り替えは gropn によるデバイスの初期化前に行う必要があります．
(('DCL.gropn(1)')) の行はそのためです．

==== 演習問題

(1) tone の図を自分の好きなカラーマップに変更してみましょう.

##[納多] はまりそうなので一旦コメントアウト. 余裕があるときに見直す.
#(2) (やや難) GPhys リファレンスマニュアル
#    (
#    ((<"英語"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
#    ((<"日本語訳"|URL:http://w.livedoor.jp/gphys/>))
#    ) を参考に, 縦軸の目盛打ちも変更してみましょう.
#    (ヒント: 今使っているのは GGraph なのでまずページ内を "GGraph" で検索すると
#    それらしいページへのリンクが見つかる.
#    次に, 横軸の目盛を set_axes で変更していたことから
#    行った先のページで "set_axes" で検索すると...)

=== で，GGraph はどこにでてきたの？ （読み飛ばしてもOK）

これまでの話では，GGraph というものが何なのかよく分からないでしょう．
GGraph は，Ruby の用語ではモジュールというカテゴリーになり，
いろんなメソッドを束ねたものとなっています．
これまで説明してき tone などは GGraph のメソッドなのです．
それが陽にわかるように呼ぶには，

  irb(main):004:0> GGraph.tone(gp)

などと呼びます．こちらがむしろ正式な呼び方なのですが，
スタートアップファイル ((<irbrc_ggraph.rb|URL:irbrc_ggraph.rb>)) 
において最初の GGraph. を省略できるように設定しているのです．
DCL もモジュールですので，GGraph. を省略しないほうが対称性がよいです．
irb を使わずプログラムをファイルにして GGraph を利用する際には，
GGraph. を省略しないことを勧めます（後ほどそのレッスンもします）．

=== ヒストリ機能

長くなりましたので，ここらで一旦 irb を終了しましょう．
その前に入力内容を保存してみましょう．
((<irbrc_ggraph.rb|URL:irbrc_ggraph.rb>)) には便利なヒストリ機能が定義されています
（中をみれば分かりますが，数行で実現されています）．
irb で (('history')) と入力してみてください．

  irb(main):021:0> history
  gp = gpopen 'air.2012-01.nc/air' 
  tone gp
  gp = gpopen "air.2012-01.nc/airrr"  # air と間違えて airrr と打ってしまった ^^;
  line(gp)
  ..(後略)..

(('history_save')) と入力すると履歴が保存されます．

  irb(main):022:0> history_save
  irb history saved in /home/username/irb_ggraph_history.rb
  => nil

履歴はホームディレクトリ直下の ((<irb_ggraph_history.rb|URL:irb_ggraph_history.rb>))
というファイルに保存されます．(('history_save')) 
を引数なしで呼ぶと，履歴はこのファイルに追記されていきます．

irb を終了するには (('exit')) と入力します．

  irb(main):022:0> exit


=== この章で引用した資料

* GPhys リファレンスマニュアル
  (
  ((<"英語"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
  ((<"日本語訳"|URL:http://w.livedoor.jp/gphys/>))
  )

* DCLがサポートする座標系
  * ((<"いろいろな地図投影法"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/rakuraku/node61.html>)) (らくらく DCL 内)
  * ((<DCL マニュアルの「座標系の種類」|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node7.html>))

* DCL
  * ((<DCLホームページ|URL:http://www.gfd-dennou.org/library/dcl/>))
  * ((<RubyDCL|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/>)) (DCLをRubyで使えるようにしたもの)
  * カラーマップ： ((<「らくらく DCL」の描画例|URL:http://www.gfd-dennou.org/library/dcl/dcl-5.4.2/src/env1/colormap/colormap_gallery.html>))
