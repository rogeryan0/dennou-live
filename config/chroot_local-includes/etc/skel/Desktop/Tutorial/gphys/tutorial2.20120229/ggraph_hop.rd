# GPhys, GGraph チュートリアル (ホップ. GGraph 編)
#
# = 履歴 (新しいものは上から追記)
#
# * 2012/02/18 納多哲史  新規作成
#

((*このドキュメントは作成中です. 学習には
((<"現行のチュートリアル"|URL:http://ruby.gfd-dennou.org/products/gphys/tutorial/>))
を参照ください.*))


== 可視化の基礎

この章は，描画ライブラリである GGraph (GPhysの付属ライブラリ)を使った気象データの可視化のチュートリアルです.
GGraph を使うと簡単に可視化できることを体験します．

前もって ((<「はじめる前に」|URL:getting_started.htm>)) の章で紹介した準備をしておいてください．

=== 描画

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
3次元め以降は最初の要素 (最下層＆最初の時刻) をとり，経度緯度断面を描きます．

((* 他の断面の描き方は後ほど示します．*))

gp への代入は, 
他の内容で上書きしたり irb を終了したりしない限り,
再度行う必要はありません.

=== エラーが出たら

誤ったコマンドを打ってしまったとしましょう.

  irb(main):003:0> gp = gpopen("air.2012-01.nc/airrr")  # air と間違えて airrr と打ってしまった
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
従って見るべき「大元」は, エラーの一番下の行です．上では 
from (irb):3 すなわち，irb の 3 行目の入力となります．
from のあとの数字は行数 (irb の場合はプロンプト右のほうに表示されている番号) です.
もちろん直前の入力です．ここまで読めば何が問題であるか思い至るでしょう．
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

トーン (最初の例と同じ)

  irb(main):004:0> tone(gp)

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

=== 描画オプション (1)

図の描き方をカスタマイズすることもできます.

座標系, 地図投影を変更してみましょう.

  irb(main):004:0> set_fig('itr'=>10 )            # 正距円筒図法
  irb(main):004:0> set_map('coast_world'=>true)   # 地球の海岸線を表示
  irb(main):004:0> tone(gp)

結果：
((:<a href="img/ggraph_hop_irb06.png">:))
((<"IMG:img/ggraph_hop_irb06_s.png">))（クリックでフルサイズ表示）
((:</a>:))

ちなみに

  irb(main):004:0> set_fig('itr'=>10 )            # 正距円筒図法

を入力した際の標準出力は, 

  => {'itr'=>1}

であるはずです.
これはそれまでに設定されていた値です.
元に戻したいときはこの値をメモしておきましょう.

何番がどの座標系に割り当てられているかは
((<"いろいろな地図投影法"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/rakuraku/node61.html>)) (らくらく DCL 内)
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

  DCL.sgscmn(3)   # 3 番のカラーマップ (黒-青-白のグラデーション) を使用
  DCL.gropn(1)    # 新しい描画窓の表示 (DCL 5.4.4 以降は不要になるはず)
  tone(gp)

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
や 
((<DCL のマニュアル|URL:http://www.gfd-dennou.org/library/dcl/dcl-f77doc/Japanese/f77/grph1/node7.html>))
を参考にしてください. インストールされている DCL がバージョンん 5.4.4 より前の場合，
カラーマップの取り替えは gropn によるデバイスの初期化前に行う必要があります．
(('DCL.gropn(1)')) の行はそのためです．

==== 演習問題

(1) tone の図を自分の好きなカラーマップに変更してみましょう.

###############
((* 2012-02-28 堀之内. 現在ここまで作業 *))
###############


# はまりそうなので一旦コメントアウト. 余裕があるときに見直す.
#(2) (やや難) GPhys リファレンスマニュアル
#    (
#    ((<"英語"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
#    ((<"日本語訳"|URL:http://w.livedoor.jp/gphys/>))
#    ) を参考に, 縦軸の目盛打ちも変更してみましょう.
#    (ヒント: 今使っているのは GGraph なのでまずページ内を "GGraph" で検索すると
#    それらしいページへのリンクが見つかる.
#    次に, 横軸の目盛を set_axes で変更していたことから
#    行った先のページで "set_axes" で検索すると...)

=== 描画オプション (2)

図は重ねがきすることができます.

  tone(gp)
  contour(gp, false)  # 前の図を消さないでその上に重ね描き

第二引数を省略した場合は true (前の図を消して図を描き直す) と同じ扱いになります.

図のカスタマイズですが, 
描画メソッドによっては, set_fig のようなメソッドでなく, 
描画メソッドの引数によって変更することができます.

  contour(gp, true, 'interval'=>5)

このとき, 第二引数は省略できないので注意してください.


==== 演習問題

(1) 'interval' を使ってトーン・コンターの間隔を設定してみましょう. 
    このとき, トーンの間隔とコンターの間隔は異なる値にし,
    かつ重ねがきしてみましょう. 

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

また, GGraph は細かい調整は苦手ですので,
こだわりたい場合はその下請けの
((<"RubyDCL"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/>))
も参考にしましょう.

=== 参考資料

* ((<"GPhys/GGraph チートシート"|URL:http://davis.gfd-dennou.org/rubygadgets/ja/?%28Others%29+GPhys%2FGGraph+%A5%C1%A1%BC%A5%C8%A5%B7%A1%BC%A5%C8>))
* GPhys リファレンスマニュアル
  (
  ((<"英語"|URL:http://ruby.gfd-dennou.org/products/gphys/doc/>)),
  ((<"日本語訳"|URL:http://w.livedoor.jp/gphys/>))
  )
* ((<"RubyDCL ドキュメント"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/>))
* ((<"いろいろな地図投影法"|URL:http://ruby.gfd-dennou.org/products/ruby-dcl/ruby-dcl-doc/rakuraku/node61.html>)) (RubyDCL ドキュメント内)
* ((<"DCL colormaps"|URL:http://www.gfd-dennou.org/library/dcl/dcl-5.4.2/src/env1/colormap/colormap_gallery.html>)) (DCL ドキュメント内)
