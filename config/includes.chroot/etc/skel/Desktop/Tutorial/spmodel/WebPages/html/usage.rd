=begin JA
= SPMODEL の使い方
=end JA
=begin EN
= How to use SPMODEL ?
=end EN

=begin

# * 2009/03/03 (佐々木洋平) 0.5 リリースにともなう更新
# * 2009/02/19 (竹広真一) tutorial へのリンクを作成
# * 2009/01/12 (竹広真一) spmodel 内リンクを相対パスに変更
# * 2007/12/05 (佐々木洋平) リンク先の修正
# * 2007/12/05 (佐々木洋平) Makefile.rd2html の更新に合わせた修正
# * 2007/07/19 (竹広真一) 
# * 2005/07/25 (小高正嗣) 
# * 2005/07/21 (小高正嗣) 
# * 2005/06/30 (小高正嗣) 
# * 2005/04/01 (小高正嗣) 
# * 2005/04/01 (小高正嗣)
# * 2005/03/30 (小高正嗣)
# * 2004/03/18 (小高正嗣)
# * 2004/01/26 (小高正嗣)
# * 2002/09/12 (竹広真一, 林祥介)
# * 2002/02/15 (竹広真一) 新規作成

=end

=begin JA

== 準備 -- ライブラリのインストール

まず ((<SPMODEL に必要となる資源|URL:./needed.htm>)) 
をインストールしてください. 
Debian GNU/Linux (i386, amd64) 環境ならば
((<deb パッケージ|URL:../debian/needed.htm>)) が利用できます.

次に((<SPMODEL ライブラリ (spml)|URL:./spml.htm>))をインストールします.
手引はソースアーカイブ中の
((<INSTALL.html.ja|URL:../spml/html/INSTALL.html.ja>)) です.
Debian GNU/Linux (i386, amd64) 環境ならば ((<deb パッケージ|URL:../debian/>))
が利用できます.

== デモ, サンプルプログラムを動かしてみる

spml をインストールできたら, spml に付属のデモプログラムをコンパイルし, 
実行してみてください. spml が正しくインストールされているならコンパイ
ル用のスクリプト ((<spmfrt|inline>)) が使えるはずです. 

 $ spmfrt hogehoge.f90
 $ ./a.out

実行後は計算結果が gtool4 形式のデータファイルが作成され, 出力されます.
((<dennou ruby 製品|URL:/library/ruby>))の gpview コマンド等を用いて結果
を表示してみてください.

うまくいったら, 好きな((<サンプルプログラム>))をダウンロードして同じよ
うに動かしてみましょう.

== プログラムを書いてみる.

SPMODEL のプログラム作成のコツの
((<"チュートリアル"|URL:../tutorial/index.htm>))あるいは
流体力学会オンラインジャーナル「ながれマルチメディア」の解説論文
((<"http://www.nagare.or.jp/mm/2006/spmodel/"|URL:http://www.nagare.or.jp/mm/2006/spmodel/>))
などを参考にして, サンプルプログラムを変更したり, 自分でプログラムを書いて
楽しんでください.


=end JA
=begin EN

== Step 0: Install required library

Install ((<Required softwares|URL:./needed.htm.en>)).
If you use Debian GNU/Linux (i386, amd64), 
((<binary packages |URL:../debian/needed.htm.en>)) can be used.


== Step 1: Install spml library

Install ((<SPMODEL library (spml)|URL:./spml.htm.en>)). 
Please read install guide
((<INSTALL.html|URL:../spml/html/INSTALL.html>)).
If you use Debian GNU/Linux (i386,amd64), 
((<binary packages |URL:../debian/index.htm.en>)) can be used.


== Step 2: Use demo and sample programs

Please compile the demonstration program attached to spml library and
execute. If installation of spml library is succeeded,
((<spmfrt|inline>)) command can be use to compile.

 $ spmfrt hogehoge.f90
 $ ./a.out

Output data format is gtool4/netCDF convection. 
Gpview which is one of the ((<dennou ruby products|URL:/library/ruby>))
is useful for displaying gtool4/netCDF binary data.

If it is succeeded in executing the demonstration program, download,
compile and execute ((<SPMODEL sample programs>)).


== Farther Step

Please change the sample program, and enjoy writing the program for 
yourself referring to the
((<"tutorial of SPMODEL programming manner"|URL:../tutorial/index.htm.en>))
(Be preparing it now), or our article on the online journal of Japan Society
of Fluid Mechanics, 
((<"http://www.nagare.or.jp/mm/2006/spmodel/"|URL:http://www.nagare.or.jp/mm/2006/spmodel/>)).


=end EN


