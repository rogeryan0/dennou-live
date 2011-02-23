=begin JA
= SPMODEL ライブラリ (spml)
=end JA
=begin EN
= SPMODEL library (spml) 
=end EN
=begin

# * 2009/03/04 (竹広真一) 最新版リンクに日付とバージョン番号を記載
# * 2009/03/02 (佐々木洋平) spml.tar.gz -> spml_current.tar.gz に修正
# * 2009/01/12 (竹広真一) spmodel 内リンクを相対パスに変更
# * 2007/12/05 (佐々木洋平) Makefile.rd2html の変更に伴う修正
# * 2006/03/04 (竹広真一) 
# * 2005/10/01 (竹広真一) 
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

SPMODEL ライブラリ (spml) は, スペクトル計算のための Fortran77 ライブ
ラリ ((<ISPACK|URL:/library/ispack/>)) のサブルーチンを Fortran90 の
関数でくるみ, さらにいくつかの便利な微分・積分・境界値問題などのための
関数やサブルーチンを追加したものです.

このライブラリに含まれる関数は, Fortran90 で強化された配列機能である
*「要素別演算:配列の要素を指定せずに各要素に対する計算を行えること」
*「配列を返す関数を作成できること」
を利用し, さらに関数の名前の命名法を 
(関数の出力するデータ種類)_(機能)_(引数のデータ種類) 
といった形に統一してあります. 

Fortran90 の配列機能を生かしたこのライブラリの配列関数を用いることで, 
時間発展方程式の時間変化項以外の部分を
数式の形そのままにプログラミングすることができるようになってます. 
支配方程式の形をそのままプログラムソースに反映させられるので
プログラムの可読性を向上させることができます. 
また, spml の配列関数は入出力配列の性質が名前からわかるようにするべく
統一的に命名法にしたがっているので, 関数の使い方が機械的になり, 
プログラムを修正することも容易に行えるようになっています. 
また, 引数と出力関数の種類を名前からわかるようにすることで
引数や出力の間違いを減らすことも狙っています. 

* ((<最新 tar+gzip パッケージ(2009.03.03, ver.0.5.0)|URL:../spml_current.tar.gz>))
  * ((<Debian パッケージ|URL:../debian/>))
* ((<最新 ソースツリー|URL:../spml/>))
* ドキュメント
  * ((<ライセンス|URL:../spml/COPYRIGHT>))
  * ((<インストールガイド|URL:../spml/html/INSTALL.html.ja>))
  * ((<マニュアル|URL:../spml/html/>))


=end JA
=begin EN

* ((<Latest tar+gzip package(2009.03.03, ver.0.5.0|URL:../spml_current.tar.gz>))
  * ((<Debian packages|URL:../debian/index.htm.en>))
* ((<Latest source code tree|URL:../spml/>))
* Documents
  * ((<Copyright|URL:../spml/COPYRIGHT>))
  * ((<Installation guid|URL:../spml/html/INSTALL.html>))
  * ((<Manual|URL:../spml/html>))(sorry, written in Japanese)

=end EN



