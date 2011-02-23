= Mac OS X への電脳Ruby関連資源のインストールメモ

北村 祐二

2006/03

== はじめに

本稿ではRubyからDCLをはじめとするライブラリ群をMac OS X上で利用するた
めの手順をまとめる．今回は必要最小限の環境を自前で構築するための最短距
離を提供することを目的としているため，例えばDCLでGTKを用いるなどの話題
については除外してある．このインストールメモにおいて前提としている環境
は以下の通りである．

* Mac OS X (PowerPC版，Intelプラットフォームについては未検証)
* gcc4
* X11

gcc4については，開発環境(Xcode)を別途インストールする必要がある．最近
のMacにははじめからインストールディスクが用意されているようだが，手元
にない場合でもAppleのサイトから最新版をダウンロードすることが可能であ
る(要Developer登録)．X11についてはOSの導入の際にカスタムインストールが
必要となるかもしれないので，その点についても注意が必要である．

筆者はMac OS X 10.4.5 + gcc4 (Xcode 2.2.1)の環境で最低限の動作確認を
行ったが，あらゆる環境で動作を保証するものではないことに注意．

== 準備

=== ruby-1.8.4

入手先:
* http://www.ruby-lang.org/ja/
* ftp://ftp.gnu.org/gnu/readline/readline-5.1.tar.gz

Mac OS XにはデフォルトでRubyがインストールされているが，readlineがリン
クされていないので特にirbを使うときに不便である．今回はRubyも自前で入
れることにした．

まず，readline-5.1のインストールから行う．

 % tar xzvf readline-5.1.tar.gz
 % cd readline-5.1
 % ./configure --prefix=/usr/local
 % make
 % sudo make install

次にRubyのインストール．

 % tar xzvf ruby-1.8.4.tar.gz 
 % cd ruby-1.8.4
 % ./configure --prefix=/usr/local --with-readline-dir=/usr/local
 % make
 % sudo make install

参考サイト:
* http://d.hatena.ne.jp/tsimo/20060212
* http://hivelogic.com/articles/2005/12/01/ruby_rails_lighttpd_mysql_tiger

=== DCL-5.3.1-C

入手先:
* http://www.gfd-dennou.org/arch/dcl/

makeする前に以下のようにソースの修正が必要である．

src/grph1/swpack/zxpack.c中の
 #include <stdlib.h>
をコメントアウトする．

そのままだとsrc/grph1/swpack/zxpack.cでwaitが多重定義されるというエラー
になる．これは，/usr/include/libstd.hから/sys/wait.hを呼んでいるためと
推察される．ただし，この作業はMac OS X 10.3以前には不要だった記憶がある．

以上の修正を行ったのち，

 % env CFLAGS='-O' ./configure --prefix=/usr/local/dcl-5.3.1-C
 % make
 % sudo make install

make installのあとに

 % sudo ranlib /usr/local/dcl-5.3.1-C/lib/libcdcl53.a
 % sudo ranlib /usr/local/dcl-5.3.1-C/lib/libf2c4dcl.a

が必要となる．

=== netCDF-3.6.1

入手先:
* http://www.unidata.ucar.edu/software/netcdf/

今回はRuby-NetCDFから呼び出すことだけを想定し，Cのライブラリ以外は作ら
ない方針とする．Fortran用のライブラリを利用するための設定は，用いる
Fortranコンパイラに強く依存するため，ここでは触れないことにする．

 % env CC=gcc CXX='' FC='' F90='' ./configure --prefix=/usr/local
 % make
 % make check
 % sudo make install

== Rubyのライブラリ群のインストール

上に述べた準備ができていれば，Rubyのライブラリをインストールするのはそ
れほど難しくない．Linux等と同様の手順で基本的にOKである．

=== NArray-0.5.8

入手先:
* http://narray.rubyforge.org/index.html.ja

 % ruby extconf.rb
 % make
 % sudo make install

=== ruby-dcl-1.5.2

入手先(以下についても同様):
* http://ruby.gfd-dennou.org/products.htm

 % ruby extconf.rb
 % make
 % sudo make install

=== ruby-netcdf-0.6.2

 % ruby extconf.rb
 % make
 % make test
 % sudo make install

=== NArrayMiss-1.1.2

 % ruby setup.rb config
 % sudo ruby setup.rb install

=== numru-units-1.5

 % sudo ruby install.rb

=== numru-misc-0.0.6

 % sudo ruby install.rb

=== gphys-0.5.1

 % sudo ruby install.rb


== [付録] GTK対応にする場合のメモ

電脳ライブラリをGTK対応にするには，GTK+2関連のライブラリをあらかじめ導
入しておく必要がある．これらのライブラリをすべて自力でコンパイルするの
はかなりの手間なので，ここでは，Finkというパッケージシステムを利用する
ことにする．実は上記において自力で導入したライブラリのいくつかはFinkを
使っても導入可能なのだが，今回はGTK+2関連のインストールのみにFinkを使
う方針とした．ただし，本インストールメモは筆者が実際に行った作業を書き
留めただけなので，実際には不要な作業が紛れ込んでいる可能性もあることに
注意していただきたい．

=== Finkのインストール

* http://fink.sourceforge.net/download/index.php?phpLang=ja
よりFink 0.8.0バイナリインストーラをダウンロード．
Finkのインストールについては上記サイトを参照のこと．
デフォルトではunstableパッケージを参照しないので，しかるべき変更を行う．
具体的には，/sw/etc/fink.confのTrees:にunstable/crypto,unstable/mainを加える:
 Trees: local/main stable/main stable/crypto unstable/crypto unstable/main
/sw/etc/apt/source.listに
 deb file:/sw/fink unstable main crypto
を追加．これらの変更を行ったら，

 % fink scanpackages; fink index
 % sudo apt-get update

を実行．

その上で，GTK+2関連のライブラリをインストールする．ここでは時間節約の
ためバイナリインストールを試みる．必要なパッケージは，

* gtk+2
* gtk+2-dev
* pango1-xft2-dev
* glib2-dev
* atk1

である．以下のようにしてパッケージを順次インストールする．

 % sudo apt-get install package-name

=== Ruby-GTK2
* http://ruby-gnome2.sourceforge.jp/ja/index.html
よりruby-gtk2-0.14.1.tar.gzを入手．

 % ruby extconf.rb
 % make
 % sudo make install

=== DCL-5.3.1-C

GTK非対応版と同じ手順でOK.

=== ruby-dcl-1.5.2

lib/dcl.rbの2行目
 if /cygwin|mingw/ =~ RUBY_PLATFORM
を
 if /cygwin|mingw|darwin/ =~ RUBY_PLATFORM
としてから，

 % ruby extconf.rb
 % make
 % sudo make install

=== gave-1.2.1

 % ruby setup.rb config
 % sudo ruby setup.rb install
