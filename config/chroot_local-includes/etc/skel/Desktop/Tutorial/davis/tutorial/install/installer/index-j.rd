=begin

= 電脳Ruby謹製品 一括インストーラ

Last modified: <2007-04-27 06:05:31 koshiro>

電脳Ruby謹製品を自動でインストールするRubyスクリプトです．
GAVE関連のすべての電脳Ruby謹製品に対応しています(Ruby-SSL2, Ruby/Msise90 には現在のところ非対応です)．

=end
=begin html
<font color="red">※このソフトウェアは現在メンテナンスされていません!</font>
=end
=begin

== ダウンロード

((<numru-install.rb|URL:http://ruby.gfd-dennou.org/products/installer/numru-install.rb>))
(Ver. 3.4.7)

== 動作環境

URIモジュールが標準添付されている Ruby 1.6.7 以上が必要です．

UNIX系OS(Linux, FreeBSD, Solaris, Windows上のCygwin, などなど)であればどのプラットフォームでも動作すると思いますが，GNU gzip と GNU patch が必要になります．

こちらでは以下の環境で動作を確認しています：

* Vine Linux 2.6 + Ruby 1.6.7
* Vine Linux 3.1 + Ruby 1.8.2
* Debian GNU/Linux 3.1 + Ruby 1.8.2
* Cygwin(on Windows XP Professional) + Ruby 1.8.4

※ numru-install.rb では，GTK+関連ライブラリの有無を以下のようにして
チェックしていますので注意してください：

:GTK+1
  gtk-config, imlib-config の各コマンドの結果で判定しています(C版DCL
  の configure も同様のチェックをしています)．また，Ruby-GNOME をイン
  ストールする際には gdk-pixbuf-config によるチェックも行われます．
  whichコマンド等で，これらのコマンドがあるかどうか確認してください．
  たとえGTK+1のライブラリがあっても，これらのコマンドがないと正しくイ
  ンストールされません．バイナリパッケージ配布のLinuxディストリビュー
  ションをお使いの場合は注意が必要です．例えば Red Hat Linux 9 では，
  これらのコマンドは開発ツールパッケージ
  ({gtk+|gdk-pixbuf|imlib}-devel-x.x.x.i386.rpm)のほうに含まれており，
  デフォルトではインストールされていない場合があります．インストール
  CD-ROMやFTPサイト等からこれらパッケージを取得しインストールしてくだ
  さい．GTK+1ライブラリをソースコードからコンパイルしてインストールし
  ている場合は問題ないと思われます．

:GTK+2
  pkg-config コマンドの結果で判定しています(C版DCL の configure も同様
  のチェックをしています)．whichコマンド等で，これらのコマンドがあるか
  どうか確認してください．バイナリパッケージ配布のLinuxディストリビュー
  ションをお使いの場合，たとえGTK+2のライブラリがあっても，pkg-config
  用のライブラリ情報ファイルがない場合がありますので注意してください．
  例えば Vine Linux 3.1 では，gtk2-devel-x.x.x.i386.rpm をインストール
  する必要があります．インストールCD-ROMやFTPサイト等からパッケージを
  取得しインストールしてください．GTK+2ライブラリをソースコードからコ
  ンパイルしてインストールしている場合は問題ないと思われます．また，
  pkg-config がライブラリ情報を参照するディレクトリは，環境変数
  PKG_CONFIG_PATH で決まっています．通常は心配ないと思いますが，Cygwin
  の場合などは注意してください．

== 使い方

((%$ ruby numru-install.rb%))

として実行します．あとは質問に答えていくだけです．

現在インストールされているRubyライブラリのバージョン情報を保持するリストファイルを指定する(はじめて使うときに作ります)と，最新のパッケージのバージョンを探しにいきます．
そうすると，インストールできるパッケージのリストが表示されますので，パッケージを選択して，ダウンロードする資源の置き場所とインストール先を指定すればOKです．

なお，Firewallがある等プロキシ経由でインターネットに接続している環境の方は，一括インストーラを実行する前に，

(sh系) ((%$ export http_proxy=http://your.proxy.server:8080/%))

(csh系) ((%% setenv http_proxy http://your.proxy.server:8080/%))

などとして環境変数 http_proxy にプロキシサーバとポート番号を設定してください．

また，Firewallがあると，passive mode でないと外部のFTPサーバに接続できない場合があります(ADSL回線をお使いの場合など)．そのときは，一括インストーラを実行する前に，

(sh系) ((%$ export FTP_PASSIVE_MODE=YES%))


(csh系) ((%% setenv FTP_PASSIVE_MODE YES%))

として環境変数 FTP_PASSIVE_MODE を設定してください．

インストールし終わったら，環境変数 PATH と RUBYLIB を確認することも忘れずに．

== 関連情報

=== Cygwin向けパッチ

一部のプロダクトでは，Cygwin環境でインストールに不具合が生じます．
numru-install.rb では，これを解決するために以下のパッチをあてています．

* C版DCL(5.3.x) -- ((<dcl53c_cygwin.patch.gz|URL:http://ruby.gfd-dennou.org/products/installer/patches/dcl53c_cygwin.patch.gz>)) (Thanks to 遠藤さん)
  * dclcc (cshスクリプト)の先頭行を ((%#!/bin/csh -f%)) から ((%#!/bin/tcsh -f%)) に変更 (Cygwinには csh がないため)

* NetCDF(3.6.1) -- ((<netcdf361_cygwin.patch.gz|URL:http://ruby.gfd-dennou.org/products/installer/patches/netcdf361_cygwin.patch.gz>))
  * Makefile の whatis 生成に関する部分をコメントアウト (makewhatis が終了ステータス 255 を返すので make が異常終了してしまうため)

== 変更履歴
:Feb 13, 2006 (3.4.7)
  * 必要ならば netcdf361_cygwin.patch.gz を適用するようにした

:Feb 8, 2006 (3.4.6)
  * NArray のサイトが移転したのに伴い，URLを変更
  * インストールチェック用のメソッド installed? を変更 (主に Ruby-FFTW3 対策)

:Jun 27, 2005 (3.4.5)
  * メソッド install の最後に メソッド installed? でインストールチェックを行うようにした(主に Ruby-GNOME2 対策)
  * モジュール NumRu::Fetch::http_get_list の改良
  * getopts を使わないようにした
  * Rubyライブラリのインストールチェックの際の require を外部プロセスで行うようにした

:May 24, 2005 (3.4.4)
  * rubydcl-1.5.0 用の Cygwinパッチを削除

:Mar 17, 2005 (3.4.3)
  * ruby-gnome, ruby-gnome2 のダウンロードサイトを変更：voxel.net -> JAIST

:Mar 3, 2005 (3.4.2)
  * Cygwin + Ruby 1.8.2 の環境で narray インポートライブラリのインストールが失敗することに対応

:Mar 3, 2005 (3.4.1)
  * netcdf の最新バージョンが 3.6.0-p? の場合，Cygwin では 3.6.1 beta release を使うようにした

:Mar 3, 2005 (3.4.0)
  * 電脳Ruby謹製品のバージョンチェックの際 FTP サーバへの接続が1回ですむように変更
  * fftw の URL を オリジナルサイトに再変更(日本のミラーサイトに最新版がなかったため)

:Mar 1, 2005 (3.3.3)
  * narray_miss のインストールスクリプトが変更されたことに対応
  * パッケージの最新バージョンを取得するための正規表現を変更
  * fftw の URL を 日本のミラーサイトに変更

:Jan 15, 2005 (3.3.2)
  * gphys のインストールスクリプトが変更されたことに対応

:Jan 6, 2005 (3.3.1)
  * 必要ならば rubydcl150_cygwin.patch.gz を適用するようにした
  * netcdfについて，Linuxでのconfigure option, Cygwinでの pacth file を変更した
  * netcdf の最新バージョンが 3.6.0 の場合，Cygwin では 3.6.1 beta release を使うようにした

:Dec 22, 2004 (3.3.0)
  * C版DCLのtarball名とURLが変更されたことに対応
  * gave のインストールスクリプトが変更されたことに対応

:Aug 12, 2004 (3.2.1)
  * バグフィックス．

:Aug 12, 2004 (3.2.0)
  * class RbLibの変更：narray_miss のインストールスクリプトが変更されたことに対応

:Aug 11, 2004 (3.1.0)
  * class RubySoftの変更：gave のインストールスクリプトが変更されたことに対応
  * パッケージ名の変更に対応：misc -> numru-misc
  * 細かなバグフィックス．

:Jul 15, 2004 (3.0.0)
  * インストールできるパッケージの追加：fftw, ruby-fftw3, multibitnums, gpv, numru-units, ruby-{gnome|gnome2}-all, gave
  * module NumRu::Fetch::ftp_fetch, NumRu::Fetch::ftp_get_list の変更：環境変数 'FTP_PASSIVE_MODE' のチェックを追加
  * module NumRu::Install を改訂
    * GTK のバージョンチェックを追加
    * class Installer の追加：インストーラ自身のバージョンチェックおよびダウンロード機能をもたせた
    * class RubySoft に method 'install' を追加
    * RbExtLib and RbLib の superclass を RubyLib に変更
    * class Software, RubySoft の method 'current_ver' の返り値を「version + GTK version」に変更
    * class Software に method 'current_ver_disp' を追加
    * パッケージの依存関係チェックの仕組みを改訂：method 'get_dep_check_pkgs' の追加，method 'check_depend' の改訂
    * module function 'upgrade' の追加
  * パッケージの依存関係を一部変更
    * rubydcl requires ruby-{gnome|gnome2}-all
    * gphys requires ruby-fftw3 and numru-units
  * 'gpatch' コマンドがあれば，'patch'の代わりにそちらを使うようにした
  * バグフィックス.

:Dec 18, 2003 (2.0.1)
  * バグフィックス(Thanks to 西澤さん)
    * 圧縮ファイルの解凍コマンドを変更：'zcat' -> 'gzip -dc'
    * Net::HTTP#get 中での例外を拾うようにした(Ruby 1.6.7の場合のみ)
  * 必要な場合に netcdf350_cxx_gcc3.patch が適用されるようにした

:Dec 10, 2003 (2.0.0)
  * インストールできるパッケージの追加：misc, met, gphys
  * module NumRu::Install を大幅に改訂
    * ダウンロードした資源を置くディレクトリを選択できるようにした
    * インストールする場所を選択できるようにした
    * バージョンも含めた依存関係の考慮
    * パッチの自動適用
    * Cygwin対応

:Jun 27, 2003 (1.0.0)
  * 最初のリリース
  * Thanks to ごとけんさん：module NumRu::Fetch の作成

=end
=begin html
<br>
<hr>
<address>
Copyright (C) 2003-2006 GFD Dennou Club. All rights reserved.
</address>
=end
