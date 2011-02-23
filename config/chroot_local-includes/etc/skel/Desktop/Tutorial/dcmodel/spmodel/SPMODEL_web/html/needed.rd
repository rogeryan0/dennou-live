=begin JA
= SPMODEL に必要となる資源
=end JA
=begin EN
= Required softwares
=end EN
=begin

# 履歴情報 (新しい更新履歴を上に追記していくこと)
#
# * 2009/03/03 (佐々木洋平) 微修正
# * 2009/03/03 (佐々木洋平) 0.5 リリースにともなう修正
# * 2009/01/12 (竹広真一) spmodel 内リンクを相対パスに変更
# * 2008/10/07 (森川靖大) gt4f90io -> gtoo5 へ
# * 2007/12/05 (佐々木洋平) Makefile.rd2html の更新に合わせた修正
# * 2007/08/09 (竹広真一) 必要なものから DCL を削除
# * 2005/10/17 (森川靖大) URL のリンク切れを修正
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

spmodel のサンプルプログラムをコンパイルし, 実行するためには
以下のものが必要になります.

* Fortran 90 コンパイラ
  * 動作確認の取れているコンパイラは以下の通りです
    * Fujitsu Fortran & C Compiler ver.5
    * Fujitsu Fortran Driver
    * Intel Fortran Compiler ver. 10.0, 11.0
    * G95 Fortran Compiler ver. 0.91
    * pgi Fortran Compiler ver.7, 8
* ((<ISPACK ライブラリ|URL:/library/ispack/>))
* ((<NetCDF|URL:http://www.unidata.ucar.edu/packages/netcdf/>))
  * ソースのミラーは((<こちら|URL:/library/netcdf/>))
* ((<gtool5 ライブラリ|URL:/library/gtool/>))

Debian GNU/Linux (i386, amd64) 環境ならば
((<deb パッケージ|URL:../debian/needed.htm>))が利用できます. 
各ライブラリ等の使用条件を確認し, 説明にしたがってインストールしてください. 

=end 
=begin EN

* Fortran90 compiler which can be used for building following libraries.
* ((<ISPACK|URL:/library/ispack/>))
* ((<NetCDF|URL:http://www.unidata.ucar.edu/packages/netcdf/>))
  * Mirror site is ((<here|URL:/library/ucar/netcdf/>))
* ((<gtool5|URL:/library/gtool/>))

((<"Debian GNU/Linux binary packages"|URL:../debian/needed.htm.en>)) are available. 

=end EN
