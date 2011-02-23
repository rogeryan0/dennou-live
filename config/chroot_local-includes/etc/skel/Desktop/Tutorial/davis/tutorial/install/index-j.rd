=begin
= 電脳Ruby謹製品 インストールガイド

Last modified: Fri Nov 14 21:28:02 JST 2008

ここでは, 電脳Ruby謹製品をインストールする方法について, プラットホーム別に解説します.

=end
=begin html
<hr>
=end
=begin

== Linux

電脳Rubyプロジェクトでは RPMパッケージ (Vine Linux [i386], Fedora [i386][x86_64], CentOS [i386][x86_64]), debパッケージ (Debian GNU/Linux [i386]) を用意しています.
Vine と Debian では apt, Fedora と CentOS では yum というパッケージ管理ユーティリティが使用できますので, 非常に簡単にインストールができます.

* ((<RPMパッケージ|URL:../../products/rpm/index-j.html>))
* ((<DEBパッケージ|URL:../../products/debian/index-j.html>))
  * ((<Ubuntu|URL:../../products/ubuntu/index.html>))
== FreeBSD

Ports があります.

* ((<FreeBSD Ports|URL:http://www.ahs.scitec.kobe-u.ac.jp/~murakami/ports.html>))
== Mac OS X

Fink と MacPorts のパッケージがあります.

* ((<Finkパッケージ|URL:http://www.gfd-dennou.org/arch/cc-env/fink-dennou/>))
* ((<MacPortsパッケージ|URL:http://www-mete.kugi.kyoto-u.ac.jp/hiroki/static/DennouTools.html>))

== その他のUNIX系OS

自分で資源をコンパイルすることになります.
((<各製品のホームページ|URL:../../products.htm>))をご覧ください.

(以前は((<一括インストーラ|URL:../../products/numru_installer/index-j.html>))というものを用意していましたが, 今はこのツールはメンテナンスされていません. )

== Windows

Windows用パッケージをお使いください. インストーラ付きですので簡単です.
Cygwinをお使いの場合は, Cygwinバイナリを用意しています. setup.exeから簡単にインストールできます.
(参考：((<Microsoft Windows版のビルト環境による違い|URL:http://www.ruby-lang.org/ja/install.cgi?cmd=view;name=Microsoft+Windows%C8%C7%A4%CE%A5%D3%A5%EB%A5%C8%B4%C4%B6%AD%A4%CB%A4%E8%A4%EB%B0%E3%A4%A4>)))

* ((<Windows用パッケージ|URL:http://www.gfd-dennou.org/arch/dcl/dcl-win/ruby_dcl.html>))
* ((<Cygwinバイナリ|URL:../../products/cygwin/package-j.html>))

== VMware

既存のシステムにインストールするのではありませんが,
Virtualマシンを使って簡単にDennou Ruby Worldを体験することができます.
Dennou Ruby 製品がインストールされたVMwareイメージがあります.

* ((<VMwareイメージ|URL:../../products/vmwa/>))

== LiveCD

* ((<LiveCD|URL:../../products/liveCD/>))

=end
=begin html
<br>
<hr>
<address>
Copyright (C) 2003-2008 GFD Dennou Club. All rights reserved.
</address>
=end
