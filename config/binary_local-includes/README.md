Debian Live DVD (Squeeze) / Debian Live Installer (Squeeze)
===========================================================

Copyright: 佐々木洋平 <uwabami@{debian.or.jp,gfd-dennou.org}>,
           Debian JP Project/GFD Dennou Club


はじめに/ 追加情報など
----------------------

Debian Live DVD 制作後の追加情報や訂正は以下の URI にてアナウンスされます.
この文書とあわせてお読みください.

http://www.gfd-dennou.org/library/spmodel/LiveCD/index.htm


目次
----

* Debian Live について
  1. Live DVD
  2. DVD の差分を USB メモリに保存する
  3. よくある質問
  4. Debian Live Installer
  5. USB メモリにインストールする
* Debian Live のカスタマイズ


Debian Live について
--------------------

この DVD は, Debian Live Project の live-build を用いて制作された Debian
Live とDebian Live Installer のハイブリッド DVD です. Debian Testing
(Squeeze) ベースで作られ, 手軽に Debian テスト版の最新環境を試せるように
なっています.

  * Debian GNU/Linux 6.0(Squeeze)
  * live-build/live-boot/live-config
  * Debian Live Installer

インストーラには Debian Live Installer を使用し, Debian GNU/Linux 6.0
(Squeeze) の環境をそのままハードディスクにインストールできます.

今回も実験的に Debian Live 上から Debian Installer を起動して Debian を
ハードディスクにインストールする, Live Installer Laucher を収録しました.

Live Installer Launcher には表示の不具合があるので, ご利用になるかたは,
Live Installer Launcher の項目もご覧ください.

収録パッケージの詳細については DVD 内の live/packages.txt をご覧ください.

Live DVD として使う
--------------------

Debian Live を起動するにはメニューの「Live」を選択します. それぞれのメ
ニューは以下のようになっています.

* Live:
  * 通常起動モードです.

* Live (fail-safe mode)
  * すべてのオプションを無効にして起動するモードです.うまく起動できない場
    合に試してみてください.

起動時のパラメータを追加するには, メニューの上で TAB キーを押して追加します.
パラメータの詳細については, DVD の live/parameters.txt をご覧ください.


DVD の差分を USB メモリに保存する
---------------------------------

Debian Live は, 起動時に「 live-rw 」もしくは「 home-rw 」というラベル名
のext2/3 パーティションをみつけると自動的にマウントし, DVD の差分を保存し
ます.

live-rw は/ (ルート) ディレクトリ全体を, home-rw は home ディレクトリを保
存します.


よくある質問
-------------

Q. ログイン画面に戻ってしまいました
A. 10 秒待つと自動的にログインします.
   すぐにログインする場合のユーザー名とパスワードは以下のとおりです.

  > ユーザー名: user
  > パスワード: live

Q. root 権限になるには, どうすればいいですか?
A. sudo を使って下さい.

 > $ sudo <コマンド>

他に
 * 「システムターミナル・スーパーユーザーモード」を開く
 * コマンド「 sudo -s 」を使って root になる
が, あります.


Debian Live Installer Launcher
-------------------------------

** 注意 **

* Live Installer Laucher は実験的に収録しています
  まだ不具合があると思うので自己責任でご利用ください.

* 現在判明している不具合
  Live Install Launcher を起動すると, インストーラ画面が切れます.

回避方法としては, 上下パネルを隠すとボタンが見えるようになるので, パネ
ルの上でコンテキストメニューを表示し [プロパティ] を選択. 「自動的に隠す」
にチェックを入れる. を上下のパネルの上でおこなってください.


Debian Live Installer
---------------------

** 注意 **

* Debian GNU/Linux Squeeze (Testing) 環境がインストールされます

Debian Live Installer は Debian Live DVD の内容をそのままハードディスクに
インストールします. インストーラの起動は, DVD を起動して「 GUI Install 」
もしくは「 Text Install 」を選択します.

インストール作業のおおまかな流れは, 「言語とキーボードを指定」→「パーティ
ションの作成」→「ルートパスワードとユーザーの作成」→「 GRUB のインストー
ル」になります.

### キーボードが英語キーボード配列になっている ###

GDM のログイン画面で日本語キーボードを選択してください.

ターミナルから変更するには, dpkg-reconfigure を使って
keyboard-configuration の設定を変更します. 変更には root 権限で以下のコマ
ンドを実行します.

 # dpkg-reconfigure keyboard-configuration

キーボードモデルと配置の質問は以下のように答えます. その他の質問は Enter
キーで先に進めて構いません.

 > キーボードモデル: Generic 105-key PC
 > キーボードの配置: Japan

直接/etc/default/keyboard を変更してもかまいません.

 > XKBMODEL="jp106"
 > XKBLAYOUT="jp"

### aptitude/Synaptic でソフトをインストールできない ###

Live Installer の初期設定ではリポジトリが無効になっています.
/etc/apt/sources.list に以下のリポジトリを追加してください.

deb http://cdn.debian.net/debian/ squeeze main contrib non-free
deb-src http://cdn.debian.net/debian/ squeeze main contrib non-free

### USB メモリにインストールする ###

dd (linux) や Win32 Image Writer (Windows) などで, ISO イメージをそのまま
USBメモリに書き込んでください.

Linux の場合は, 例えば

>  $ sudo dd if=debian_live-binary-20101104062609-hybrid.iso of=/dev/ (USB メモリのデバイス) bs=1M

です.

Image Writer for Windows:
https://launchpad.net/win32-image-writer/+download

Debian Live のカスタマイズについて
-----------------------------------

github にある live-build のレシピを利用して Debian Live DVD カスタマイズ
することができます.

カスタマイズをおこなうには Linux 上で作業をする必要がありますが, Live
DVD 上からもカスタマイズは可能ですので, 興味のある方はチャレンジしてみた
ください.

カスタマイズには 10GB 程度の保存領域が必要になるので, USB 接続 HDD などを
用意して保存領域を確保する必要があります.

ビルドの方法は, 以下のコマンドによりビルドすることができます.

 $ git clone git://github.com/uwabami/dennou-live.git
 $ cd dennou-live.git
 $ make

live-build の設定については, Debian Live のサイトを見るのが一番ですが,日
本語の資料では, 関西 Debian 勉強会の資料があるので, それを参考にするとよ
いでしょう.


関西 Debian 勉強会  資料
http://tokyodebian.alioth.debian.org/pdf/debianmeetingresume200906-kansai.pdf

DebianLive - Debian Wiki (英語)
http://wiki.debian.org/DebianLive

Debian Live Manual (英語)
http://alioth.debian.org/~lamby-guest/live-manual/html/

