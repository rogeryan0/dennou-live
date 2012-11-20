Debian Live DVD/USB (Wheezy) with GFD-Dennou Club dcmodel/davis product
========================================================================

Copyright(c) 2012: Youhei SASAKI <uwabami@gfd-dennou.org>


はじめに/ 追加情報など
----------------------

Debian Live DVD 制作後の追加情報や訂正は以下の URI にてアナウンスされます.
この文書とあわせてお読みください.

http://www.gfd-dennou.org/library/spmodel/LiveCD/index.htm


目次
----

* Debian Live について
  1. Live DVD/USB として使う
  2. DVD の差分を USB メモリに保存する
  3. よくある質問
* Debian Live のカスタマイズ


Debian Live について
--------------------

この DVD/USB は, Debian Live Project の live-build を用いて制作されました.
Debian GNU/Linux 7.0 (Wheezy) ベースで作られており, 地球流体電脳倶楽部の
ソフトウェアを気軽に試せるようになっています.

収録パッケージの詳細については DVD 内の live/packages.txt をご覧ください.

Live DVD/USB として使う
-----------------------

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
のext2/3/4 パーティションをみつけると自動的にマウントし, DVD の差分を保存
します. live-rw は/ (ルート) ディレクトリ全体を, home-rw は home ディレク
トリを保存します. DVD から起動し, 変更を保存したい場合には, 適宜ラベルを
つけた USB メモリを用意して下さい.

尚, 実習時に配布した USB の場合は, live-rw として, 全ての変更内容を保存
するようにしてあります.

よくある質問
-------------

Q. ネットワークに接続するには?
A. wicd を使用します. 右上のネットワークのアイコンをクリックするか, 
   terminal 上で

  > wicd-gtk -n

と入力し, 設定ダイアログを開いて下さい. 設定の詳細は man を参照して下さい.

Q. ログイン画面に戻ってしまいました
A. 10 秒待つと自動的にログインします.
   すぐにログインする場合のユーザー名とパスワードは以下のとおりです.

  > ユーザー名: user
  > パスワード: live

Q. root 権限になるには, どうすればいいですか?
A. sudo を使って下さい.

Q. キーボードが英語(日本語)キーボード配列になっている
A. GDM のログイン画面で日本語(英語)キーボードを選択してください.

Debian Live のカスタマイズについて
-----------------------------------

今回配布した DVD/USB のイメージを作成するためのレシピは github で公開し
ています. カスタマイズをおこなうには Debian GNU/Linux 上で作業をする必
要がありますが, Live DVD 上からもカスタマイズは可能ですので, 興味のある
方はチャレンジしてみたください.

カスタマイズには 10GB 程度の保存領域が必要になるので, USB 接続 HDD など
を用意して保存領域を確保する必要があります.

ビルドの方法は, 以下のコマンドによりビルドすることができます.

 $ git clone git://github.com/uwabami/dennou-live.git
 $ cd dennou-live.git
 $ make

live-build の設定については, Debian Live のサイトを見るのが一番です. 日本
語の資料では, 関西 Debian 勉強会の資料があるのでそれを参考にするとよいで
しょう.

関西 Debian 勉強会  資料
http://tokyodebian.alioth.debian.org/pdf/debianmeetingresume200906-kansai.pdf

DebianLive - Debian Wiki (英語)
http://wiki.debian.org/DebianLive

Debian Live Manual (英語)
http://live.debian.org/live-manual/html/
