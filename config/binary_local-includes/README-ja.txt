======================================================================

　Debian Live DVD (Lenny) / Debian Installer (Lenny)
 オープンソースカンファレンス2009 Kansai (2009.07.10-11)

　Debian JP Project / 関西Debian勉強会
　のがたじゅん <nogajun@debian.or.jp>

======================================================================

目次
----------------------------------------------------------------------
+ Debian Liveについて
  1. Live DVDとして使う
  2. Debian Installerとして使う
+ Debian Liveのカスタマイズ
+ Debian勉強会のお知らせ
+ 追加情報など


Debian Liveについて
----------------------------------------------------------------------
　このDVDは、Debian Live Projectのlive-helperを用いて制作されたDebian
LiveとDebian InstallerのハイブリッドDVDです。
　以前配布したDebian Liveとの違いは、以前の内容に加え以下のような違いが
あります。

  ・Debian GNU/Linux 5.0.2
  ・backports.orgより以下のパッケージを追加。
　  OpenOffice.org 3.1.0
　  Pidgin 2.5.5
  ・live-initramfsにDebian Live Projectのスナップショット版を使用。
  ・デスクトップにアイコンが作成されます。
  ・ネットワークからNVIDIA、ATIのプロプライエタリドライバをインストール
  　して起動することが可能。(live-initscripts)

　その他、東京エリア・関西Debian勉強会の資料作成、管理に必要なemacs,
git, TeXの環境や、このDebian Liveをカスタマイズするための環境、レスキュー
ツールなど入っています。
　パッケージの一覧についてはDVDのlive/packages.txtをご覧ください。


Live DVDとして使う
----------------------------------------------------------------------
　Debian Liveを起動するにはメニューの「Live」を選択します。
　それぞれのメニューは以下のようになっています。

  ・Live
  　通常起動モードです。

  ・Live (NVIDIA)
  ・Live (ATI)
  　起動時にプロプライエタリドライバをダウンロードして起動するモードです。
  　ネットワークに接続している必要があります。

  ・Live (persistent)
  　データを保存するディレクトリをマウントして起動します。
  　データの保存方法については「USBメモリにDVDの差分を保存する」をご覧ください

  ・Live (US Keyboard)
  　英語キーボードモードです。

  ・Live (fail-safe mode)
  　すべてのオプションを無効にして起動するモードです。
  　うまく起動できない場合に試してみてください。

　起動時のパラメータを追加するには、メニューの上で「e」キーを押して追加します。
　パラメータの詳細については、DVDのlive/parameters.txtをご覧ください。


Debian Installerとして使う
----------------------------------------------------------------------
　Debian GNU/Linux 5.0(Lenny)をハードディスクにインストールするには、メ
ニューから「Install (GUI)」を選択してください。
　インストール方法については下記のURIを参考にしてください。

 Debian JP Project - Debian GNU/Linux クイックインストール解説 (Etch/Lenny 編)
 http://www.debian.or.jp/using/quick-etch/

 Debian GNU/Linux 5.0：ITpro
 http://itpro.nikkeibp.co.jp/article/COLUMN/20090326/327243/?ST=lin-os&P=1


USBメモリにDVDの差分を保存する
----------------------------------------------------------------------
　Debian Liveは、起動時に「live-rw」もしくは「home-rw」というラベル名の
ext2/ext3パーティションをみつけると自動的にマウントし、DVDの差分を保存
します。(live-rwは/(ルート)ディレクトリ全体を、home-rwはhomeディレクト
リを保存します。)
　ここではUSBメモリにDVDの差分を保存するための方法を説明します。

用意するもの:
　・Debian Live DVD
　・USBメモリ

1. DVDを起動しUSBメモリをセットする。
　USBメモリなどリムーバブルデバイスをセットすると自動的にマウントされて
しまうので、アンマウントしておきます。
　アンマウントの方法は、マウントされたデバイスアイコン上のコンテキスト
メニュー(マウス右クリックメニュー)から、「アンマウント」を選びます。

2. 保存パーティションの作成
　「live-rw」もしくは「home-rw」というラベル名で、ext3パーティションを
作成します。作成方法はGUIでもターミナル、どちらでも構いません。

2.a GUI(gparted)からパーティション作成
　デスクトップにある「Partition Editor」アイコンをクリックします。
　右上のプルダウンメニューからUSBメモリを選び、パーティションを適当なサ
イズにリサイズ(もしくは削除)します。
　空いた領域にext2パーティションを作成をしますが、この時、ラベル名を
「live-rw」もしくは「home-rw」と名づけることを忘れないでください。

2.b ターミナルからパーティション作成
　GNOMEメニューの「アプリケーション」→「アクセサリ」→「Root　
Terminal」を選択し作業をします。
　/dev/sdXnはUSBメモリのデバイスファイル名です(Xはアルファベット、nはパー
ティション番号)。dmesgの内容を確認した上で自分の環境に合わせて適宜読み
替えてください。

 パーティションを確認する。

 # fdisk -l

　cfdisk(またはfdisk)でパーティションを作成します。

 # cfdisk /dev/sdX

　「live-rw」というラベル名でext3でフォーマットします。

 # mkfs.ext2 -L live-rw /dev/sdXn

　tune2fsでfsckをしないようにしておくといいでしょう。

 # tune2fs -i0 -c0 /dev/sdXn 

　もし後からラベル名をつけるときはtune2fsで変更します。

 # tune2fs -L live-rw /dev/sdXn

3. DVDを再起動して確認する
　USBメモリを差したままDVDをpersistentオプションで起動します。
　起動してUSBメモリのパーティションが/live/cowディレクトリにマウントさ
れていれば、差分保存ができます。　


Debian Liveのカスタマイズについて
----------------------------------------------------------------------
　このDebian Live DVDは、githubにあるレシピを利用してカスタマイズするこ
とができます。
　カスタマイズにはDebian GNU/Linux上作業する必要がありますが、このLive
DVDを使う場合、「USBメモリにDVDの差分を保存する。」を参考に、USB HDD上
に差分保存パーティションを用意すると、LiveDVD上でカスタマイズすることが
できます。
　カスタマイズには10GBほど容量が必要になります

　同じものをビルドするには、以下のコマンドでビルドすることができます。

 $ git clone git://github.com/nogajun/debian-study-live-cd.git
 $ cd debian-study-live-cd
 $ make

　live-helperの設定を変更する事により、さまざまなカスタマイズをすること
が可能ですので、ぜひチャレンジしてみてください。

　資料については、英語の資料を見るのが手っ取り早いのですが、日本語の資
料がほしいならば、関西Debian勉強会で筆者が発表した資料があるので、それ
を参考にするとわかりやすいと思います。

関西Debian勉強会　資料
KansaiDebianMeetingArchivesの添付ファイル:debianmeetingresume200906-kansai.pdf - Debian Wiki
http://wiki.debian.org/KansaiDebianMeetingArchives?action=AttachFile&do=view&target=debianmeetingresume200906-kansai.pdf

nogajun's debian-study-live-cd at master ― GitHub
http://github.com/nogajun/debian-study-live-cd/

DebianLive - Debian Wiki (英語)
http://wiki.debian.org/DebianLive

Debian Live Manual (英語)
http://alioth.debian.org/~lamby-guest/live-manual/html/


Debian勉強会のお知らせ
----------------------------------------------------------------------
　Debian勉強会とは、Debianの開発者になれることをひそかに夢見るユーザた
ちと、ある時にはそれを優しく手助けをし、またある時には厳しく叱咤激励す
る Debian開発者らがFace to FaceでDebian GNU/Linuxのさまざまなトピック
（新しいパッケージ、Debian特有の機能の仕組について、Debian界隈で起こっ
た出来事、etc）について語り合うイベントです。

　毎月、関東、関西で開かれているので、Debianの開発に興味のある方は、ぜ
ひご参加ください。

　開催日時については、Debian JP ProjectのWebサイト、debian-usersメーリ
ングリスト、mixiのDebianコミュニティなどでアナウンスされます。

 東京エリアDebian勉強会
 http://tokyodebian.alioth.debian.org/

 関西Debian勉強会
 http://wiki.debian.org/KansaiDebianMeeting

 Debian JP Project
 http://www.debian.or.jp/

 Debian Project
 http://www.debian.org/


追加情報など
----------------------------------------------------------------------
　Debian Live DVD制作後の追加情報や訂正は以下のURIにてアナウンスされる
予定です。
　http://wiki.debian.org/KansaiDebianMeetingOSC2009

----------------------------------------------------------------------
