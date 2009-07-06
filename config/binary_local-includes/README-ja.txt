============================================================

　Debian Live DVD (Lenny) / Debian Installer (Lenny)
 オープンソースカンファレンス2009 Kansai (2009.07.10-11)

============================================================

目次
------------------------------------------------------------
+ Debian Liveについて
  1. Live DVDとして使う
  2. Debian Installerとして使う
+ 資料作成ハンズオンの準備
  1. USBメモリにDVDの差分を保存する
  2. non-freeセクションのパッケージをインストール
  3. 資料リポジトリの複製とコンパイルの確認
+ Debian Liveのカスタマイズ
+ Debian勉強会のお知らせ
+ 追加情報など


Debian Liveについて
------------------------------------------------------------
　このDVDは、Debian Live Projectのlive-helperを用いて制作され
たDebian LiveとDebian InstallerのハイブリッドDVDです。

　以前配布したDebian Liveとの違いは、以前の内容に加え以下のよ
うな違いがあります。

・Debian GNU/Linux 5.0.2ベース
・backports.orgより以下のパッケージを追加。
　Kernel 2.6.30
　OpenOffice.org 3.1.0
　Pidgin
・live-initramfsにDebian Live Projectのスナップショット版を
 使用。
・デスクトップにアイコンが作成されるにした。
・起動オプションにより、ネットワークからNVIDIA、ATIのプロプライエタリド
 ライバをインストールすることが可能。

　その他、東京エリア・関西Debian勉強会の資料作成、管理に必要
なemacs, git, TeXの環境や、このDebian Liveをカスタマイズする
ための環境、レスキューツールなど入っています。

Live DVDとして使う
------------------------------------------------------------
　DVDを起動してメニューから「Debian Live」を選択します。

　「Debian Live(noswap/nopersistent)」は、通常はスワップパー
ティションやDVDの差分保存パーティションを探して自動的マウント
しますが、このメニューではそれらのパーティションは自動的にマ
ウントしません。
　うまく起動できない場合は、「Debian Live(fail-safe mode)」を
試してみてください。

Debian Installerとして使う
------------------------------------------------------------
　起動時のメニューから「Debian Installer」を選択してください。
Debian GNU/Linux 5.0(Lenny)をHDDにインストールすることができ
ます。　
　インストール方法については下記のURIを参考にしてください。

Debian JP Project - Debian GNU/Linux クイックインストール解説 (Etch/Lenny 編)
http://www.debian.or.jp/using/quick-etch/

Debian GNU/Linux 5.0：ITpro
http://itpro.nikkeibp.co.jp/article/COLUMN/20090326/327243/?ST=lin-os&P=1




1. USBメモリにDVDの差分を保存する。
------------------------------------------------------------
　Debian Liveは起動時にlive-rwとラベルのついたext2/ext3パーティ
ションをみつけると自動的にマウントしDVDの差分を保存します。
　ここではUSBメモリにDVDの差分を保存するための方法を説明します。

用意するもの:
　・Debian Live DVD
　・USBメモリ

1. DVDを起動しUSBメモリをセットする。
　この時USBメモリが自動的にマウントされてしまうので、マウント
されたデバイスアイコン上のコンテキストメニュー(マウス右クリッ
クメニュー)から「アンマウント」を選んでアンマウントしておきます。

2. 保存パーティションの作成
　「live-rw」というラベル名でext3パーティションを作成します。
作成方法はGUIでもターミナル、どちらでも構いません。

2.a gpartedからパーティション作成
　GNOMEメニューの「システム」→「システム管理」→「Partition
Editor」を選択し、gpartedを起動します。
　右上のプルダウンメニューからUSBメモリのデバイスを選び、パー
ティションを適当なサイズにリサイズ(もしくは削除)し、空いた領
域にext3パーティションを作成します。
　この時、ラベル名を「live-rw」と名づけることを忘れないでくだ
さい。

2.b ターミナルからパーティション作成
　GNOMEメニューの「アプリケーション」→「アクセサリ」→「Root　
Terminal」を選択し作業をします。
　/dev/sdXnはUSBメモリのデバイスファイル名です(Xはアルファベッ
ト、nはパーティション番号)。dmesgの内容を確認した上で自分の環
境に合わせて適宜読み替えてください。

 パーティションを確認する。

 # fdisk -l

　cfdisk(またはfdisk)でパーティションを作成します。

 # cfdisk /dev/sdX

　「live-rw」というラベル名でext3でフォーマットします。

 # mkfs.ext3 -l live-rw /dev/sdXn

　tune2fsでfsckをしないようにしておくのもいいかもしれません。

 # tune2fs -i 0 -c 0 -L live-rw /dev/sdXn 

　もし後からラベル名をつけるときはtune2fsで変更します。

 # tune2fs -L live-rw /dev/sdXn

3. DVDを再起動して確認する
　USBメモリを差したままDVDを起動します。
　起動してUSBメモリのパーティションが/live/cowディレクトリに
マウントされていれば、差分保存ができます。　


2. non-freeセクションのパッケージインストール
------------------------------------------------------------
　文書ビューアのevinceを使ってPDFを読むためにはpoppler-dataパッ
ケージが必要ですが、ライセンスの都合によりDVDに納めることがで
きませんでした。
　以下Lennyリリースノートより引用。

==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--
中国語／日本語／韓国語を含む PDF ファイルの閲覧について
-------------------------------------------------------
今回のリリースより、evince を利用して中国語／日本語／韓国語を
含む PDF ファイルを開く際、ほとんどの場合は対応する中国語／日
本語／韓国語フォントだけではなく non-free コンポーネントに含
まれる poppler-data も必要となります。この poppler-data に含
まれる Adobe CMAP データを利用すると、それまで正常に表示がで
きなかった中国語／日本語／韓国語の文字表示が可能となります。

なお、残念ながら poppler-data パッケージは改変再配布が認めら
れていない non-free コンポーネントのため、デフォルトの「デス
クトップ環境」ではインストールされません。evince にて中国語／
日本語／韓国語を含む PDF ファイルを正常に表示できないという場
合は /etc/apt/sources.list 内、あるいは
/etc/apt/sources.list.d ディレクトリ以下の apt line を編集し
て non-free コンポーネントを利用するように変更してから
apt/aptitude のデータベースの update を実施し、poppler-data
パッケージをインストールしてください。

以前のリリースからのアップグレードを行った方への注意：
evince では、今回のリリースから Adobe CMAP を参照するプログラ
ムの実装が変わったため、正常な動作を行うには、さらに
poppler-data パッケージのインストールが必要となる点についてご
注意願います。
==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--

　poppler-data以外にも、資料作成ハンズオンには
gs-cjk-resourceとxpdf-japaneseパッケージも必要になりますが、
それぞれAdobe CMAPデータに依存しているため、同様にこのDVDには
含まれていません。
　これらのパッケージは自分でインストールする必要があります。

a. Synapticからインストール
　Synapticを起動して「設定」→「リポジトリ」を開いて以下の個所
にチェックを入れます。

　・非フリーな依存関係のあるDFSG適合ソフトウェア(contrib)
　・DFSGに適合しないソフトウェア(non-free)

　「再読込」ボタンを押して、poppler-data, gs-cjk-resource,
xpdf-japaneseパッケージをインストールします。　

b. ターミナルからインストール
 # sed -i 's#ftp.jp.debian.org/debian/ lenny main$#& contrib non-free#g' /etc/apt/sources.list
 # aptitude update
 # aptitude install poppler-data gs-cjk-resource xpdf-japanese



3. 資料リポジトリの複製とコンパイルの確認。
------------------------------------------------------------
　gitを使ってdebian勉強会資料リポジトリをホームディレクトリに
複製し、TeX 文書がコンパイルできることを確認します。
　ホームディレクトリで以下のコマンドを実行します。
(東京エリアDebian勉強会2008年11月資料より抜粋)

$ git clone git://git.debian.org/git/tokyodebian/monthly-report.git
$ cd monthly-report
$ cp -p git-pre-commit.sh .git/hooks/pre-commit
$ make -j4
$ ls *.pdf # 110くらいのPDFファイルが生成されていることを確認

　コンパイル作業はすべての勉強会資料をコンパイルするので、非
力なマシンでは、かなり時間がかかります。

　以上で資料作成ハンズオンのための環境が整いました。



Debian Liveのカスタマイズについて
------------------------------------------------------------
　このDebian Live DVDは、資料作成ハンズオン準備の「USBメモリ
にDVDの差分を保存する。」を参考にUSB HDD上に差分保存パーティ
ションを用意すると、LiveDVD上でカスタマイズすることができます。
　カスタマイズには10GBほど容量が必要になります

　同じものをビルドするには、以下のコマンドでビルドすることができます。

$ git clone git://github.com/nogajun/debian-study-live-cd.git
$ cd debian-study-live-cd
$ make

　live-helperの設定を変更する事により、さまざまなカスタマイズ
をすることが可能ですので、ぜひチャレンジしてみてください。

DebianLive - Debian Wiki
http://wiki.debian.org/DebianLive

Debian Live Manual
http://alioth.debian.org/~lamby-guest/live-manual/html/

nogajun's debian-study-live-cd at master ― GitHub
http://github.com/nogajun/debian-study-live-cd/



Debian勉強会のお知らせ
------------------------------------------------------------
　Debian勉強会とは、Debianの開発者になれることをひそかに夢見
るユーザたちと、ある時にはそれを優しく手助けをし、またある時
には厳しく叱咤激励する Debian開発者らがFace to FaceでDebian
GNU/Linuxのさまざまなトピック（新しいパッケージ、Debian特有の
機能の仕組について、Debian界隈で起こった出来事、etc）について
語り合うイベントです。

　毎月、関東、関西で開かれているので、Debianの開発に興味のあ
る方は、ぜひご参加ください。

　開催日時については、Debian JP ProjectのWebサイト、
debian-usersメーリングリスト、mixiのDebianコミュニティなどで
アナウンスされます。

東京エリアDebian勉強会
http://tokyodebian.alioth.debian.org/

関西Debian勉強会
http://wiki.debian.org/KansaiDebianMeeting

Debian Project
http://www.debian.org/

Debian JP Project
http://www.debian.or.jp/



追加情報など
------------------------------------------------------------

　Debian Live DVD制作後の追加情報や訂正は以下のURIにてアナウ
ンスされる予定です。
　http://wiki.debian.org/KansaiDebianMeetingOSC2009

------------------------------------------------------------

関西Debian勉強会
のがたじゅん <nogajun@debian.or.jp>
