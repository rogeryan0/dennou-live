============================================================

　Debian Live DVD(lenny) / Debian Installer(lenny)
　TeX資料作成ハンズオンバージョン (2008.11.7-8 KOF)

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

　以前配布したDebian Liveとの違いは以前の内容に加え、東京エリ
ア・関西Debian勉強会の資料作成、管理に必要なemacs, git, TeXの
環境がほぼすべて入っています。　
　関西Debian勉強会では、このDebian Liveを使い、資料を作成、管
理するためのハンズオンを予定しています。
　ハンズオン参加のための準備方法については「資料作成ハンズオ
ンの準備について」をご覧ください。

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
　インストール方法については現安定版のEtchとほぼ同じ手順です
ので下記のURIを参考にしてください。

Debian JP Project - Debian GNU/Linux クイックインストール解説 (Etch 編)
http://www.debian.or.jp/using/quick-etch/



資料作成ハンズオンの準備について
------------------------------------------------------------
　資料作成ハンズオンの会場には、ネットワーク環境もなく、時間
も限られているので、参加する方は以下の準備をしておいてください。

1. USBメモリにDVDの差分を保存できるようにする。
2. apt-lineのnon-freeセクションを有効にし、poppler-data,
   gs-cjk-resource, xpdf-japaneseのパッケージをインストールする。
3. debian勉強会の資料リポジトリからリポジトリを複製し、コンパ
   イルできる事を確認する。


1. USBメモリにDVDの差分を保存する。
------------------------------------------------------------
　Debian Liveは起動時にlive-rwとラベルのついたext2/ext3パーティ
ションをみつけると自動的にマウントしDVDの差分を保存します。
　ここではUSBメモリにDVDの差分を保存するための方法を説明します。

用意するもの:
　・Debian Live DVD
　・USBメモリ(資料作成ハンズオンの準備には512MB以上)

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
ト、nは番号)。自分の環境に合わせて適当に読み替えてください。

 パーティションを確認する。

 # fdisk -l

　cfdisk(またはfdisk)でパーティションを作成します。

 # cfdisk /dev/sdX

　「live-rw」というラベル名でext3でフォーマットします。

 # mkfs.ext3 -l live-rw /dev/sdXn

　tune2fsでfsckをしないようにしておくのもいいかもしれません。

 # tune2fs -i 0 -c 0 /dev/sdXn 

　もし後からラベル名をつけるときはtune2fsで変更します。

 # tune2fs -L live-rw /dev/sdXn

3 再起動して確認する
　USBメモリを差したままDVDから起動します。
 起動してUSBメモリのパーティションが/live/cowディレクトリにマ
ウントされていれば、差分保存ができます。　


2. non-freeセクションのパッケージインストール
------------------------------------------------------------
　PDFを読むためにはpoppler-dataパッケージが必要ですが、ライセ
ンスの都合上、DVDに納めることができませんでした。
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
gs-cjk-resourceとxpdf-japaneseも必要になりますが、それぞれ
Adobe CMAPに依存しているため同様にこのDVDには含まれていません。
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
　ホームディレクトリで以下のコマンドを実行します。
(東京エリアDebian勉強会2008年11月資料より抜粋)

$ git clone git://git.debian.org/git/tokyodebian/monthly-report.git
$ cd monthly-report
$ cp -p git-pre-commit.sh .git/hooks/pre-commit
$ make -j4
$ ls *.pdf # 110くらいのPDFファイルが生成されていることを確認

　makeはマシンにもよりますが、非力なマシンでは、かなり時間が
かかります。

　以上で資料作成ハンズオンのための環境が整いました。



Debian Liveのカスタマイズについて
------------------------------------------------------------
　このDebian Live DVDは、資料作成ハンズオン準備の「USBメモリ
にDVDの差分を保存する。」を参考にUSB HDD上に差分保存パーティ
ションを用意することにより、LiveDVD上でカスタマイズすることが
できます。
　カスタマイズには10GBほど容量が必要になります

　同じものをビルドするには、以下のコマンドでビルドすることができます。

$ git clone git://github.com/nogajun/debian-study-live-cd.git
$ make

　live-helperの設定を変更する事により、さまざまなカスタマイズ
することが可能ですので、ぜひチャレンジしてみてください。

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
る方は、ぜひ参加してください。

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

　http://wiki.debian.org/KansaiDebianMeetingKOF2008


------------------------------------------------------------
のがたじゅん <nogajun@gmail.com>
