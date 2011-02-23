=begin

# * Shin-Ichi Takehiro, Youhei SASAKI, Masatsugu Odaka
#   * $Id: INSTALL.rd,v 1.3 2009-07-06 09:03:21 uwabami Exp $
# * 2009/03/01(佐々木洋平) INSTALL ドキュメントを整理
# * 2009/03/01(佐々木洋平) 微修正
# * 2009/02/25(佐々木洋平) RD 化
#   * 過去の更新履歴
#      * 2007/12/04(竹広真一)
#      * 2005/02/01(竹広真一)
#      * 2005/01/10(竹広真一)
#      * 2004/01/27(小高正嗣)
#      * 2004/01/26(小高正嗣)
#      * 2002/05/20(竹広真一)
=end

=begin JA
= spml インストールガイド
=end JA

=begin EN
= Spml  Installation Guide
=end EN

=begin JA
== 動作環境
spml ライブラリは以下の環境での動作を確認しています.
=end JA

=begin EN
== Operation Environment
Spml library is operated by following environments.
=end EN

=begin
 * OS: ((<"Debian GNU/Linux"|URL:http//www.debian.org/>))
 * Fortran Compiler
   * ((<G95 Fortran Compiler ver. 0.91|URL:http://www.g95.org/>))
     * with ((<MPICH2 1.0.7|URL:http://www.mcs.anl.gov/research/projects/mpich2/>))
     * with ((<OpenMPI 1.3.2|URL:http://www.open-mpi.org/>))
   * ((<Fujitsu Fortran Driver ver.1.0(Japanese)|URL:http://www.fqs.fujitsu.com/fort-c/>))
     * with ((<MPICH2 1.0.7|URL:http://www.mcs.anl.gov/research/projects/mpich2/>))
     * with ((<OpenMPI 1.3.2|URL:http://www.open-mpi.org/>))
   * ((<Fujitsu Fortran Compiler ver.5.0(Japanese)|URL:http://www.fqs.fujitsu.com/fort-c/>))
     * with ((<MPICH2 1.0.7|URL:http://www.mcs.anl.gov/research/projects/mpich2/>))
   * ((<Intel Fortran Compiler ver 10.0, 11.0|URL:http://www.intel.com/software/products/compilers/flin/>))

=end



=begin JA

過去には以下の環境でも動作したことが確認されています.
確認はしていませんが, 現在のバージョンでもおそらく動作すると期待されます.

=end JA
=begin EN
Spml library was operated by following environments in the past.
Latest version may be operated (unconfirmed).
=end EN
=begin

  * Cray XT4 + PGI Fortran
  * NEC SX-6 + FORTRAN90/SX
  * NEC SX-8 + FORTRAN90/SX
  * NEC SX-8R + FORTRAN90/SX
  * NEC SX-9 + FORTRAN90/SX
  * Linux + PGI Fortran

=end
=begin JA
== インストール作業の概略

spml ライブラリのインストールは以下の様に行ないます.
詳しくは各項目を参照して下さい.

  (1) ((<必要なソフトウェア>)) をインストールします.
  (2) ((<ビルドの手引き>)) に従い, ソースからライブラリをビルドします.
  (3) ((<インストールの手引き>)) に従い, ライブラリをインストールします.
  (4) ((<テストプログラム実行の手順>)) に従い, インストールされた
      ライブラリが正常に機能するかどうか確認してください.
  (5) ((<パスの設定>)) を行ってください.

=end
=begin EN
== General outline

Install spml as follows. Refer each items for details.

  (1) Satisfy ((<Software Requirements>)).
  (2) Build the library following ((<How to build>)).
  (3) Install the library following ((<How to install>)).
  (4) Check whether the installed library functions normally
      following ((<Execute test programs>)).
  (5) ((<Set PATH>)).

=end

=begin JA
== 必要なソフトウェア

spml を利用するためには, 以下のソフトウェアを
事前にインストールしておく必要があります.

* ((<netCDF|URL:http://www.gfd-dennou.org/library/netcdf>)), version 3.6 以上
* ((<gtool5|URL:http://www.gfd-dennou.org/library/gtool>))
* ((<ispack|URL::http://www.gfd-dennou.org/library/ispack>))

=end

=begin EN
== Software Requirements

The following software needs to use spml
* ((<netCDF|URL:http://www.gfd-dennou.org/library/netcdf>)), version 3.6
* ((<gtool5|URL:http://www.gfd-dennou.org/library/gtool>))
* ((<ispack|URL::http://www.gfd-dennou.org/library/ispack>))

=end EN

=begin JA
== ビルドの手引き
=end JA

=begin EN
== How to build
=end EN

=begin JA
=== TGZ パッケージの展開

適当な作業ディレクトリでソースアーカイブを展開します.
ソースは spml-((|バージョン|)) というディレクトリに展開されます.

        $ tar xvzf spml_current.tgz

または

        $ zcat spml_current.tar.gz | tar -xvf -

=end JA

=begin EN
=== Extract TGZ Package

Make an empty directory, and extract archive.
A directory `spml-((|version|))'
created at the current working directory.

        $ tar xvzf spml_current.tgz

or

        $ zcat spml_current.tar.gz | tar -xvf -

=end EN

=begin JA
=== Fortran コンパイラの指定

環境変数 ((* FC *)) に使用する Fortran コンパイラを指定してください.
以下は, 利用するコンパイラが frt の場合です.

* sh, bash の場合

        $ FC=frt ; export FC

* csh, tcsh の場合

        $ setenv FC frt

最適化やデバッグのためのオプションは環境変数 ((* SYSFFLAGS *))
に設定してください. 以下の例は Fujitsu Fortran を使用する場合の高速化と
並列化のためのオプションです.

* sh, bash の場合

        $ SYSFFLAGS="-Kfast,parallel" ; export FFLAGS

* csh, tcsh の場合

        $ setenv SYSFFLAGS "-Kfast,parallel"

=end JA
=begin EN
=== Specify Fortran Compiler

Specify Fortran compiler to environment variable ((* FC *)).
For example, if you use "frt",

* sh, bash

        $ FC=frt ; export FC

* csh, tcsh

        $ setenv FC frt

Specify Fortran compiler options for optimization and debug to
environment variable ((* SYSFFLAGS *)).
For example, if you set options for automatic optimization and
automatic parallelization to Fujitsu Fortran,

* sh, bash

        $ SYSFFLAGS="-Kfast,parallel" ; export FFLAGS

* csh, tcsh

        $ setenv SYSFFLAGS "-Kfast,parallel"

=end EN


=begin JA
=== Config.mk の作成

展開されたディレクトリに移動し, (({ ./configure }))を実行します. 
* (({ --with-netcdf= })) には netCDF ライブラリのパスを指定します. 
  (以下の例は /usr/local/netcdf/lib/libnetcdf.a にライブラリがある 
  場合のものです). 
* (({ --with-gtool5= })) には gtool5 ライブラリのパスを指定します. 
  (以下の例は /usr/local/gtool5/lib/libgtool5.a にライブラリがある 
  場合のものです). 
* (({ --with-ispack= })) には ispack ライブラリのパスを指定します. 
  (以下の例は /usr/local/ispack/lib/libisp.a にライブラリがある 
  場合のものです). 

このコマンドによって (({ Config.mk })) ファイルが生成されます. 
netCDF ライブラリが共有ライブラリである場合, 
(({ --with-netcdff= })) も指定する必要があるかもしれません. 
詳しくは下記のオプションの詳細を参照してください. 

        $ ./configure --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a \
                      --with-gtool5=/usr/local/gtool5/lib/libgtool5.a \
                      --with-ispack=/usr/local/ispack/lib/libisp.a 

spml を MPI 用にビルドする場合には,
((<MPI 用にビルドする場合には>)) を参照してください. 

ビルドには GNU make が必要となるため, configure は PATH 内から
自動で GNU make を探査しようとしますが, もし見つからない場合,
エラーを返します. その場合には環境変数 ((* MAKE *)) に
GNU make コマンドを指定して再度 configure を実行してください.

インストール先などを変更したい場合は, 以下のように (({ --help })) オプ
ションをつけることで, 指定可能なオプションリストが表示されます.

        $ ./configure --help

主なオプションに関しての説明です.

:(({--with-netcdf=}))((|ARG|))
  ((|ARG|)) に ((<ビルドに必要な netCDF ライブラリ>)) 
  を指定します. 必ず明示的に指定する必要があります.

:(({--with-netcdff=}))((|ARG|))
  netCDF ライブラリが共有ライブラリである場合, C 用ライブラリと
  Fortran 用ライブラリとに分かれてビルドされている場合があります.
  その際は, 上記オプションに C 用ライブラリを指定し, 本オプションの
  ((|ARG|)) に ((<Fortran 用ライブラリ>)) を指定します.

:(({--with-netcdf-include=}))((|ARG|))
  必要ならば netCDF ライブラリの Fortran 用ヘッダ(netcdf.inc) を指定します. 

:(({--with-gtool5=}))((|ARG|))
  ((|ARG|)) に ((<ビルドに必要な gtool5 ライブラリ>)) 
  を指定します. 必ず明示的に指定する必要があります.

:(({--with-ispack=}))((|ARG|))
  ((|ARG|)) に ((<ビルドに必要な ispack ライブラリ>)) 
  を指定します. 必ず明示的に指定する必要があります.

:(({--with-ssl2=}))((|ARG|))
:(({--with-ssl2tp=}))((|ARG|))
:(({--with-lapack=}))((|ARG|))
  ((|ARG|)) に ((<固有値問題を解くために使用するライブラリ>))
  を指定します. 

:(({--with-fftw=}))((|ARG|))
:(({--with-rfftw=}))((|ARG|))
  フーリエ変換に FFTW ver.2 を使用する場合に指定します.
  ((<ispack>)) を -FFTW2 付きでビルドしている場合には指定する必要があります.

:(({--with-mpifc=}))((|ARG|))
  MPI 並列を有効にする場合に指定します. 

:(({--prefix=}))((|ARG|))
  ((|ARG|)) にライブラリやモジュール, 実行ファイルのインストール先の
  ディレクトリのプレフィックスを指定します.
  デフォルトは (({ /usr/local/spml })) です.

:(({--host=}))((|ARG|))
  クロスコンパイルを行う場合には, パッケージが実行されるシステムタイプ名
  を ((|ARG|)) に指定します.

:(({--libdir=}))((|ARG|))
  ((|ARG|)) にライブラリのインストール先のディレクトリを指定します.
  デフォルトは (({ /usr/local/spml/lib })) です.

:(({--includedir=}))((|ARG|))
  ((|ARG|)) にモジュール情報ファイルのインストール先のディレクトリ
  を指定します. デフォルトは (({ /usr/local/spml/include })) です.

:(({--bindir=}))((|ARG|))
  ((|ARG|)) に実行ファイルのインストール先のディレクトリを指定します.
  デフォルトは (({ /usr/local/spml/bin })) です.

:(({--docdir=}))((|ARG|))
  ((|ARG|)) にドキュメント/マニュアルのインストール先のディレクトリを指定します.
  デフォルトは (({ /usr/local/spml/doc })) です.

:(({--with-abort=}))((|ARG|))
  ((|ARG|)) に (({abort, errtra-setrcd, exit, setrcd, stop})) のいずれか
  を指定することで, 終了時に用いる Fortran の内部関数を変更することが
  可能です. デフォルトは (({ abort })) です.

  : abort
    Fortran の内部サブルーチン abort で終了します.

  : errtra-setrcd
    Fujitsu Fortran の ERRTRA サービスサブルーチンを呼び出し,
    現在実行中のプログラム単位までのトレースバックマップを出力し,
    終了します. Fujitsu Fortran の SETRCD サービスサブルーチンにて,
    復帰コード 13 を設定し, 終了コードとして 3 を設定します.

  : exit
    Fortran の内部サブルーチン exit で終了します.

  : setrcd
    Fujitsu Fortran の SETRCD サービスサブルーチンを呼び出し,
    Fortran の復帰コードとして 3 を設定して終了します.

  : stop
    Fortran の内部関数 stop で終了します.

:(({--config-cache})) または (({-C}))

  (({ Config.mk })) ファイルが生成されると同時に, (({config.cache}))
  ファイルが作成され, (({ ./configure })) の引数に指定されたライブラリの位置
  などの情報が保持されます.

  再度 (({ ./configure })) を実行する際にもこのオプションを指定することで,
  (({config.cache})) が読み込まれ, 前回指定したオプション等が引き継がれます.
  既に存在する (({config.cache})) を無視する場合はこのオプションを
  指定せずに (({ ./configure })) を実行してください.

  例えば下記のように (({ ./configure })) を実行するとします.

      $ ./configure -C --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a
                       --with-gtool5=/usr/local/gtool5/lib/libgtool5.a
                       --with-ispack=/usr/local/ispack/lib/libisp.a

  すると次回以降, 下記のように (({ ./configure })) を実行することで
  ライブラリの位置情報などが引き継がれます.

      $ ./configure -C


=end JA
=begin EN

=== Create `Config.mk'

Move created directroy, and excute `(({ ./configure }))'.
You should set necessary library location, netCDF, gtool5, ispack.
If your path of netCDF library is `/usr/local/netcdf/lib/libnetcdf.a',
gtool5 library is `/usr/local/gtool5/lib/libgtool5.a',
ISPACK library is `/usr/local/ispack/lib/libisp.a',
you should set options as follow.
Then a configure file `Config.mk' will be created at
the current working directory.
If the netCDF library is a shared library, (({ --with-netcdff= }))
option may be needed.
See details of options as follows.

        $ ./configure --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a \
                      --with-gtool5=/usr/local/gtool5/lib/libgtool5.a \
                      --with-ispack=/usr/local/ispack/lib/libisp.a 

If spml is built for MPI, see ((<How to build for MPI>)).

GNU make is needed to build, so configure automatically inquires
into GNU make in PATH. However, it returns error when GNU make is
not found. In that case, please set the GNU make command for
environment variable ((* MAKE *)). And rerun execute `(({ ./configure }))'.

If you want to change directory to which the library and the module, etc.
are installed, please set (({ --help })) option as follow. Available
options are showed.

        $ ./configure --help

Descriptions about principal options are listed below.

:(({--with-netcdf=}))((|ARG|))
  Specify ((<netCDF library needed for build>)) to ((|ARG|)).
  You must specify explicitly.

:(({--with-netcdff=}))((|ARG|))
  If the netCDF library is a shared library, the library may be divided
  C library from Fortran library. In the case, specify the C library
  to above option, and specify 
  ((<the Fortran library>)) to ((|ARG|)) in this option.

:(({--with-netcdf-include=}))((|ARG|))
  Set location of netCDF header file for fortran(netcdf.inc), if you need.

:(({--with-gtool5=}))((|ARG|))
  Specify ((<gtool5 library needed for build>)) to ((|ARG|)).
  You must specify explicitly.

:(({--with-ispack=}))((|ARG|))
  Specify ((<ispack library needed for build>)) to ((|ARG|)).
  You must specify explicitly.

:(({--with-ssl2=}))((|ARG|))
:(({--with-ssl2tp=}))((|ARG|))
:(({--with-lapack=}))((|ARG|))
  Specify ((<library using solve eigenvalue problem >)) to ((|ARG|)),
  if you need.

:(({--with-fftw=}))((|ARG|))
:(({--with-rfftw=}))((|ARG|))
  Specify ((<library using FFTW ver.2>)).
  You must specify explicitly if you build ispack library with -FFTW2 option.

:(({--with-mpifc=}))((|ARG|))
  Specify ((<MPI Fortran Compiler>)) to ((|ARG|)) if you want to build MPI support.

:(({--prefix=}))((|ARG|))
  Specify prefix to ((|ARG|)).
  Default value is (({ /usr/local/spml })).

:(({--host=}))((|ARG|))
  When cross-compiling, specify
  the type of system on which the package will run to
  ((|ARG|)).

:(({--libdir=}))((|ARG|))
  Specify directory to which the library is installed to ((|ARG|)).
  Default value is (({ /usr/local/spml/lib })).

:(({--includedir=}))((|ARG|))
  Specify directory to which the module is installed to ((|ARG|)).
  Default value is (({ /usr/local/spml/include })).

:(({--bindir=}))((|ARG|))
  Specify directory to which the executable file is installed to ((|ARG|)).
  Default value is (({ /usr/local/spml/bin })).

:(({--with-docdir=}))((|ARG|))
  Specify directory to which the documentation file is installed to ((|ARG|)).
  Default value is (({ /usr/local/spml/doc })).

:(({--with-abort=}))((|ARG|))
  Specify one of (({abort, errtra-setrcd, exit, setrcd, stop})) to
  ((|ARG|)).
  Default value is (({ abort })).

  : abort
    Stop by intrinsic subroutine "abort".

  : errtra-setrcd
    Stop by Fujitsu Fortran service subroutine "ERRTRA".
    And outputs error trace back map.

  : exit
    Stop by intrinsic subroutine "exit".

  : setrcd
    Stop by Fujitsu Fortran service subroutine "SETRCD".
    And outputs error trace back map.

  : stop
    Stop by intrinsic subroutine "stop".

:(({--config-cache or -C}))
  (({ config.cache })) is created at the same time as (({ Config.mk }))'s
  being generated.
  (({ config.cache })) stores information investigated with
  (({ ./configure })).

  If you set this option, when you execute (({ ./configure })) again,
  (({config.cache})) is loaded.
  If you want to ignore (({config.cache})), don't set this option

  For example, execute (({ ./configure })) as follows.

    $ ./configure -C --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a \
                     --with-gtool5=/usr/local/gtool5/lib/libgtool5.a \
                     --with-ispack=/usr/local/ispack/lib/libisp.a 

  Then, information such as the locations of libraries is
  succeeded by executing (({ ./configure })) as follows after next time.

      $ ./configure -C

=end EN

=begin JA
=== MPI 用にビルドする場合には

spml を MPI 用にビルドする場合にはまず MPI ライブラリをシステムに
インストールしてください.

次に gtool5 を MPI サポートを有効にして install します. 
詳細については
((<gtool5 の install ドキュメント|URL:http://www.gfd-dennou.org/library/gtool5/INSTALL.htm>))を参照して下さい.

Config.mk を作成する場合には, 環境変数 FC に mpif90 などの MPI 用コンパイル
コマンドを指定してください.
そして, 以下のように configure にはオプション --with-mpifc を指定して下さい

    $ ./configure -C --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a \
                     --with-gtool5=/usr/local/gtool5/lib/libgtool5.a \
                     --with-ispack=/usr/local/ispack/lib/libisp.a \
                     --with-mpifc=/usr/local/mpich/mpif90

その他のオプションなどについては, ((<Config.mk の作成>)) を参照してください. 

=end JA

=begin EN
=== How to build for MPI

If spml is built for MPI, install MPI library to a system, and build gtool5
library with MPI support, see
((<"Gtool5 Installation Guide"|URL:http://www.gfd-dennou.org/library/gtool5/INSTALL.htm.en>)).

When "Config.mk" is edited, specify a compile commend like as "mpif90"
to environment variable ((* FC *)).
And specify "--with-mpiexec" option of "configure".

    $ ./configure -C --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a \
                     --with-gtool5=/usr/local/gtool5/lib/libgtool5.a \
                     --with-ispack=/usr/local/ispack/lib/libisp.a \
                     --with-mpifc=/usr/local/mpich/mpif90

See ((<Create `Config.mk'>)) about other options. 

=end EN

=begin JA
=== Config.mk の編集

環境に合わせ (({ Config.mk })) を手動で編集してください. 
下記の設定についてよく分からない場合にはとりあえず
((<ソースコードのコンパイル>)) へ進んでください. 

   FC          : Fortran コンパイラ

   SYSFFLAGS   : コンパイル時・リンク時に必要なフラグ
                 (Fortran コンパイラ実行時に必要なオプション)

   SYSLDFLAGS  : リンク時に必要なフラグ

   SYSLDLIBS   : リンク時に必要なライブラリ

   F90MODTYPE  : モジュール情報の渡し方

   DEST_LIB    : spml ライブラリインストールディレクトリ

   DEST_INC    : spml モジュールインストールディレクトリ

   DEST_BIN    : spml 実行ファイルインストールディレクトリ

   DEST_DOC    : spml ドキュメントファイルインストールディレクトリ

   MODS        : モジュールファイル拡張子

   MAKE        : GNU make コマンド

   AR          : アーカイブコマンド

   ARFLAGS     : アーカイブ時に必要なフラグ

   RANLIB      : アーカイブのインデックスを作成するコマンド

=end JA
=begin EN
=== Edit `Config.mk'

Edit `(({Config.mk}))' manually, if you want to change. 
If you do not understand following settings, 
go forward to ((<Compile source code>)) tentatively. 

   FC          : Fortran Compiler

   SYSFFLAGS   : Flags needed when compiled and linked

   SYSLDFLAGS  : Flags needed when linked

   SYSLDLIBS   : Libraries needed when linked

   F90MODTYPE  : Information of Modules
                 (std.mod, HP.mod, fqs.mod, intel.d, hitachi.f90)

   DEST_LIB    : Directory to which the library file is installed

   DEST_INC    : directory to which the module files are installed

   DEST_BIN    : directory to which the executable files are installed

   DEST_DOC    : directory to which the documantation files are installed

   MODS        : Extensions of Module Files used when "make clean"

   MAKE        : GNU make command

   AR          : Archive command

   ARFLAGS     : Flags of AR

   RANLIB      : Generate index to archive


=end EN

=begin JA
=== ソースコードのコンパイル

ビルドには必ず GNU make を使用してください. 他の "make" プログラムを使
用すると, 正しくビルドが行われません.
以降 GNU make のコマンド名を "make" と表記しますが,
これらはシステムの GNU make コマンドの名前に置き換えてください.

./configure を実行すると, 以下のように GNU make のコマンド名が
表示されます. このメッセージに従って GNU make を実行してください.

  Execute GNU make in the current directory, as follows.

    /usr/bin/make

=end JA
=begin EN
=== Compile source code

You must use GNU make to build. No other "make" program is acceptable.
"make" tentatively means GNU make at the following.
Replace them with GNU make of your system.

When ./configure is executed, the command name of GNU make is
displayed as follows. Execute GNU make according to this
message.

  Execute GNU make in the current directory, as follows.

    /usr/bin/make

=end EN

=begin JA
=== ドキュメントの生成

マニュアルとコードリファレンスのコンパイルはカレントディレクトリ
において, 以下のコマンドを実行してください.
((<spml の TGZ パッケージ|URL:http://www.gfd-dennou.org/library/spml/spml.tar.gzz>))
から入手する場合には既に生成済みです.

        $ make doc
        $ make install


=end JA
=begin EN
=== Generate documentations

To generate documentations, execute the following command
in current directory. If you get from
((<Spml TGZ package|URL:http://www.gfd-dennou.org/library/spmodel/spml.tar.gz>)), 
documentations are already generated.

        $ make doc
        $ make install

=end EN

=begin JA
== インストールの手引き

カレントディレクトリで以下のコマンドを実行してください.
システム領域にインストールする場合には管理者権限が
必要です. (デフォルトの場合はシステム領域にインストールします).

        # make install


=end JA
=begin EN
== How to install

In current directory, execute following command.
If you install to system, you need to be administrator.
(By default, you install to system).

        # make install


=end EN


=begin JA
== テストプログラム実行の手順

カレントディレクトリにおいて, 以下のコマンドを実行してください.
エラーが生じずに
"(({ *** Compilation and installation are succeeded !! *** }))"
というメッセージが表示されればインストールは完了です.

        $ make test

=end JA
=begin EN
== Execute test programs

In current directry, execute following command.
If message "(({ *** Compilation and installation are succeeded !! *** }))"
are showed without error, installation is completed.

        $ make test

=end EN


=begin JA
== パスの設定

上記のように正しくインストールが行われたら,
((*spmfrt*)) というシェルスクリプトが (({--prefix=}))((|ARG|))
で指定されたディレクトリ以下の bin ディレクトリ ((|ARG|))/bin
に作成されているはずです.
( (({--prefix=})) を指定しなかった場合は
(({ /usr/local/spml/bin/ })) 以下).

このディレクトリへのパスを通してください.
以下は (({ /usr/local/spml/bin/ })) 以下に ((*spmfrt*))
がインストールされた場合の例です.

* sh, bash

        $ PATH=$PATH:/usr/local/spml/bin ; export PATH

* csh, tcsh

        $ setenv PATH $PATH:/usr/local/spml/bin

((*spmfrt*)) は spml ライブラリを利用した Fortran プログラムを
簡単にコンパイル, リンクするためのシェルスクリプトです.
これまで利用していた Fortran コンパイラのコマンドの代わりに
spmfrt を用いることで, 自動的に spml ライブラリへの
リンク, モジュール群へのディレクトリ指定を行ってくれます.

        $ spmfrt test.f90

        /usr/bin/g95 -I/usr/local/spml/include \
                     -I/usr/local/gtool5/include -O test.f90 \
                     -L/usr/local/gtool5/lib \
                     -L/usr/local/netcdf/lib \
                     -L/usr/local/ispack/lib \
                     -L/usr/local/spml/lib \
                     -lgtool5 -lnetcdf -lisp -lspml


=end JA
=begin EN
== Set PATH

If the installation is correctly done as stated above,
shell script ((*spmfrt*)) is made under the directory
(({--prefix=}))((|ARG|))/bin
(By default, (({ /usr/local/spml/bin/ })) ).

Please specify PATH to this directory.
It is an example as follows when ((*spmfrt*)) is installed in
(({/usr/local/spml/bin/})) .

* sh, bash

        $ PATH=$PATH:/usr/local/spml/bin ; export PATH

* csh, tcsh

        $ setenv PATH $PATH:/usr/local/spml/bin

((*spmfrt*)) is a shell script in order to easily compile and link
Fortran programs which utilizes the spml library.
Link to the gtool5 library and directory appointment to the modules
are done automatically by using spmfrt in place of command of the
Fortran compiler.

        $ spmfrt test.f90

        /usr/bin/g95 -I/usr/local/spml/include \
                     -I/usr/local/gtool5/include -O test.f90 \
                     -L/usr/local/gtool5/lib \
                     -L/usr/local/netcdf/lib \
                     -L/usr/local/ispack/lib \
                     -L/usr/local/spml/lib \
                     -lgtool5 -lnetcdf -lisp -lspml

=end EN

#== Mode setting for Emacs
#Local Variables:
#mode: rd
#End:
#
