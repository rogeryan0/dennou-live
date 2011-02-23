=begin

# * Shin-Ichi Takehiro, Youhei SASAKI, Masatsugu Odaka
#   * $Id: INSTALL.rd,v 1.3 2009-07-06 09:03:21 uwabami Exp $
# * 2009/03/01(��������ʿ) INSTALL �ɥ�����Ȥ�����
# * 2009/03/01(��������ʿ) ������
# * 2009/02/25(��������ʿ) RD ��
#   * ���ι�������
#      * 2007/12/04(�ݹ�����)
#      * 2005/02/01(�ݹ�����)
#      * 2005/01/10(�ݹ�����)
#      * 2004/01/27(��������)
#      * 2004/01/26(��������)
#      * 2002/05/20(�ݹ�����)
=end

=begin JA
= spml ���󥹥ȡ��륬����
=end JA

=begin EN
= Spml  Installation Guide
=end EN

=begin JA
== ư��Ķ�
spml �饤�֥��ϰʲ��δĶ��Ǥ�ư����ǧ���Ƥ��ޤ�.
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

���ˤϰʲ��δĶ��Ǥ�ư������Ȥ���ǧ����Ƥ��ޤ�.
��ǧ�Ϥ��Ƥ��ޤ���, ���ߤΥС������Ǥ⤪���餯ư���ȴ��Ԥ���ޤ�.

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
== ���󥹥ȡ����Ȥγ�ά

spml �饤�֥��Υ��󥹥ȡ���ϰʲ����ͤ˹Ԥʤ��ޤ�.
�ܤ����ϳƹ��ܤ򻲾Ȥ��Ʋ�����.

  (1) ((<ɬ�פʥ��եȥ�����>)) �򥤥󥹥ȡ��뤷�ޤ�.
  (2) ((<�ӥ�ɤμ����>)) �˽���, ����������饤�֥���ӥ�ɤ��ޤ�.
  (3) ((<���󥹥ȡ���μ����>)) �˽���, �饤�֥��򥤥󥹥ȡ��뤷�ޤ�.
  (4) ((<�ƥ��ȥץ����¹Ԥμ��>)) �˽���, ���󥹥ȡ��뤵�줿
      �饤�֥�꤬����˵�ǽ���뤫�ɤ�����ǧ���Ƥ�������.
  (5) ((<�ѥ�������>)) ��ԤäƤ�������.

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
== ɬ�פʥ��եȥ�����

spml �����Ѥ��뤿��ˤ�, �ʲ��Υ��եȥ�������
�����˥��󥹥ȡ��뤷�Ƥ���ɬ�פ�����ޤ�.

* ((<netCDF|URL:http://www.gfd-dennou.org/library/netcdf>)), version 3.6 �ʾ�
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
== �ӥ�ɤμ����
=end JA

=begin EN
== How to build
=end EN

=begin JA
=== TGZ �ѥå�������Ÿ��

Ŭ���ʺ�ȥǥ��쥯�ȥ�ǥ��������������֤�Ÿ�����ޤ�.
�������� spml-((|�С������|)) �Ȥ����ǥ��쥯�ȥ��Ÿ������ޤ�.

        $ tar xvzf spml_current.tgz

�ޤ���

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
=== Fortran ����ѥ���λ���

�Ķ��ѿ� ((* FC *)) �˻��Ѥ��� Fortran ����ѥ������ꤷ�Ƥ�������.
�ʲ���, ���Ѥ��륳��ѥ��餬 frt �ξ��Ǥ�.

* sh, bash �ξ��

        $ FC=frt ; export FC

* csh, tcsh �ξ��

        $ setenv FC frt

��Ŭ����ǥХå��Τ���Υ��ץ����ϴĶ��ѿ� ((* SYSFFLAGS *))
�����ꤷ�Ƥ�������. �ʲ������ Fujitsu Fortran ����Ѥ�����ι�®����
���󲽤Τ���Υ��ץ����Ǥ�.

* sh, bash �ξ��

        $ SYSFFLAGS="-Kfast,parallel" ; export FFLAGS

* csh, tcsh �ξ��

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
=== Config.mk �κ���

Ÿ�����줿�ǥ��쥯�ȥ�˰�ư��, (({ ./configure }))��¹Ԥ��ޤ�. 
* (({ --with-netcdf= })) �ˤ� netCDF �饤�֥��Υѥ�����ꤷ�ޤ�. 
  (�ʲ������ /usr/local/netcdf/lib/libnetcdf.a �˥饤�֥�꤬���� 
  ���Τ�ΤǤ�). 
* (({ --with-gtool5= })) �ˤ� gtool5 �饤�֥��Υѥ�����ꤷ�ޤ�. 
  (�ʲ������ /usr/local/gtool5/lib/libgtool5.a �˥饤�֥�꤬���� 
  ���Τ�ΤǤ�). 
* (({ --with-ispack= })) �ˤ� ispack �饤�֥��Υѥ�����ꤷ�ޤ�. 
  (�ʲ������ /usr/local/ispack/lib/libisp.a �˥饤�֥�꤬���� 
  ���Τ�ΤǤ�). 

���Υ��ޥ�ɤˤ�ä� (({ Config.mk })) �ե����뤬��������ޤ�. 
netCDF �饤�֥�꤬��ͭ�饤�֥��Ǥ�����, 
(({ --with-netcdff= })) ����ꤹ��ɬ�פ����뤫�⤷��ޤ���. 
�ܤ����ϲ����Υ��ץ����ξܺ٤򻲾Ȥ��Ƥ�������. 

        $ ./configure --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a \
                      --with-gtool5=/usr/local/gtool5/lib/libgtool5.a \
                      --with-ispack=/usr/local/ispack/lib/libisp.a 

spml �� MPI �Ѥ˥ӥ�ɤ�����ˤ�,
((<MPI �Ѥ˥ӥ�ɤ�����ˤ�>)) �򻲾Ȥ��Ƥ�������. 

�ӥ�ɤˤ� GNU make ��ɬ�פȤʤ뤿��, configure �� PATH �⤫��
��ư�� GNU make ��õ�����褦�Ȥ��ޤ���, �⤷���Ĥ���ʤ����,
���顼���֤��ޤ�. ���ξ��ˤϴĶ��ѿ� ((* MAKE *)) ��
GNU make ���ޥ�ɤ���ꤷ�ƺ��� configure ��¹Ԥ��Ƥ�������.

���󥹥ȡ�����ʤɤ��ѹ�����������, �ʲ��Τ褦�� (({ --help })) ����
������Ĥ��뤳�Ȥ�, �����ǽ�ʥ��ץ����ꥹ�Ȥ�ɽ������ޤ�.

        $ ./configure --help

��ʥ��ץ����˴ؤ��Ƥ������Ǥ�.

:(({--with-netcdf=}))((|ARG|))
  ((|ARG|)) �� ((<�ӥ�ɤ�ɬ�פ� netCDF �饤�֥��>)) 
  ����ꤷ�ޤ�. ɬ������Ū�˻��ꤹ��ɬ�פ�����ޤ�.

:(({--with-netcdff=}))((|ARG|))
  netCDF �饤�֥�꤬��ͭ�饤�֥��Ǥ�����, C �ѥ饤�֥���
  Fortran �ѥ饤�֥��Ȥ�ʬ����ƥӥ�ɤ���Ƥ����礬����ޤ�.
  ���κݤ�, �嵭���ץ����� C �ѥ饤�֥�����ꤷ, �ܥ��ץ�����
  ((|ARG|)) �� ((<Fortran �ѥ饤�֥��>)) ����ꤷ�ޤ�.

:(({--with-netcdf-include=}))((|ARG|))
  ɬ�פʤ�� netCDF �饤�֥��� Fortran �ѥإå�(netcdf.inc) ����ꤷ�ޤ�. 

:(({--with-gtool5=}))((|ARG|))
  ((|ARG|)) �� ((<�ӥ�ɤ�ɬ�פ� gtool5 �饤�֥��>)) 
  ����ꤷ�ޤ�. ɬ������Ū�˻��ꤹ��ɬ�פ�����ޤ�.

:(({--with-ispack=}))((|ARG|))
  ((|ARG|)) �� ((<�ӥ�ɤ�ɬ�פ� ispack �饤�֥��>)) 
  ����ꤷ�ޤ�. ɬ������Ū�˻��ꤹ��ɬ�פ�����ޤ�.

:(({--with-ssl2=}))((|ARG|))
:(({--with-ssl2tp=}))((|ARG|))
:(({--with-lapack=}))((|ARG|))
  ((|ARG|)) �� ((<��ͭ�������򤯤���˻��Ѥ���饤�֥��>))
  ����ꤷ�ޤ�. 

:(({--with-fftw=}))((|ARG|))
:(({--with-rfftw=}))((|ARG|))
  �ա��ꥨ�Ѵ��� FFTW ver.2 ����Ѥ�����˻��ꤷ�ޤ�.
  ((<ispack>)) �� -FFTW2 �դ��ǥӥ�ɤ��Ƥ�����ˤϻ��ꤹ��ɬ�פ�����ޤ�.

:(({--with-mpifc=}))((|ARG|))
  MPI �����ͭ���ˤ�����˻��ꤷ�ޤ�. 

:(({--prefix=}))((|ARG|))
  ((|ARG|)) �˥饤�֥���⥸�塼��, �¹ԥե�����Υ��󥹥ȡ������
  �ǥ��쥯�ȥ�Υץ�ե��å�������ꤷ�ޤ�.
  �ǥե���Ȥ� (({ /usr/local/spml })) �Ǥ�.

:(({--host=}))((|ARG|))
  ��������ѥ����Ԥ����ˤ�, �ѥå��������¹Ԥ���륷���ƥॿ����̾
  �� ((|ARG|)) �˻��ꤷ�ޤ�.

:(({--libdir=}))((|ARG|))
  ((|ARG|)) �˥饤�֥��Υ��󥹥ȡ�����Υǥ��쥯�ȥ����ꤷ�ޤ�.
  �ǥե���Ȥ� (({ /usr/local/spml/lib })) �Ǥ�.

:(({--includedir=}))((|ARG|))
  ((|ARG|)) �˥⥸�塼�����ե�����Υ��󥹥ȡ�����Υǥ��쥯�ȥ�
  ����ꤷ�ޤ�. �ǥե���Ȥ� (({ /usr/local/spml/include })) �Ǥ�.

:(({--bindir=}))((|ARG|))
  ((|ARG|)) �˼¹ԥե�����Υ��󥹥ȡ�����Υǥ��쥯�ȥ����ꤷ�ޤ�.
  �ǥե���Ȥ� (({ /usr/local/spml/bin })) �Ǥ�.

:(({--docdir=}))((|ARG|))
  ((|ARG|)) �˥ɥ������/�ޥ˥奢��Υ��󥹥ȡ�����Υǥ��쥯�ȥ����ꤷ�ޤ�.
  �ǥե���Ȥ� (({ /usr/local/spml/doc })) �Ǥ�.

:(({--with-abort=}))((|ARG|))
  ((|ARG|)) �� (({abort, errtra-setrcd, exit, setrcd, stop})) �Τ����줫
  ����ꤹ�뤳�Ȥ�, ��λ�����Ѥ��� Fortran �������ؿ����ѹ����뤳�Ȥ�
  ��ǽ�Ǥ�. �ǥե���Ȥ� (({ abort })) �Ǥ�.

  : abort
    Fortran ���������֥롼���� abort �ǽ�λ���ޤ�.

  : errtra-setrcd
    Fujitsu Fortran �� ERRTRA �����ӥ����֥롼�����ƤӽФ�,
    ���߼¹���Υץ����ñ�̤ޤǤΥȥ졼���Хå��ޥåפ���Ϥ�,
    ��λ���ޤ�. Fujitsu Fortran �� SETRCD �����ӥ����֥롼����ˤ�,
    ���������� 13 �����ꤷ, ��λ�����ɤȤ��� 3 �����ꤷ�ޤ�.

  : exit
    Fortran ���������֥롼���� exit �ǽ�λ���ޤ�.

  : setrcd
    Fujitsu Fortran �� SETRCD �����ӥ����֥롼�����ƤӽФ�,
    Fortran �����������ɤȤ��� 3 �����ꤷ�ƽ�λ���ޤ�.

  : stop
    Fortran �������ؿ� stop �ǽ�λ���ޤ�.

:(({--config-cache})) �ޤ��� (({-C}))

  (({ Config.mk })) �ե����뤬����������Ʊ����, (({config.cache}))
  �ե����뤬��������, (({ ./configure })) �ΰ����˻��ꤵ�줿�饤�֥��ΰ���
  �ʤɤξ����ݻ�����ޤ�.

  ���� (({ ./configure })) ��¹Ԥ���ݤˤ⤳�Υ��ץ�������ꤹ�뤳�Ȥ�,
  (({config.cache})) ���ɤ߹��ޤ�, ������ꤷ�����ץ�������������Ѥ���ޤ�.
  ����¸�ߤ��� (({config.cache})) ��̵�뤹����Ϥ��Υ��ץ�����
  ���ꤻ���� (({ ./configure })) ��¹Ԥ��Ƥ�������.

  �㤨�в����Τ褦�� (({ ./configure })) ��¹Ԥ���Ȥ��ޤ�.

      $ ./configure -C --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a
                       --with-gtool5=/usr/local/gtool5/lib/libgtool5.a
                       --with-ispack=/usr/local/ispack/lib/libisp.a

  ����ȼ���ʹ�, �����Τ褦�� (({ ./configure })) ��¹Ԥ��뤳�Ȥ�
  �饤�֥��ΰ��־���ʤɤ������Ѥ���ޤ�.

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
=== MPI �Ѥ˥ӥ�ɤ�����ˤ�

spml �� MPI �Ѥ˥ӥ�ɤ�����ˤϤޤ� MPI �饤�֥��򥷥��ƥ��
���󥹥ȡ��뤷�Ƥ�������.

���� gtool5 �� MPI ���ݡ��Ȥ�ͭ���ˤ��� install ���ޤ�. 
�ܺ٤ˤĤ��Ƥ�
((<gtool5 �� install �ɥ������|URL:http://www.gfd-dennou.org/library/gtool5/INSTALL.htm>))�򻲾Ȥ��Ʋ�����.

Config.mk �����������ˤ�, �Ķ��ѿ� FC �� mpif90 �ʤɤ� MPI �ѥ���ѥ���
���ޥ�ɤ���ꤷ�Ƥ�������.
������, �ʲ��Τ褦�� configure �ˤϥ��ץ���� --with-mpifc ����ꤷ�Ʋ�����

    $ ./configure -C --with-netcdf=/usr/local/netcdf/lib/libnetcdf.a \
                     --with-gtool5=/usr/local/gtool5/lib/libgtool5.a \
                     --with-ispack=/usr/local/ispack/lib/libisp.a \
                     --with-mpifc=/usr/local/mpich/mpif90

����¾�Υ��ץ����ʤɤˤĤ��Ƥ�, ((<Config.mk �κ���>)) �򻲾Ȥ��Ƥ�������. 

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
=== Config.mk ���Խ�

�Ķ��˹�碌 (({ Config.mk })) ���ư���Խ����Ƥ�������. 
����������ˤĤ��Ƥ褯ʬ����ʤ����ˤϤȤꤢ����
((<�����������ɤΥ���ѥ���>)) �ؿʤ�Ǥ�������. 

   FC          : Fortran ����ѥ���

   SYSFFLAGS   : ����ѥ��������󥯻���ɬ�פʥե饰
                 (Fortran ����ѥ���¹Ի���ɬ�פʥ��ץ����)

   SYSLDFLAGS  : ��󥯻���ɬ�פʥե饰

   SYSLDLIBS   : ��󥯻���ɬ�פʥ饤�֥��

   F90MODTYPE  : �⥸�塼�������Ϥ���

   DEST_LIB    : spml �饤�֥�ꥤ�󥹥ȡ���ǥ��쥯�ȥ�

   DEST_INC    : spml �⥸�塼�륤�󥹥ȡ���ǥ��쥯�ȥ�

   DEST_BIN    : spml �¹ԥե����륤�󥹥ȡ���ǥ��쥯�ȥ�

   DEST_DOC    : spml �ɥ�����ȥե����륤�󥹥ȡ���ǥ��쥯�ȥ�

   MODS        : �⥸�塼��ե������ĥ��

   MAKE        : GNU make ���ޥ��

   AR          : ���������֥��ޥ��

   ARFLAGS     : ���������ֻ���ɬ�פʥե饰

   RANLIB      : ���������֤Υ���ǥå�����������륳�ޥ��

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
=== �����������ɤΥ���ѥ���

�ӥ�ɤˤ�ɬ�� GNU make ����Ѥ��Ƥ�������. ¾�� "make" �ץ������
�Ѥ����, �������ӥ�ɤ��Ԥ��ޤ���.
�ʹ� GNU make �Υ��ޥ��̾�� "make" ��ɽ�����ޤ���,
�����ϥ����ƥ�� GNU make ���ޥ�ɤ�̾�����֤������Ƥ�������.

./configure ��¹Ԥ����, �ʲ��Τ褦�� GNU make �Υ��ޥ��̾��
ɽ������ޤ�. ���Υ�å������˽��ä� GNU make ��¹Ԥ��Ƥ�������.

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
=== �ɥ�����Ȥ�����

�ޥ˥奢��ȥ����ɥ�ե���󥹤Υ���ѥ���ϥ����ȥǥ��쥯�ȥ�
�ˤ�����, �ʲ��Υ��ޥ�ɤ�¹Ԥ��Ƥ�������.
((<spml �� TGZ �ѥå�����|URL:http://www.gfd-dennou.org/library/spml/spml.tar.gzz>))
�������ꤹ����ˤϴ��������ѤߤǤ�.

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
== ���󥹥ȡ���μ����

�����ȥǥ��쥯�ȥ�ǰʲ��Υ��ޥ�ɤ�¹Ԥ��Ƥ�������.
�����ƥ��ΰ�˥��󥹥ȡ��뤹����ˤϴ����Ը��¤�
ɬ�פǤ�. (�ǥե���Ȥξ��ϥ����ƥ��ΰ�˥��󥹥ȡ��뤷�ޤ�).

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
== �ƥ��ȥץ����¹Ԥμ��

�����ȥǥ��쥯�ȥ�ˤ�����, �ʲ��Υ��ޥ�ɤ�¹Ԥ��Ƥ�������.
���顼����������
"(({ *** Compilation and installation are succeeded !! *** }))"
�Ȥ�����å�������ɽ�������Х��󥹥ȡ���ϴ�λ�Ǥ�.

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
== �ѥ�������

�嵭�Τ褦�����������󥹥ȡ��뤬�Ԥ�줿��,
((*spmfrt*)) �Ȥ��������륹����ץȤ� (({--prefix=}))((|ARG|))
�ǻ��ꤵ�줿�ǥ��쥯�ȥ�ʲ��� bin �ǥ��쥯�ȥ� ((|ARG|))/bin
�˺�������Ƥ���Ϥ��Ǥ�.
( (({--prefix=})) ����ꤷ�ʤ��ä�����
(({ /usr/local/spml/bin/ })) �ʲ�).

���Υǥ��쥯�ȥ�ؤΥѥ����̤��Ƥ�������.
�ʲ��� (({ /usr/local/spml/bin/ })) �ʲ��� ((*spmfrt*))
�����󥹥ȡ��뤵�줿������Ǥ�.

* sh, bash

        $ PATH=$PATH:/usr/local/spml/bin ; export PATH

* csh, tcsh

        $ setenv PATH $PATH:/usr/local/spml/bin

((*spmfrt*)) �� spml �饤�֥������Ѥ��� Fortran �ץ�����
��ñ�˥���ѥ���, ��󥯤��뤿��Υ����륹����ץȤǤ�.
����ޤ����Ѥ��Ƥ��� Fortran ����ѥ���Υ��ޥ�ɤ������
spmfrt ���Ѥ��뤳�Ȥ�, ��ưŪ�� spml �饤�֥��ؤ�
���, �⥸�塼�뷲�ؤΥǥ��쥯�ȥ�����ԤäƤ���ޤ�.

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
