= Mac OS X �ؤ���ǾRuby��Ϣ�񸻤Υ��󥹥ȡ�����

��¼ ʹ��

2006/03

== �Ϥ����

�ܹƤǤ�Ruby����DCL��Ϥ���Ȥ���饤�֥�귲��Mac OS X������Ѥ��뤿
��μ���ޤȤ�롥�����ɬ�׺Ǿ��¤δĶ������ǹ��ۤ��뤿��κ�û��
Υ���󶡤��뤳�Ȥ���Ū�Ȥ��Ƥ��뤿�ᡤ�㤨��DCL��GTK���Ѥ���ʤɤ�����
�ˤĤ��ƤϽ������Ƥ��롥���Υ��󥹥ȡ�����ˤ���������Ȥ��Ƥ���Ķ�
�ϰʲ����̤�Ǥ��롥

* Mac OS X (PowerPC�ǡ�Intel�ץ�åȥե�����ˤĤ��Ƥ�̤����)
* gcc4
* X11

gcc4�ˤĤ��Ƥϡ���ȯ�Ķ�(Xcode)�����ӥ��󥹥ȡ��뤹��ɬ�פ����롥�Ƕ�
��Mac�ˤϤϤ��ᤫ�饤�󥹥ȡ���ǥ��������Ѱդ���Ƥ���褦�������긵
�ˤʤ����Ǥ�Apple�Υ����Ȥ���ǿ��Ǥ����������ɤ��뤳�Ȥ���ǽ�Ǥ�
��(��Developer��Ͽ)��X11�ˤĤ��Ƥ�OS��Ƴ���κݤ˥������।�󥹥ȡ��뤬
ɬ�פȤʤ뤫�⤷��ʤ��Τǡ��������ˤĤ��Ƥ����դ�ɬ�פǤ��롥

ɮ�Ԥ�Mac OS X 10.4.5 + gcc4 (Xcode 2.2.1)�δĶ��Ǻ���¤�ư���ǧ��
�Ԥä�����������Ķ���ư����ݾڤ����ΤǤϤʤ����Ȥ����ա�

== ����

=== ruby-1.8.4

������:
* http://www.ruby-lang.org/ja/
* ftp://ftp.gnu.org/gnu/readline/readline-5.1.tar.gz

Mac OS X�ˤϥǥե���Ȥ�Ruby�����󥹥ȡ��뤵��Ƥ��뤬��readline�����
������Ƥ��ʤ��Τ��ä�irb��Ȥ��Ȥ������ؤǤ��롥�����Ruby�⼫������
��뤳�Ȥˤ�����

�ޤ���readline-5.1�Υ��󥹥ȡ��뤫��Ԥ���

 % tar xzvf readline-5.1.tar.gz
 % cd readline-5.1
 % ./configure --prefix=/usr/local
 % make
 % sudo make install

����Ruby�Υ��󥹥ȡ��롥

 % tar xzvf ruby-1.8.4.tar.gz 
 % cd ruby-1.8.4
 % ./configure --prefix=/usr/local --with-readline-dir=/usr/local
 % make
 % sudo make install

���ͥ�����:
* http://d.hatena.ne.jp/tsimo/20060212
* http://hivelogic.com/articles/2005/12/01/ruby_rails_lighttpd_mysql_tiger

=== DCL-5.3.1-C

������:
* http://www.gfd-dennou.org/arch/dcl/

make�������˰ʲ��Τ褦�˥������ν�����ɬ�פǤ��롥

src/grph1/swpack/zxpack.c���
 #include <stdlib.h>
�򥳥��ȥ����Ȥ��롥

���Τޤޤ���src/grph1/swpack/zxpack.c��wait��¿����������Ȥ������顼
�ˤʤ롥����ϡ�/usr/include/libstd.h����/sys/wait.h��Ƥ�Ǥ��뤿���
�仡����롥�����������κ�Ȥ�Mac OS X 10.3�����ˤ����פ��ä����������롥

�ʾ�ν�����Ԥä��Τ���

 % env CFLAGS='-O' ./configure --prefix=/usr/local/dcl-5.3.1-C
 % make
 % sudo make install

make install�Τ��Ȥ�

 % sudo ranlib /usr/local/dcl-5.3.1-C/lib/libcdcl53.a
 % sudo ranlib /usr/local/dcl-5.3.1-C/lib/libf2c4dcl.a

��ɬ�פȤʤ롥

=== netCDF-3.6.1

������:
* http://www.unidata.ucar.edu/software/netcdf/

�����Ruby-NetCDF����ƤӽФ����Ȥ��������ꤷ��C�Υ饤�֥��ʳ��Ϻ��
�ʤ����ˤȤ��롥Fortran�ѤΥ饤�֥������Ѥ��뤿�������ϡ��Ѥ���
Fortran����ѥ���˶�����¸���뤿�ᡤ�����ǤϿ���ʤ����Ȥˤ��롥

 % env CC=gcc CXX='' FC='' F90='' ./configure --prefix=/usr/local
 % make
 % make check
 % sudo make install

== Ruby�Υ饤�֥�귲�Υ��󥹥ȡ���

��˽Ҥ٤��������Ǥ��Ƥ���С�Ruby�Υ饤�֥��򥤥󥹥ȡ��뤹��ΤϤ�
��ۤ��񤷤��ʤ���Linux����Ʊ�ͤμ��Ǵ���Ū��OK�Ǥ��롥

=== NArray-0.5.8

������:
* http://narray.rubyforge.org/index.html.ja

 % ruby extconf.rb
 % make
 % sudo make install

=== ruby-dcl-1.5.2

������(�ʲ��ˤĤ��Ƥ�Ʊ��):
* http://ruby.gfd-dennou.org/products.htm

 % ruby extconf.rb
 % make
 % sudo make install

=== ruby-netcdf-0.6.2

 % ruby extconf.rb
 % make
 % make test
 % sudo make install

=== NArrayMiss-1.1.2

 % ruby setup.rb config
 % sudo ruby setup.rb install

=== numru-units-1.5

 % sudo ruby install.rb

=== numru-misc-0.0.6

 % sudo ruby install.rb

=== gphys-0.5.1

 % sudo ruby install.rb


== [��Ͽ] GTK�б��ˤ�����Υ��

��Ǿ�饤�֥���GTK�б��ˤ���ˤϡ�GTK+2��Ϣ�Υ饤�֥��򤢤餫����Ƴ
�����Ƥ���ɬ�פ����롥�����Υ饤�֥��򤹤٤Ƽ��Ϥǥ���ѥ��뤹���
�Ϥ��ʤ�μ�֤ʤΤǡ������Ǥϡ�Fink�Ȥ����ѥå����������ƥ�����Ѥ���
���Ȥˤ��롥�¤Ͼ嵭�ˤ����Ƽ��Ϥ�Ƴ�������饤�֥��Τ����Ĥ���Fink��
�ȤäƤ�Ƴ����ǽ�ʤΤ����������GTK+2��Ϣ�Υ��󥹥ȡ���Τߤ�Fink���
�����ˤȤ��������������ܥ��󥹥ȡ������ɮ�Ԥ��ºݤ˹Ԥä���Ȥ��
α�᤿�����ʤΤǡ��ºݤˤ����פʺ�Ȥ�ʶ�����Ǥ����ǽ���⤢�뤳�Ȥ�
���դ��Ƥ�������������

=== Fink�Υ��󥹥ȡ���

* http://fink.sourceforge.net/download/index.php?phpLang=ja
���Fink 0.8.0�Х��ʥꥤ�󥹥ȡ�������������ɡ�
Fink�Υ��󥹥ȡ���ˤĤ��ƤϾ嵭�����Ȥ򻲾ȤΤ��ȡ�
�ǥե���ȤǤ�unstable�ѥå������򻲾Ȥ��ʤ��Τǡ�������٤��ѹ���Ԥ���
����Ū�ˤϡ�/sw/etc/fink.conf��Trees:��unstable/crypto,unstable/main��ä���:
 Trees: local/main stable/main stable/crypto unstable/crypto unstable/main
/sw/etc/apt/source.list��
 deb file:/sw/fink unstable main crypto
���ɲá��������ѹ���Ԥä��顤

 % fink scanpackages; fink index
 % sudo apt-get update

��¹ԡ�

���ξ�ǡ�GTK+2��Ϣ�Υ饤�֥��򥤥󥹥ȡ��뤹�롥�����Ǥϻ��������
����Х��ʥꥤ�󥹥ȡ�����ߤ롥ɬ�פʥѥå������ϡ�

* gtk+2
* gtk+2-dev
* pango1-xft2-dev
* glib2-dev
* atk1

�Ǥ��롥�ʲ��Τ褦�ˤ��ƥѥå�������缡���󥹥ȡ��뤹�롥

 % sudo apt-get install package-name

=== Ruby-GTK2
* http://ruby-gnome2.sourceforge.jp/ja/index.html
���ruby-gtk2-0.14.1.tar.gz�����ꡥ

 % ruby extconf.rb
 % make
 % sudo make install

=== DCL-5.3.1-C

GTK���б��Ǥ�Ʊ������OK.

=== ruby-dcl-1.5.2

lib/dcl.rb��2����
 if /cygwin|mingw/ =~ RUBY_PLATFORM
��
 if /cygwin|mingw|darwin/ =~ RUBY_PLATFORM
�Ȥ��Ƥ��顤

 % ruby extconf.rb
 % make
 % sudo make install

=== gave-1.2.1

 % ruby setup.rb config
 % sudo ruby setup.rb install