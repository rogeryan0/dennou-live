=begin

= ��ǾRuby������ ��祤�󥹥ȡ���

Last modified: <2007-04-27 06:05:31 koshiro>

��ǾRuby�����ʤ�ư�ǥ��󥹥ȡ��뤹��Ruby������ץȤǤ���
GAVE��Ϣ�Τ��٤Ƥ���ǾRuby�����ʤ��б����Ƥ��ޤ�(Ruby-SSL2, Ruby/Msise90 �ˤϸ��ߤΤȤ������б��Ǥ�)��

=end
=begin html
<font color="red">�����Υ��եȥ������ϸ��ߥ��ƥʥ󥹤���Ƥ��ޤ���!</font>
=end
=begin

== ���������

((<numru-install.rb|URL:http://ruby.gfd-dennou.org/products/installer/numru-install.rb>))
(Ver. 3.4.7)

== ư��Ķ�

URI�⥸�塼�뤬ɸ��ź�դ���Ƥ��� Ruby 1.6.7 �ʾ夬ɬ�פǤ���

UNIX��OS(Linux, FreeBSD, Solaris, Windows���Cygwin, �ʤɤʤ�)�Ǥ���ФɤΥץ�åȥե�����Ǥ�ư���Ȼפ��ޤ�����GNU gzip �� GNU patch ��ɬ�פˤʤ�ޤ���

������Ǥϰʲ��δĶ���ư����ǧ���Ƥ��ޤ���

* Vine Linux 2.6 + Ruby 1.6.7
* Vine Linux 3.1 + Ruby 1.8.2
* Debian GNU/Linux 3.1 + Ruby 1.8.2
* Cygwin(on Windows XP Professional) + Ruby 1.8.4

�� numru-install.rb �Ǥϡ�GTK+��Ϣ�饤�֥���̵ͭ��ʲ��Τ褦�ˤ���
�����å����Ƥ��ޤ��Τ���դ��Ƥ���������

:GTK+1
  gtk-config, imlib-config �γƥ��ޥ�ɤη�̤�Ƚ�ꤷ�Ƥ��ޤ�(C��DCL
  �� configure ��Ʊ�ͤΥ����å��򤷤Ƥ��ޤ�)���ޤ���Ruby-GNOME �򥤥�
  ���ȡ��뤹��ݤˤ� gdk-pixbuf-config �ˤ������å���Ԥ��ޤ���
  which���ޥ�����ǡ������Υ��ޥ�ɤ����뤫�ɤ�����ǧ���Ƥ���������
  ���Ȥ�GTK+1�Υ饤�֥�꤬���äƤ⡤�����Υ��ޥ�ɤ��ʤ�����������
  �󥹥ȡ��뤵��ޤ��󡥥Х��ʥ�ѥå��������ۤ�Linux�ǥ����ȥ�ӥ塼
  �����򤪻Ȥ��ξ�����դ�ɬ�פǤ����㤨�� Red Hat Linux 9 �Ǥϡ�
  �����Υ��ޥ�ɤϳ�ȯ�ġ���ѥå�����
  ({gtk+|gdk-pixbuf|imlib}-devel-x.x.x.i386.rpm)�Τۤ��˴ޤޤ�Ƥ��ꡤ
  �ǥե���ȤǤϥ��󥹥ȡ��뤵��Ƥ��ʤ���礬����ޤ������󥹥ȡ���
  CD-ROM��FTP�����������餳���ѥå���������������󥹥ȡ��뤷�Ƥ���
  ������GTK+1�饤�֥��򥽡��������ɤ��饳��ѥ��뤷�ƥ��󥹥ȡ��뤷
  �Ƥ����������ʤ��Ȼפ��ޤ���

:GTK+2
  pkg-config ���ޥ�ɤη�̤�Ƚ�ꤷ�Ƥ��ޤ�(C��DCL �� configure ��Ʊ��
  �Υ����å��򤷤Ƥ��ޤ�)��which���ޥ�����ǡ������Υ��ޥ�ɤ����뤫
  �ɤ�����ǧ���Ƥ����������Х��ʥ�ѥå��������ۤ�Linux�ǥ����ȥ�ӥ塼
  �����򤪻Ȥ��ξ�硤���Ȥ�GTK+2�Υ饤�֥�꤬���äƤ⡤pkg-config
  �ѤΥ饤�֥�����ե����뤬�ʤ���礬����ޤ��Τ���դ��Ƥ���������
  �㤨�� Vine Linux 3.1 �Ǥϡ�gtk2-devel-x.x.x.i386.rpm �򥤥󥹥ȡ���
  ����ɬ�פ�����ޤ������󥹥ȡ���CD-ROM��FTP������������ѥå�������
  ���������󥹥ȡ��뤷�Ƥ���������GTK+2�饤�֥��򥽡��������ɤ��饳
  ��ѥ��뤷�ƥ��󥹥ȡ��뤷�Ƥ����������ʤ��Ȼפ��ޤ����ޤ���
  pkg-config ���饤�֥�����򻲾Ȥ���ǥ��쥯�ȥ�ϡ��Ķ��ѿ�
  PKG_CONFIG_PATH �Ƿ�ޤäƤ��ޤ����̾�Ͽ��ۤʤ��Ȼפ��ޤ�����Cygwin
  �ξ��ʤɤ���դ��Ƥ���������

== �Ȥ���

((%$ ruby numru-install.rb%))

�Ȥ��Ƽ¹Ԥ��ޤ������Ȥϼ���������Ƥ��������Ǥ���

���ߥ��󥹥ȡ��뤵��Ƥ���Ruby�饤�֥��ΥС�����������ݻ�����ꥹ�ȥե��������ꤹ��(�Ϥ���ƻȤ��Ȥ��˺��ޤ�)�ȡ��ǿ��Υѥå������ΥС�������õ���ˤ����ޤ���
��������ȡ����󥹥ȡ���Ǥ���ѥå������Υꥹ�Ȥ�ɽ������ޤ��Τǡ��ѥå����������򤷤ơ���������ɤ���񸻤��֤����ȥ��󥹥ȡ��������ꤹ���OK�Ǥ���

�ʤ���Firewall���������ץ�����ͳ�ǥ��󥿡��ͥåȤ���³���Ƥ���Ķ������ϡ���祤�󥹥ȡ����¹Ԥ������ˡ�

(sh��) ((%$ export http_proxy=http://your.proxy.server:8080/%))

(csh��) ((%% setenv http_proxy http://your.proxy.server:8080/%))

�ʤɤȤ��ƴĶ��ѿ� http_proxy �˥ץ��������Фȥݡ����ֹ�����ꤷ�Ƥ���������

�ޤ���Firewall������ȡ�passive mode �Ǥʤ��ȳ�����FTP�����Ф���³�Ǥ��ʤ���礬����ޤ�(ADSL�����򤪻Ȥ��ξ��ʤ�)�����ΤȤ��ϡ���祤�󥹥ȡ����¹Ԥ������ˡ�

(sh��) ((%$ export FTP_PASSIVE_MODE=YES%))


(csh��) ((%% setenv FTP_PASSIVE_MODE YES%))

�Ȥ��ƴĶ��ѿ� FTP_PASSIVE_MODE �����ꤷ�Ƥ���������

���󥹥ȡ��뤷����ä��顤�Ķ��ѿ� PATH �� RUBYLIB ���ǧ���뤳�Ȥ�˺�줺�ˡ�

== ��Ϣ����

=== Cygwin�����ѥå�

�����Υץ�����ȤǤϡ�Cygwin�Ķ��ǥ��󥹥ȡ�����Զ�礬�����ޤ���
numru-install.rb �Ǥϡ�������褹�뤿��˰ʲ��Υѥå��򤢤ƤƤ��ޤ���

* C��DCL(5.3.x) -- ((<dcl53c_cygwin.patch.gz|URL:http://ruby.gfd-dennou.org/products/installer/patches/dcl53c_cygwin.patch.gz>)) (Thanks to ��ƣ����)
  * dclcc (csh������ץ�)����Ƭ�Ԥ� ((%#!/bin/csh -f%)) ���� ((%#!/bin/tcsh -f%)) ���ѹ� (Cygwin�ˤ� csh ���ʤ�����)

* NetCDF(3.6.1) -- ((<netcdf361_cygwin.patch.gz|URL:http://ruby.gfd-dennou.org/products/installer/patches/netcdf361_cygwin.patch.gz>))
  * Makefile �� whatis �����˴ؤ�����ʬ�򥳥��ȥ����� (makewhatis ����λ���ơ����� 255 ���֤��Τ� make ���۾ｪλ���Ƥ��ޤ�����)

== �ѹ�����
:Feb 13, 2006 (3.4.7)
  * ɬ�פʤ�� netcdf361_cygwin.patch.gz ��Ŭ�Ѥ���褦�ˤ���

:Feb 8, 2006 (3.4.6)
  * NArray �Υ����Ȥ���ž�����Τ�ȼ����URL���ѹ�
  * ���󥹥ȡ�������å��ѤΥ᥽�å� installed? ���ѹ� (��� Ruby-FFTW3 �к�)

:Jun 27, 2005 (3.4.5)
  * �᥽�å� install �κǸ�� �᥽�å� installed? �ǥ��󥹥ȡ�������å���Ԥ��褦�ˤ���(��� Ruby-GNOME2 �к�)
  * �⥸�塼�� NumRu::Fetch::http_get_list �β���
  * getopts ��Ȥ�ʤ��褦�ˤ���
  * Ruby�饤�֥��Υ��󥹥ȡ�������å��κݤ� require �����ץ����ǹԤ��褦�ˤ���

:May 24, 2005 (3.4.4)
  * rubydcl-1.5.0 �Ѥ� Cygwin�ѥå�����

:Mar 17, 2005 (3.4.3)
  * ruby-gnome, ruby-gnome2 �Υ�������ɥ����Ȥ��ѹ���voxel.net -> JAIST

:Mar 3, 2005 (3.4.2)
  * Cygwin + Ruby 1.8.2 �δĶ��� narray ����ݡ��ȥ饤�֥��Υ��󥹥ȡ��뤬���Ԥ��뤳�Ȥ��б�

:Mar 3, 2005 (3.4.1)
  * netcdf �κǿ��С������ 3.6.0-p? �ξ�硤Cygwin �Ǥ� 3.6.1 beta release ��Ȥ��褦�ˤ���

:Mar 3, 2005 (3.4.0)
  * ��ǾRuby�����ʤΥС����������å��κ� FTP �����Фؤ���³��1��Ǥ���褦���ѹ�
  * fftw �� URL �� ���ꥸ�ʥ륵���Ȥ˺��ѹ�(���ܤΥߥ顼�����Ȥ˺ǿ��Ǥ��ʤ��ä�����)

:Mar 1, 2005 (3.3.3)
  * narray_miss �Υ��󥹥ȡ��륹����ץȤ��ѹ����줿���Ȥ��б�
  * �ѥå������κǿ��С�������������뤿�������ɽ�����ѹ�
  * fftw �� URL �� ���ܤΥߥ顼�����Ȥ��ѹ�

:Jan 15, 2005 (3.3.2)
  * gphys �Υ��󥹥ȡ��륹����ץȤ��ѹ����줿���Ȥ��б�

:Jan 6, 2005 (3.3.1)
  * ɬ�פʤ�� rubydcl150_cygwin.patch.gz ��Ŭ�Ѥ���褦�ˤ���
  * netcdf�ˤĤ��ơ�Linux�Ǥ�configure option, Cygwin�Ǥ� pacth file ���ѹ�����
  * netcdf �κǿ��С������ 3.6.0 �ξ�硤Cygwin �Ǥ� 3.6.1 beta release ��Ȥ��褦�ˤ���

:Dec 22, 2004 (3.3.0)
  * C��DCL��tarball̾��URL���ѹ����줿���Ȥ��б�
  * gave �Υ��󥹥ȡ��륹����ץȤ��ѹ����줿���Ȥ��б�

:Aug 12, 2004 (3.2.1)
  * �Х��ե��å�����

:Aug 12, 2004 (3.2.0)
  * class RbLib���ѹ���narray_miss �Υ��󥹥ȡ��륹����ץȤ��ѹ����줿���Ȥ��б�

:Aug 11, 2004 (3.1.0)
  * class RubySoft���ѹ���gave �Υ��󥹥ȡ��륹����ץȤ��ѹ����줿���Ȥ��б�
  * �ѥå�����̾���ѹ����б���misc -> numru-misc
  * �٤��ʥХ��ե��å�����

:Jul 15, 2004 (3.0.0)
  * ���󥹥ȡ���Ǥ���ѥå��������ɲá�fftw, ruby-fftw3, multibitnums, gpv, numru-units, ruby-{gnome|gnome2}-all, gave
  * module NumRu::Fetch::ftp_fetch, NumRu::Fetch::ftp_get_list ���ѹ����Ķ��ѿ� 'FTP_PASSIVE_MODE' �Υ����å����ɲ�
  * module NumRu::Install �����
    * GTK �ΥС����������å����ɲ�
    * class Installer ���ɲá����󥹥ȡ��鼫�ȤΥС����������å�����ӥ�������ɵ�ǽ��⤿����
    * class RubySoft �� method 'install' ���ɲ�
    * RbExtLib and RbLib �� superclass �� RubyLib ���ѹ�
    * class Software, RubySoft �� method 'current_ver' ���֤��ͤ��version + GTK version�פ��ѹ�
    * class Software �� method 'current_ver_disp' ���ɲ�
    * �ѥå������ΰ�¸�ط������å��λ��Ȥߤ������method 'get_dep_check_pkgs' ���ɲá�method 'check_depend' �β���
    * module function 'upgrade' ���ɲ�
  * �ѥå������ΰ�¸�ط�������ѹ�
    * rubydcl requires ruby-{gnome|gnome2}-all
    * gphys requires ruby-fftw3 and numru-units
  * 'gpatch' ���ޥ�ɤ�����С�'patch'������ˤ������Ȥ��褦�ˤ���
  * �Х��ե��å���.

:Dec 18, 2003 (2.0.1)
  * �Х��ե��å���(Thanks to ��߷����)
    * ���̥ե�����β��ॳ�ޥ�ɤ��ѹ���'zcat' -> 'gzip -dc'
    * Net::HTTP#get ��Ǥ��㳰�򽦤��褦�ˤ���(Ruby 1.6.7�ξ��Τ�)
  * ɬ�פʾ��� netcdf350_cxx_gcc3.patch ��Ŭ�Ѥ����褦�ˤ���

:Dec 10, 2003 (2.0.0)
  * ���󥹥ȡ���Ǥ���ѥå��������ɲá�misc, met, gphys
  * module NumRu::Install �������˲���
    * ��������ɤ����񸻤��֤��ǥ��쥯�ȥ������Ǥ���褦�ˤ���
    * ���󥹥ȡ��뤹���������Ǥ���褦�ˤ���
    * �С�������ޤ᤿��¸�ط��ι�θ
    * �ѥå��μ�ưŬ��
    * Cygwin�б�

:Jun 27, 2003 (1.0.0)
  * �ǽ�Υ�꡼��
  * Thanks to ���Ȥ��󤵤�module NumRu::Fetch �κ���

=end
=begin html
<br>
<hr>
<address>
Copyright (C) 2003-2006 GFD Dennou Club. All rights reserved.
</address>
=end
