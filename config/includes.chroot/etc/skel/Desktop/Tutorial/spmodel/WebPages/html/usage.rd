=begin JA
= SPMODEL �λȤ���
=end JA
=begin EN
= How to use SPMODEL ?
=end EN

=begin

# * 2009/03/03 (��������ʿ) 0.5 ��꡼���ˤȤ�ʤ�����
# * 2009/02/19 (�ݹ�����) tutorial �ؤΥ�󥯤����
# * 2009/01/12 (�ݹ�����) spmodel ���󥯤����Хѥ����ѹ�
# * 2007/12/05 (��������ʿ) �����ν���
# * 2007/12/05 (��������ʿ) Makefile.rd2html �ι����˹�碌������
# * 2007/07/19 (�ݹ�����) 
# * 2005/07/25 (��������) 
# * 2005/07/21 (��������) 
# * 2005/06/30 (��������) 
# * 2005/04/01 (��������) 
# * 2005/04/01 (��������)
# * 2005/03/30 (��������)
# * 2004/03/18 (��������)
# * 2004/01/26 (��������)
# * 2002/09/12 (�ݹ�����, �ӾͲ�)
# * 2002/02/15 (�ݹ�����) ��������

=end

=begin JA

== ���� -- �饤�֥��Υ��󥹥ȡ���

�ޤ� ((<SPMODEL ��ɬ�פȤʤ��|URL:./needed.htm>)) 
�򥤥󥹥ȡ��뤷�Ƥ�������. 
Debian GNU/Linux (i386, amd64) �Ķ��ʤ��
((<deb �ѥå�����|URL:../debian/needed.htm>)) �����ѤǤ��ޤ�.

����((<SPMODEL �饤�֥�� (spml)|URL:./spml.htm>))�򥤥󥹥ȡ��뤷�ޤ�.
����ϥ������������������
((<INSTALL.html.ja|URL:../spml/html/INSTALL.html.ja>)) �Ǥ�.
Debian GNU/Linux (i386, amd64) �Ķ��ʤ�� ((<deb �ѥå�����|URL:../debian/>))
�����ѤǤ��ޤ�.

== �ǥ�, ����ץ�ץ�����ư�����Ƥߤ�

spml �򥤥󥹥ȡ���Ǥ�����, spml ����°�Υǥ�ץ����򥳥�ѥ��뤷, 
�¹Ԥ��ƤߤƤ�������. spml �����������󥹥ȡ��뤵��Ƥ���ʤ饳��ѥ�
���ѤΥ�����ץ� ((<spmfrt|inline>)) ���Ȥ���Ϥ��Ǥ�. 

 $ spmfrt hogehoge.f90
 $ ./a.out

�¹Ը�Ϸ׻���̤� gtool4 �����Υǡ����ե����뤬��������, ���Ϥ���ޤ�.
((<dennou ruby ����|URL:/library/ruby>))�� gpview ���ޥ�������Ѥ��Ʒ��
��ɽ�����ƤߤƤ�������.

���ޤ����ä���, ������((<����ץ�ץ����>))���������ɤ���Ʊ����
����ư�����Ƥߤޤ��礦.

== �ץ�����񤤤Ƥߤ�.

SPMODEL �Υץ��������Υ��Ĥ�
((<"���塼�ȥꥢ��"|URL:../tutorial/index.htm>))���뤤��
ή���ϳز񥪥�饤�󥸥㡼�ʥ�֤ʤ���ޥ����ǥ����פβ�����ʸ
((<"http://www.nagare.or.jp/mm/2006/spmodel/"|URL:http://www.nagare.or.jp/mm/2006/spmodel/>))
�ʤɤ򻲹ͤˤ���, ����ץ�ץ������ѹ�������, ��ʬ�ǥץ�����񤤤�
�ڤ���Ǥ�������.


=end JA
=begin EN

== Step 0: Install required library

Install ((<Required softwares|URL:./needed.htm.en>)).
If you use Debian GNU/Linux (i386, amd64), 
((<binary packages |URL:../debian/needed.htm.en>)) can be used.


== Step 1: Install spml library

Install ((<SPMODEL library (spml)|URL:./spml.htm.en>)). 
Please read install guide
((<INSTALL.html|URL:../spml/html/INSTALL.html>)).
If you use Debian GNU/Linux (i386,amd64), 
((<binary packages |URL:../debian/index.htm.en>)) can be used.


== Step 2: Use demo and sample programs

Please compile the demonstration program attached to spml library and
execute. If installation of spml library is succeeded,
((<spmfrt|inline>)) command can be use to compile.

 $ spmfrt hogehoge.f90
 $ ./a.out

Output data format is gtool4/netCDF convection. 
Gpview which is one of the ((<dennou ruby products|URL:/library/ruby>))
is useful for displaying gtool4/netCDF binary data.

If it is succeeded in executing the demonstration program, download,
compile and execute ((<SPMODEL sample programs>)).


== Farther Step

Please change the sample program, and enjoy writing the program for 
yourself referring to the
((<"tutorial of SPMODEL programming manner"|URL:../tutorial/index.htm.en>))
(Be preparing it now), or our article on the online journal of Japan Society
of Fluid Mechanics, 
((<"http://www.nagare.or.jp/mm/2006/spmodel/"|URL:http://www.nagare.or.jp/mm/2006/spmodel/>)).


=end EN


