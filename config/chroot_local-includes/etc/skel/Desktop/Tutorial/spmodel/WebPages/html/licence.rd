=begin JA
= ���Ѿ�����
=end JA

=begin EN
= License
=end EN

=begin

# * 2012/02/29 (��������ʿ) ����
# * 2011/12/06 (��������ʿ) ����
# * 2010/04/06 (��������ʿ) ����
# * 2009/01/12 (�ݹ�����) spmodel ���󥯤����Хѥ����ѹ�
# * 2007/12/05 (��������ʿ) Makefile.rd2html �ι����˹�碌������
# * 2007/07/19 (�ݹ�����) �ǽ�����
# * 2007/04/24 (��������) �ǽ�����
# * 2005/09/30 (�ݹ�����) �ǽ�����
# * 2005/07/21 (��������) �ǽ�����
# * 2005/06/30 (��������) �ǽ�����
# * 2005/04/01 (��������) �ǽ�����
# * 2005/04/01 (��������)
# * 2005/03/30 (��������)
# * 2004/03/18 (��������)
# * 2004/01/26 (��������)
# * 2002/09/12 (�ݹ�����, �ӾͲ�)
# * 2002/02/15 (�ݹ�����) ��������

=end

=begin JA

== Copyright and License

SPMODEL �ϼ�ͳ�˻��ѡ����ѡ������ۤ��Ƥ��ޤ��ޤ���. ����������ϥ��ڥ��ȥ��ǥ볫ȯ���롼�פˤ���ޤ�.
�ܺ٤�((<�����ʥ饤���󥹵���|URL:../spml/COPYRIGHT>))�����ɤ�������.

�������������Ѥ��Ƥ��� ((<ISPACK|URL:/library/ispack/>)),
((<gtool5|URL:/library/gtool/gtool5.htm>)),
((<netCDF|URL:http://www.unidata.ucar.edu/packages/netcdf/>)),
�ʤɤ�¾�Υ饤�֥��ϥ饤���󥹵��꤬�ۤʤ�ΤǤ��줾���ǧ���Ƥ�������.

�饤�֥�ꡦ����ץ�ץ���ࡦ����ʸ�ʤ�,
SPMODEL �λ񸻤����Ѥ���ݤˤ�,
���Ƥ��̤ˤ錄�äơֶ���Ū�פǤ��뤳�Ȥ���դ��Ƥ�������.
�饤�֥��䥵��ץ�ץ�����ư��Ϥ����ޤǤ��̵�ݾڡפǤ�.
���Ѥ���ݤˤϳƼ����η�̤����������ǧ���Ƥ����Ѥ��Ƥ�������.
����ʸ�ʤɤˤ�ְ㤤�������������⤷��ޤ���.

�����θ��˵��Ť��줿�ݤˤ�,
�������ϵ�ή����Ǿ��������ڥ��ȥ��ǥ�ץ������ȥ��롼�פޤǸ����ĺ����й����Ǥ�.

=== ((<���ѤˤĤ���>))

�ܥץ����ϥե꡼���եȥ������Ȥ��ƥ�꡼������Ƥ��ޤ���,
SPMODEL �Υ饤�֥������Ѥ�����, ����ץ�ץ���ढ�뤤�ϥץ���ߥ󥰤ΥΥ��ϥ����򻲹ͤˤ�������줿���̤���ʸ�� web ���ˤ�ȯɽ����ݤˤ�,
������ʸ���ˤ� SPMODEL �����Ѥ��뤤�ϻ��ͤˤ����ݤ򵭤�, ��ե���󥹤ˤ�����ĺ���ޤ��褦���ꤤ���ޤ�.

������ϰʲ����̤�Ǥ�:

: ������ (��ʸ)
  �ݹ� ����, ������ ��ʿ, ���� ����, �в� ����, ���� ����, �ⶶ ˧��, ��߷ ����,
  ���� ���, ���� ����, �� �Ͳ�, spmodel ��ȯ���롼��, 2011:
  ����Ū�ϵ�ή���ϳإ��ڥ��ȥ��ǥ뽸 (SPMODEL),
  http://www.gfd-dennou.org/library/spmodel/, �ϵ�ή����Ǿ�����.


: ������ (��ʸ)
  Takehiro, S., SASAKI, Y., Morikawa, Y., Ishioka, K., Odaka, M., Takahashi, Y.O., Nishizawa, S.,
  Nakajima, K., Ishiwatari, M., Hayashi, Y.-Y.,
  SPMODEL Development Group, 2011:
  Hierarchical Spectral Models for GFD (SPMODEL),
  http://www.gfd-dennou.org/library/spmodel/, GFD Dennou Club.

�����դ���ʸ�������ѤǤ��ʤ����ˤ�,
ή���ϳز񥪥�饤�󥸥㡼�ʥ�֤ʤ���ޥ����ǥ����פβ�����ʸ���ե���󥹤Ȥ��Ƥ����Ƥ�������.

: ���ɤĤ���ʸ�ΰ����� (��ʸ)
  �ݹ�����, ��������, �в�����, ��������, �ӾͲ�, SPMODEL ��ȯ���롼��, 2006:
  ����Ū�ϵ�ή�Υ��ڥ��ȥ��ǥ뽸 SPMODEL,
  �ʤ���ޥ����ǥ��� 2006,
  ((<"http://www.nagare.or.jp/mm/2006/spmodel/"|URL:http://www.nagare.or.jp/mm/2006/spmodel/>))

: ���ɤĤ���ʸ�ΰ����� (��ʸ)
  Takehiro, S., Odaka, M., Ishioka, K., Ishiwatari, M., Hayashi, Y.-Y., 2006:
  SPMODEL: A Series of Hierarchical Spectral Models for Geophyiscal Fluid Dynamics,
  Nagare Multimedia 2006,
  ((<"http://www.nagare.or.jp/mm/2006/spmodel/"|URL:http://www.nagare.or.jp/mm/2006/spmodel/>))

=end
=begin EN

== Copyright and License

All the resources of SPMODEL are released as Free Software, copyrighted by SPMODEL Development Group.
Please read ((<COPYRIGHT|URL:../spml/COPYRIGHT>)) in detail.

Use of SPMODEL is permitted under the principle of "NO WARRANTY" and "NO RESPONSIBILITY".
SPMODEL Development Group shall not be liable for any event arising in any way out of the use of these resources.
Redistribution in source and binary forms, with or without modification,
is also permitted provided that the above copyright notice,
disclaimer and this condition are retained.

=== ((<Citation>))

Please be aware that the fact that these programs are released as Free
Software does not excuse you from scientific propriety, which obligates
you to give appropriate credit.

If you write a scientific paper describing research that made
substantive use of these programs, it is your obligation as a scientist
to (a) mention the fashion in which this software was used with a
citation to the literature or (b) mention this software in the Acknowledgements section.

:Example
  Takehiro, S., SASAKI, Y., Morikawa, Y., Ishioka, K., Odaka, M., Takahashi, Y.O., Nishizawa, S.,
  Nakajima, K., Ishiwatari, M., Hayashi, Y.-Y.,
  SPMODEL Development Group, 2011:
  Hierarchical Spectral Models for GFD (SPMODEL),
  http://www.gfd-dennou.org/library/spmodel/, GFD Dennou Club.

When you have to refer only an article of a refereed journal, please
cite our ariticle of online journal of Japan Society of Fluid Mechanics
as follows:

:Example
  Takehiro, S., Odaka, M., Ishioka, K., Ishiwatari, M., Hayashi, Y.-Y., 2006:
  SPMODEL: A Series of Hierarchical Spectral Models for Geophyiscal Fluid Dynamics,
  Nagare Multimedia 2006,
  ((<"http://www.nagare.or.jp/mm/2006/spmodel/"|URL:http://www.nagare.or.jp/mm/2006/spmodel/>))

=end

