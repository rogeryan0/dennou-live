=begin JA
= ���Ѿ�����
=end JA

=begin EN
= Licence
=end EN

=begin

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

SPMODEL �ϸ��桦����ξ���Ѥ����뤳�Ȥ�����Ȥ��Ƥ���ޤ�.
�ϵ�ή����Ǿ�����(SPMODEL ��ȯ���롼��)�γ�ư��˸����褦�����Ѥλ���
�Ǥʤ��¤�, ��ͳ�˻��ѡ����ѡ������ۤ��Ƥ��ޤ��ޤ���. 
((<�����ʥ饤���󥹵���|URL:../spml/COPYRIGHT>))�����ɤ�������. 
�������������Ѥ��Ƥ��� ((<ISPACK|URL:/library/ispack/>)),
((<gt4f90io|URL:/library/gtool4/>)),
((<netCDF|URL:http://www.unidata.ucar.edu/packages/netcdf/>)),
�ʤɤ�¾�Υ饤�֥��ϥ饤���󥹵��꤬�ۤʤ�ΤǤ��줾���ǧ���Ƥ�������.

�饤�֥�ꡦ����ץ�ץ���ࡦ����ʸ�ʤ�, SPMODEL �λ񸻤����Ѥ����
�ˤ�, ���Ƥ��̤ˤ錄�äơֶ���Ū�פǤ��뤳�Ȥ���դ��Ƥ�������.  �饤
�֥��䥵��ץ�ץ�����ư��Ϥ����ޤǤ��̵�ݾڡפǤ�.  ���Ѥ���
�ݤˤϳƼ����η�̤����������ǧ���Ƥ����Ѥ��Ƥ�������.  ����ʸ�ʤɤ�
��ְ㤤�������������⤷��ޤ���.

�����θ��˵��Ť��줿�ݤˤ�, �������ϵ�ή����Ǿ��������ڥ��ȥ��
�ǥ�ץ������ȥ��롼�פޤǸ����ĺ����Ф��꤬�����Ǥ�.

=== ������

SPMODEL �Υ饤�֥������Ѥ�����, ����ץ�ץ���ढ�뤤�ϥץ����
�󥰤ΥΥ��ϥ����򻲹ͤˤ�������줿���̤���ʸ�� web ���ˤ�ȯɽ�����
�ˤ�, �Ǥ������������ʸ���ˤ� SPMODEL �����Ѥ��뤤�ϻ��ͤˤ����ݤ�
��, ��ե���󥹤ˤ�����ĺ���ޤ��褦���ꤤ���ޤ�.

: ������ (��ʸ)

    �ݹ�����, �в�����, ��߷����, ë����, ��������, ��������, ��������, �ӾͲ�, 
    SPMODEL ��ȯ���롼��, 2007:  
    ����Ū�ϵ�ή���ϳإ��ڥ��ȥ��ǥ뽸 (SPMODEL),  
    http://www.gfd-dennou.org/library/spmodel/, �ϵ�ή����Ǿ�����. 

#    �ݹ�����, �в�����, ��߷����, ë����, ��������, ��������, ��������, �ӾͲ�, 
#    SPMODEL ��ȯ���롼��, 2006:  
#    ����Ū�ϵ�ή���ϳإ��ڥ��ȥ��ǥ뽸 (SPMODEL),  
#    http://www.gfd-dennou.org/library/spmodel/, �ϵ�ή����Ǿ�����. 

: ������ (��ʸ)

    Takehiro, S., Ishioka, K., Nishizawa, S., Taniguchi, H., Morikawa, Y., Odaka, M., Ishiwatari, M., Hayashi, Y.-Y.,
    SPMODEL Development Group, 2007: 
    Hierarchical Spectral Models for GFD (SPMODEL),
    http://www.gfd-dennou.org/library/spmodel/, GFD Dennou Club.

#    Takehiro, S., Ishioka, K., Kakinami, Y., Nishizawa, S., Morikawa, Y., Odaka, M., Ishiwatari, M., Hayashi, Y.-Y.,
#    SPMODEL Development Group, 2005: 
#    Hierarchical Spectral Models for GFD (SPMODEL),
#    http://www.gfd-dennou.org/library/spmodel/, GFD Dennou Club.

�����դ���ʸ�������ѤǤ��ʤ����ˤ�, ή���ϳز񥪥�饤�󥸥㡼�ʥ�֤ʤ�
��ޥ����ǥ����פβ�����ʸ���ե���󥹤Ȥ��Ƥ����Ƥ�������. 

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

Please read ((<COPYRIGHT|URL:../spml/COPYRIGHT>)) file.

=== Citation

Takehiro, S., Ishioka, K., Nishizawa, S., Taniguchi, H., Morikawa, Y., Odaka, M., Ishiwatari, M., Hayashi, Y.-Y.,
SPMODEL Development Group, 2007: 
Hierarchical Spectral Models for GFD (SPMODEL),
http://www.gfd-dennou.org/library/spmodel/, GFD Dennou Club.

#    Takehiro, S., Ishioka, K., Kakinami, Y., Nishizawa, S., Morikawa, Y., Odaka, M., Ishiwatari, M., Hayashi, Y.-Y.,
#    SPMODEL Development Group, 2007: 
#    Hierarchical Spectral Models for GFD (SPMODEL),
#    http://www.gfd-dennou.org/library/spmodel/, GFD Dennou Club.

When you have to refer only an article of a refereed journal, please 
cite our ariticle of online journal of Japan Society of Fluid Mechanics
as follows:

  Takehiro, S., Odaka, M., Ishioka, K., Ishiwatari, M., Hayashi, Y.-Y., 2006:
  SPMODEL: A Series of Hierarchical Spectral Models for Geophyiscal Fluid Dynamics,
  Nagare Multimedia 2006, 
  ((<"http://www.nagare.or.jp/mm/2006/spmodel/"|URL:http://www.nagare.or.jp/mm/2006/spmodel/>))

=end

