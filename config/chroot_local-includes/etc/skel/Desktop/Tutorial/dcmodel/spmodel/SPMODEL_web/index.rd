=begin JA
= ����Ū�ϵ�ή�Υ��ڥ��ȥ��ǥ뽸 SPMODEL
=end JA
=begin EN
= SPMODEL: Hierarchical Spectral Models for GFD
=end EN

=begin

# * 2010/03/02 (��������ʿ)
# * 2008/03/04 (�ݹ�����) 
# * 2008/02/19 (�ݹ�����) 
# * 2008/01/12 (�ݹ�����) 
# * 2008/04/23 (��������ʿ) 
# * 2007/12/05 (��������ʿ) 
# * 2007/10/10 (��������ʿ)
# * 2005/12/14 (�ӾͲ�)
# * 2005/10/01 (�ݹ�����) 
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

�ϵ�ή����Ǿ��������ڥ��ȥ��ǥ�ץ������ȤǤ�, �ϵ�ή���ϳؤθ���
�䶵��˻Ȥ��륹�ڥ��ȥ�ˡ���Ѥ������ޤ��ޤʿ��ͥ�ǥ�򤽤����ư��
�ʤ�Ƥ��ޤ�.


=end JA
=begin EN

In SPMODEL project, we are now developing hierarchical spectral models
for geophysical fluid dynamics which can be used for research and
education with ease.  


=end EN

=begin HTML
<table align="right">
<tr><td>
<A href="gallery/shallow-topo/">
<IMG ALT="shallow_topo_linear_small.jpg" SRC="./gallery/shallow_topo_linear_small.jpg" width=240, BORDER=1></a>
</td>
<td>
<A href="gallery/kh-instability//">
<IMG ALT="kh1.gif" SRC="./gallery/kh-instability/kh1.gif" align="center" BORDER=1 width=240></a>
</td>
</tr>
</table>

=end

=begin JA

* ((<SPMODEL ���ܻؤ����>))
* ((<�����꡼|URL:./gallery/index.htm>))
* ((<SPMODEL �λȤ���|URL:./html/usage.htm>))
  * ((<SPMODEL ��ɬ�פȤʤ��|URL:./html/needed.htm>))
  * ((<SPMODEL �饤�֥�� (spml)|URL:./html/spml.htm>))
* ((<�ץ���ླྀ|URL:./html/sample.htm>))
* ((<���塼�ȥꥢ�����|URL:./tutorial/index.htm>))
* ((<Live CD|URL:./LiveCD/index.htm>))
* ((<���Ѿ�����|URL:./html/licence.htm>))
* ((<ȯɽ����|URL:./html/paper.htm>))
* ((<�ռ�|URL:./html/acknowledge.htm>))
* ((<��󥯡���ȯ�Ը�������|URL:./html/link.htm>))

== �˥塼��(2010.03.02)

SPMODEL �饤�֥��(spml) �ο����С������ 0.6.0 ���꡼�����ޤ���.
����ι���/�ѹ����ϰʲ����̤�Ǥ�.

* �����ɲù���
  * x ������������, y ����̵���ΰ�Υ⥸�塼��(ef_module)
  * 3 ������ϩ�ΰ�Υ⥸�塼�� (tee_module)

== SPMODEL ���ܻؤ����

SPMODEL �Ǥ�, �ϵ�ή���ϳؤ��о줹�뤵�ޤ��ޤʥ�٥�ζ���������Ϥο�
�ͥ�ǥ��, ���� 1 ������ǥ뤫�� 2 �������뤤�� 3 ������ǥ�ޤǳ���
Ū���������Ƥ��ޤ�.

���Τ褦�ʰ�Ϣ�ο��ͥ�ǥ�򤽤��뤳�Ȥ�
* ɸ��Ū���뤤�Ͻ��פ� GFD �Υ��饹�ȥ졼�����򤿤䤹���Ƹ��Ǥ���褦�ˤ�����
  �� �ϵ�ή���ϳؤ��������ڤ���Ω�Ƥ�
* ���ͷ׻��ˤ���θ��򸦵�Ԥδ֤Ƕ�ͭ������
  �� ����ο����ˤ�����򤫤���ͷ׻��ˤ�������
* ��Ϣ�Υ�ǥ�ο������Ӥ򥹥ࡼ���˹Ԥ�����
  �� ���ʣ���ʥ�ǥ�η�̤����򤹤뤿���ƻ��
�Ȥ��ä����Ȥ��ܻؤ��Ƥ��ޤ�. 

���Τ���, �����������褦�Ȥ��Ƥ����ǥ륷�꡼���δ������ˤǤϡֲ�����
���⤯���򤷤䤹��, �ѷ����䤹�����ȡפ�ǽ����ˤ����Ƥ��ޤ�. ��̾�ץ�
����ޡ����(CPU��̵�¤�®��, ����ȥǥ��������̤�̵�¤��礭���Ȥ���
�Ķ��β��ǻŻ��򤹤�) �ˤ������ä�, ���ԡ��ɤϤȤꤢ���������ˤ��Ƥ⹽
��ʤ�, �Ȥ��������󥹤Ǥ�.  �������ʤ��餳�Υ�ǥ륷�꡼����١�����
���뤳�Ȥ�, �㤨�к���ü����׻���Ԥ����Ѥ����®�ʥ�ǥ�γ�ȯ�ʤɤ�
�ưפˤʤ뤳�Ȥ���Ԥ��Ƥ��ޤ�.

SPMODEL �Υ�ǥ�Ǥ�, �ʻ����ȥ��ڥ��ȥ���֤Υǡ����Ѵ��������ʬ�ʤɤ�
����Ū������ؿ�����ʤ� SPMODEL library (spml) ���Ѥ��Ƥޤ�. 
Fortran90 ������ǽ�������������Υ饤�֥�������ؿ����Ѥ��뤳�Ȥ�, 
����ȯŸ�������λ����Ѳ���ʳ�����ʬ��
�����η����Τޤޤ˥ץ���ߥ󥰤��뤳�Ȥ��Ǥ���褦�ˤʤäƤޤ�. 
�����������η��򤽤Τޤޥץ���ॽ������ȿ�Ǥ�������Τ�
�ץ����β���������夵���뤳�Ȥ��Ǥ��ޤ�. 
�ޤ�, spml ������ؿ��������������������̾������狼��褦�ˤ���٤�
����Ū��̿̾ˡ�ˤ������äƤ���Τ�, �ؿ��λȤ���������Ū�ˤʤ�, 
�ץ����������뤳�Ȥ��ưפ˹Ԥ���褦�ˤʤäƤ��ޤ�. 

SPMODEL �Υ饤�֥��ȥ���ץ�ץ���༫�Τ� Fortran90 �Υץ����
�󥰽�ˡ�ΰ�Ĥμ¸��Ǥ⤢��ޤ�. �ϵ�ή���ϳؤ�����˸¤餺���ޤ��ޤ�
ʪ�����ݤΥ��ڥ��ȥ�ˡ�ˤ����ͷ׻��ΰ�ĤΥ�������Ȥ��ƻ��ͤˤ���ĺ
����ФȻפ��ޤ�.


=end JA
=begin EN

* ((<The goal of SPMODEL>))
* ((<Gallery|URL:./gallery/index.htm.en>))
* ((<How to use SPMODEL ?|URL:./html/usage.htm.en>))
  * ((<Required softwares|URL:./html/needed.htm.en>))
  * ((<SPMODEL library (spml)|URL:./html/spml.htm.en>))
* ((<SPMODEL programs|URL:./html/sample.htm.en>))
* ((<Tutorial|URL:./tutorial/index.htm>))
* ((<Live CD|URL:./LiveCD/index.htm>))
* ((<Licence|URL:./html/licence.htm.en>))
* ((<Publication|URL:./html/paper.htm.en>))
* ((<Acknowledgement|URL:./html/acknowledge.htm.en>))
* ((<Link & Developer's archive|URL:./html/link.htm.en>))

== News(2010.03.02)

New version (0.6.0) of SPMODEL library (spml) was released. 
Modified points and newly implemented functions are as folllows:

* Newly implemented functions
  * Functions for mpi calculation (which call MPI routines of ISPACK)
  * A module for disk and sphere geometry by using Jacobi polynomials (aq_module)
  * A module for disk and sphere geometry by using Chebyshev polynomials and Gauss-Radau grid (au_module)
  * A module for axisymmetric geometry by using Legendre polynomials(l_module)
  * Modules for converting models of sphere and sperical shell geometries
    to the axisymmetric version (w_zonal_module, wa_zonal_module, wt_zonal_module)

* Modified points
  * Declaration of grid data arrays was changed from (im,jm) to (0:im-1, 1:jm).
  * Almost documantation were produced by rdoc-f95. 

In particular, please take care of the declaration of grid data arrays in
the spherical harmonics modules when you upgrade the spml library from 
the older version. 

== The goal of SPMODEL

In SPMODEL project, we develop numerical models based on various
approximation equation systems that appear in the geophysical fluid
dynamics hierarchically.

=end EN

