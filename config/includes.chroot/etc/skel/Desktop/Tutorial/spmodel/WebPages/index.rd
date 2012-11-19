=begin JA
= ����Ū�ϵ�ή�Υ��ڥ��ȥ��ǥ뽸 SPMODEL
=end JA
=begin EN
= SPMODEL: Hierarchical Spectral Models for Geophysical Fluid Dynamics
=end EN

=begin

# * 2012/02/29 (��������ʿ)
# * 2012/02/27 (��������ʿ)
# * 2012/02/25 (��������ʿ)
# * 2011/09/16 (��������ʿ)
# * 2010/04/27 (��������ʿ)
# * 2010/04/11 (�ݹ�����)
# * 2010/04/06 (��������ʿ, �ݹ�����)
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

�ϵ�ή����Ǿ��������ڥ��ȥ��ǥ�ץ������ȤǤ�,
�ϵ�ή���ϳؤθ���䶵��˻Ȥ��륹�ڥ��ȥ�ˡ���Ѥ������ޤ��ޤʿ��ͥ�ǥ�򤽤����ư��ʤ�Ƥ��ޤ�.


=end JA
=begin EN

SPMODEL project is now developing various hierarchical spectral models
for geophysical fluid dynamics which can be easily used for research
and education.

=end EN

=begin HTML
<table align="right">
<tr><td>
<A href="gallery/shallow-topo/">
<IMG ALT="shallow_topo_linear_small.jpg" SRC="./gallery/shallow_topo_linear_small.jpg" width=240, BORDER=1></a>
</td>
<td>
<A href="gallery/kh-instability/">
<IMG ALT="kh1.gif" SRC="./gallery/kh-instability/kh1.gif" align="center" BORDER=1 width=240></a>
</td>
</tr>
</table>

=end

=begin JA

# * ((<SPMODEL ���ܻؤ����|URL:./html/goal.htm>))
* ((<SPMODEL ���ܻؤ����>))
* ((<�����꡼|URL:./gallery/index.htm>))
* ((<SPMODEL �λȤ���|URL:./html/usage.htm>))
  * ((<SPMODEL ��ɬ�פȤʤ��|URL:./html/needed.htm>))
  * ((<SPMODEL �饤�֥�� (spml)|URL:./html/spml.htm>))
* ((<�ץ���ླྀ|URL:./html/sample.htm>))
* ((<���塼�ȥꥢ�����|URL:./tutorial/index.htm>))
* ((<���Ѿ�����|URL:./html/licence.htm>))
* ((<ȯɽ����|URL:./html/paper.htm>))
* ((<��ȯ���С�|URL:./html/member.htm>))
* ((<�ռ�|URL:./html/acknowledge.htm>))
* ((<��󥯡���ȯ�Ը�������|URL:./html/link.htm>))

=end JA
=begin EN

# * ((<The goal of SPMODEL|URL:./html/goal.htm.en>))
* ((<The goal of SPMODEL>))
* ((<Gallery|URL:./gallery/index.htm.en>))
* ((<How to use SPMODEL ?|URL:./html/usage.htm.en>))
  * ((<Required softwares|URL:./html/needed.htm.en>))
  * ((<SPMODEL library (spml)|URL:./html/spml.htm.en>))
* ((<SPMODEL programs|URL:./html/sample.htm.en>))
* ((<Tutorial|URL:./tutorial/index.htm.en>))
* ((<Licence|URL:./html/licence.htm.en>))
* ((<Publication|URL:./html/paper.htm.en>))
* ((<Development group|URL:./html/member.htm.en>))
* ((<Acknowledgement|URL:./html/acknowledge.htm.en>))
* ((<Link & Developer's archive|URL:./html/link.htm.en>))
=end EN

=begin JA

== �˥塼��(2012.02.29)

SPMODEL �饤�֥��(spml) �ο����С������ 0.7.1 ���꡼�����ޤ���.
����ι���/�ѹ����ϰʲ����̤�Ǥ�.

* �饤�����ѹ�
  * �ȼ����ꥸ�ʥ� �� MIT �饤����.
    �ܺ٤�((<���Ѿ�����|URL:./html/licence.htm>))�򻲾Ȳ�����.
* �����ɲù���
  * ���̤���ӵ���ΰ��򤯤���Υ⥸�塼��� sjpack_cuda ������
    * w_module_sjpack_cuda, wa_module_sjpack_cuda, wt_module_sjpack_cuda
  * �������太��ӵ���ѥ⥸�塼��� MPI ������
    * ��������: wq_mpi_module
    * ��������� + ���: wtq_mpi_module
  * ��������ϩ�ΰ��򤯤���Υ⥸�塼��� fftj ������
    * tee_module_fftj, eea_module, eea_module_fftj
  * ������������ LU ʬ��ǲ򤯺ݤ� CUDA ����Ѥ���⥸�塼��: lumatrix_cuda
    * �� PGI Compiler + Accelerator (>= 2010)
* �Х�����/����¾���ѹ��ʤ�
  * ���оΥ⥸�塼�����������ΰ�ν���
  * FORTRAN77 �Υ��֥롼����򤹤٤� fortran 90 ��

=end JA
=begin EN

== News(2012.02.29)

New version (0.7.1) of SPMODEL library (spml) was released.
Modified points and newly implemented functions are as follows:

* License changed
  * original license �� MIT License.
    Please see ((<License|URL:./html/licence.htm.en>)) in detail.
* Newly implemented functions
  * Modules of sphere and spherical shell geometry using SJPACK_CUDA
    * w_module_sjpack_cuda, wa_module_sjpack_cuda, wt_module_sjpack_cuda
  * Modules of sphere and spherical shell geometry, MPI parallelization
    * wq_mpi_module, wtq_mpi_module
  * A module for solving systems of linear equations by use of CUDA: lumatrix_cuda
    * Depends: PGI Compiler + Accelerator (>= 2010)
* Bug fixed/misc. update
  * Modules of axial symmetry: fix working space size
  * Remove FORTRAN77 files: rewrite in fortran 90

=end EN

=begin JA
== SPMODEL ���ܻؤ����

SPMODEL �Ǥ�, �ϵ�ή���ϳؤ��о줹�뤵�ޤ��ޤʥ�٥�ζ���������Ϥο��ͥ�ǥ��, ���� 1 ������ǥ뤫�� 2 �������뤤�� 3 ������ǥ�ޤǳ���Ū���������Ƥ��ޤ�.

���Τ褦�ʰ�Ϣ�ο��ͥ�ǥ�򤽤��뤳�Ȥ�
* ɸ��Ū���뤤�Ͻ��פ� GFD �Υ��饹�ȥ졼�����򤿤䤹���Ƹ��Ǥ���褦�ˤ�����
  �� �ϵ�ή���ϳؤ��������ڤ���Ω�Ƥ�
* ���ͷ׻��ˤ���θ��򸦵�Ԥδ֤Ƕ�ͭ������
  �� ����ο����ˤ�����򤫤���ͷ׻��ˤ�������
* ��Ϣ�Υ�ǥ�ο������Ӥ򥹥ࡼ���˹Ԥ�����
  �� ���ʣ���ʥ�ǥ�η�̤����򤹤뤿���ƻ��
�Ȥ��ä����Ȥ��ܻؤ��Ƥ��ޤ�.

���Τ���, �����������褦�Ȥ��Ƥ����ǥ륷�꡼���δ������ˤǤϡֲ��������⤯���򤷤䤹��, �ѷ����䤹�����ȡפ�ǽ����ˤ����Ƥ��ޤ�. ��̾�ץ���ޡ����(CPU��̵�¤�®��, ����ȥǥ��������̤�̵�¤��礭���Ȥ����Ķ��β��ǻŻ��򤹤�) �ˤ������ä�, ���ԡ��ɤϤȤꤢ���������ˤ��Ƥ⹽��ʤ�, �Ȥ��������󥹤Ǥ�.  �������ʤ��餳�Υ�ǥ륷�꡼����١����ˤ��뤳�Ȥ�, �㤨�к���ü����׻���Ԥ����Ѥ����®�ʥ�ǥ�γ�ȯ�ʤɤ��ưפˤʤ뤳�Ȥ���Ԥ��Ƥ��ޤ�.

SPMODEL �Υ�ǥ�Ǥ�, �ʻ����ȥ��ڥ��ȥ���֤Υǡ����Ѵ��������ʬ�ʤɤδ���Ū������ؿ�����ʤ� SPMODEL library (spml) ���Ѥ��Ƥޤ�. Fortran90 ������ǽ�������������Υ饤�֥�������ؿ����Ѥ��뤳�Ȥ�, ����ȯŸ�������λ����Ѳ���ʳ�����ʬ������η����Τޤޤ˥ץ���ߥ󥰤��뤳�Ȥ��Ǥ���褦�ˤʤäƤޤ�. ���������η��򤽤Τޤޥץ���ॽ������ȿ�Ǥ�������Τǥץ����β���������夵���뤳�Ȥ��Ǥ��ޤ�. �ޤ�, spml ������ؿ��������������������̾������狼��褦�ˤ���٤�����Ū��̿̾ˡ�ˤ������äƤ���Τ�, �ؿ��λȤ���������Ū�ˤʤ�, �ץ����������뤳�Ȥ��ưפ˹Ԥ���褦�ˤʤäƤ��ޤ�.

SPMODEL �Υ饤�֥��ȥ���ץ�ץ���༫�Τ� Fortran90 �Υץ���ߥ󥰽�ˡ�ΰ�Ĥμ¸��Ǥ⤢��ޤ�. �ϵ�ή���ϳؤ�����˸¤餺���ޤ��ޤ�ʪ�����ݤΥ��ڥ��ȥ�ˡ�ˤ����ͷ׻��ΰ�ĤΥ�������Ȥ��ƻ��ͤˤ���ĺ����ФȻפ��ޤ�.

=end JA
=begin EN
== The goal of SPMODEL

SPMODEL project is developing a series of numerical models based on various systems of approximate equations, that appear in the geophysical fluid dynamics.
The model series is arranged hierarchically from simple spartial one-dimensinal models to complex two or three dimensional models.

The aims of the series of numerical models are as follows:
* Easy reproduction of illustrations of standard or important GFD problems
  in order to make use of understandings and teaching
  of geophysical fluid dynamics.
* Sharing knowledge obtained by numerical experiments with ease:
  from the era of unserstanding with conventional mathematical equations
  to the era of understanding through the results of numerical calculations.
* Smooth comparison bwtween the numerical results of the series of the models:
  it is helpful in understanding of the results of more complex models.

For such purposes, readability and understandability of the source codes to permit easy re-building and/or modification are given priority in designing the program source codes. This policy might sacrifice calculation speed of the programs, however, we anticipate that on the basis of this model series,
advanced models, which can perform leading-edge massive calculations for exapmle, could be developed easily.

The programs of the series of the models use "SPMODEL library (spml)", which provides basic functions for numerical fluid dynamics calculations with the spectral methods, such as conversion between grid and spectral data and spartial derivatives. By using the array-valued functions of SPMODEL library under favor of
the array operation features of Fortran90, the main part of each program source code is written in a similar form to the original mathematical expressions,
which contributes to readability and understandability of the program source codes. Moreover, with the help of the introduction of systematic function naming rules of the array-valued functions of SPMODEL library, we can use these functions routinely, and the program source codes can be modified easyly and safely.

The SPMODEL library and the sample programs serve as one of the experiments for Fortran90 programing style. We are pleased if we colud offer them as references for programing style of several numerical calculations not only in geophysical fluid dynamics but also in other several fields of phisics.

=end EN
