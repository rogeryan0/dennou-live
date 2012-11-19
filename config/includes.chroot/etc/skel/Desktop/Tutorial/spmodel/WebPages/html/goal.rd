=begin JA
= SPMODEL ���ܻؤ����
=end JA
=begin EN
= The goal of SPMODEL
=end EN
=begin

# * 2012/02/27 (��������ʿ) �Ѹ콤���ɵ�
# * 2007/12/05 (��������ʿ) Makefile.rd2html �ι����˹�碌������
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

Fortran90 ������ǽ�������������Υ饤�֥�������ؿ����Ѥ��뤳�Ȥ�, ����ȯŸ�������λ����Ѳ���ʳ�����ʬ������η����Τޤޤ˥ץ���ߥ󥰤��뤳�Ȥ��Ǥ���褦�ˤʤäƤޤ�. �����������η��򤽤Τޤޥץ���ॽ������ȿ�Ǥ�������Τǥץ����β���������夵���뤳�Ȥ��Ǥ��ޤ�. �ޤ�, spml ������ؿ��������������������̾������狼��褦�ˤ���٤�����Ū��̿̾ˡ�ˤ������äƤ���Τ�, �ؿ��λȤ���������Ū�ˤʤ�, �ץ����������뤳�Ȥ��ưפ˹Ԥ���褦�ˤʤäƤ��ޤ�.

SPMODEL �Υ饤�֥��ȥ���ץ�ץ���༫�Τ� Fortran90 �Υץ���ߥ󥰽�ˡ�ΰ�Ĥμ¸��Ǥ⤢��ޤ�. �ϵ�ή���ϳؤ�����˸¤餺���ޤ��ޤ�ʪ�����ݤΥ��ڥ��ȥ�ˡ�ˤ����ͷ׻��ΰ�ĤΥ�������Ȥ��ƻ��ͤˤ���ĺ����ФȻפ��ޤ�.


=end JA
=begin EN

SPMODEL project is developing a series of numerical models
based on various systems of approximate equations,
that appear in the geophysical fluid dynamics.
The model series is arranged hierarchically from simple
spartial one-dimensinal models to complex
two or three dimensional models.

The aims of the series of numerical models are as follows:
* Easy reproduction of illustrations of standard or important GFD problems
  in order to make use of understandings and teaching
  of geophysical fluid dynamics.
* Sharing knowledge obtained by numerical experiments with ease:
  from the era of unserstanding with conventional mathematical equations
  to the era of understanding through the results of numerical calculations.
* Smooth comparison bwtween the numerical results of the series of the models:
  it is helpful in understanding of the results of more complex models.

For such purposes, readability and understandability of the source codes
to permit easy re-building and/or modification
are given priority in designing the program source codes.
This policy might sacrifice calculation speed of the programs, however,
we anticipate that on the basis of this model series,
advanced models,
which can perform leading-edge massive calculations for exapmle,
could be developed easily.

The programs of the series of the models use "SPMODEL library (spml)",
which provides basic functions for numerical fluid dynamics calculations
with the spectral methods, such as conversion between grid and
spectral data and spartial derivatives.
By using the array-valued functions of SPMODEL library under favor of
the array operation features of Fortran90,
the main part of each program source code is written
in a similar form to the original mathematical expressions,
which contributes to readability and understandability
of the program source codes.
Moreover, with the help of the introduction
of systematic function naming rules of the array-valued functions
of SPMODEL library, we can use these functions routinely,
and the program source codes can be modified easyly and safely.

The SPMODEL library and the sample programs serve as one of
the experiments for Fortran90 programing style.
We are pleased if we colud offer them as references for programing style of
several numerical calculations not only in geophysical fluid dynamics
but also in other several fields of phisics.

=end EN

