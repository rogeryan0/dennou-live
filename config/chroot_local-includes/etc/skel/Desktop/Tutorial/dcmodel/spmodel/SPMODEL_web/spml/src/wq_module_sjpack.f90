!--
!----------------------------------------------------------------------
! Copyright(c) 2009-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wq_module_sjpack
!
!    spml/wq_module_sjpack �⥸�塼��ϵ���Ǥ�ή�α�ư�򥹥ڥ��ȥ�ˡ��
!    ��äƿ��ͷ׻����뤿��� Fortran90 �ؿ����󶡤����ΤǤ���. 
!
!    ��ʿ�����˵���Ĵ��ȡ���Ѵ������ư��������
!    Matsushima and Marcus (1994) ���󾧤��줿¿�༰���Ѥ���
!    ���ڥ��ȥ�׻��Τ���Τ��ޤ��ޤʴؿ����󶡤���. 
!
!    ������ wa_module_sjpack, aq_module ���Ѥ��Ƥ���. 
!    �ǲ����Ǥϵ���Ĵ���Ѵ��Ѵ��Υ��󥸥�Ȥ��� 
!    ISPACK �� Fortran77 ���֥롼������Ѥ��Ƥ���.
!
!    �ؿ�, ���֥롼�����̾���ȵ�ǽ�� wq_module �Τ�Τ�Ʊ���Ǥ���. 
!    �������ä� use ʸ�� wq_module ���� wq_module_sjpack ��
!    �ѹ���������� SJPACK �ε�ǽ���Ȥ���褦�ˤʤ�. 
! 
!    ������ l_nm, nm_l �λȤ����ˤ���դ��줿��. wq_module �� l_nm,
!    nm_l ��wq_Initial �ǽ�������ʤ��Ȥ��Ѥ��뤳�Ȥ��Ǥ���(��̤�����
!    �ȿ��˰ͤ�ʤ�)��, wq_module_sjpack �Τ�ΤϽ���������Τ��ˤ�����
!    �����Ȥ��Ǥ��ʤ�.
!
!    Matsushima and Marcus (1994) ��¿�༰�˴ؤ��������� 
!    doc/spectral_radial.tex �򻲾ȤΤ���. 
!
!
!����  2008/04/03  �ݹ�����  wu_module ������
!      2008/05/02  �ݹ�����  �������ɲ�
!      2008/07/01  ��������ʿ ��ʿ�����γʻ����� 0:im-1,1:jm �˽���
!      2008/07/04  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2008/07/20  �ݹ�����  wq_Rad2_wq, wq_Rad2Inv_wq, wq_Lapla_wq �ɲ�
!      2008/07/21  �ݹ�����  ��������׻��ѹ�
!      2009/01/09  �ݹ�����  wu_Initial ��å����������դ��ɲ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/07/31  �ݹ�����  �������׻�������� threadprivate ����(OpenMP)
!      2009/12/06  �ݹ�����  ��Ƭ�����Ȥ���, threadprivate �����ȥ�����
!      2010/03/10  ��������ʿ  threadprivate ���(����ѥ����¸)
!
!����
!      �ǡ�������� index
!        x : ����         y : ����        r : ư��
!        w : ����Ĵ�´ؿ����ڥ��ȥ�
!        n : ����Ĵ�´ؿ����ڥ��ȥ�(��ʿ���ȿ�)
!        m : ����Ĵ�´ؿ����ڥ��ȥ�(�Ӿ��ȿ�)
!        q : ���ڥ��ȥ�ؿ����ڥ��ȥ�
!        a : Ǥ�դμ���
!
!        xyr : 3 �����ʻ����ǡ���
!        xy  : ��ʿ 2 �����ʻ����ǡ���
!        yr  : �Ҹ��� 2 �����ʻ����ǡ���
!        xr  : ������ 2 �����ʻ����ǡ���
!
!        wr  : ��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���
!        wq  : ���ڥ��ȥ�ǡ���
!
!++
module wq_module_sjpack
  !
  != wq_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wq_module_sjpack.f90,v 1.2 2010-03-02 10:09:06 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/wq_module_sjpack �⥸�塼��ϵ���Ǥ�ή�α�ư�򥹥ڥ��ȥ�ˡ��
  ! ��äƿ��ͷ׻����뤿��� Fortran90 �ؿ����󶡤����ΤǤ���. 
  !
  ! ��ʿ�����˵���Ĵ��ȡ���Ѵ������ư��������
  ! Matsushima and Marcus (1994) ���󾧤��줿¿�༰���Ѥ���
  ! ���ڥ��ȥ�׻��Τ���Τ��ޤ��ޤʴؿ����󶡤���. 
  !
  ! ������ wa_module, aq_module ���Ѥ��Ƥ���. 
  ! �ǲ����Ǥϵ���Ĵ���Ѵ��Υ��󥸥�Ȥ��� 
  ! ISPACK �� Fortran77 ���֥롼������Ѥ��Ƥ���.
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (wq_, nmr_, nr_, xyr_, wr_, w_, xy_, x_, y_, r_, a_) ��, 
  !   �֤��ͤη��򼨤��Ƥ���.
  !   wq_  :: ���ڥ��ȥ�ǡ���(����Ĵ��ȡ���������ӥ������Ѵ�)
  !   nmr_ :: ��ʿ���ڥ��ȥ�ǡ���(���ȿ� n, �Ӿ��ȿ�����ʬ, ư��)
  !   nr_  :: ��ʿ���ڥ��ȥ�ǡ���(���ȿ� n, ư��)
  !   xyr_ :: 3 �����ʻ����ǡ���(���١����١�ư��)
  !   wr_  :: ��ʿ���ڥ��ȥ�, ư�³ʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(DLon, GradLat, GradLat, DivLon, DivLat, Lapla,..)
  !   ��, ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (wq_, xyz_, wr_, w_, xy_, x_, y_, r_, a_) ��, �����ѿ���
  !   �������ڥ��ȥ�ǡ�������ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _wq      :: ���ڥ��ȥ�ǡ���
  !   _xyr     :: 3 �����ʻ����ǡ���
  !   _xyr_xyr :: 2 �Ĥ�3 �����ʻ����ǡ���, ...
  !
  !=== �ƥǡ����μ��������
  !
  ! * xyr : 3 �����ʻ����ǡ���(���١����١�ư��)
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:im-1,1:jm,km). 
  !   * im, jm, km �Ϥ��줾�����, ����, ư�º�ɸ�γʻ������Ǥ���, 
  !     ���֥롼���� wq_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * wq : ���ڥ��ȥ�ǡ���
  !   * �ѿ��μ���ȼ����� real(8), dimension((nm+1)*(nm+1),0:lm). 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ�, lm �ϥ����ӥ�����¿�༰�κ��缡��
  !     �Ǥ���, ���֥롼���� wq_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !   * ��ʿ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ϴؿ� l_nm, nm_l �ˤ�ä�Ĵ�٤�
  !     ���Ȥ��Ǥ���. 
  !   * ư�¥��ڥ��ȥ�ǡ����γ�Ǽ��ˡ�ˤĤ��Ƥ� aq_module.f90 ��
  !     ���ȤΤ���. km < 2*im �Ǥʤ���Фʤ�ʤ�. 
  !
  ! * nmr : ��ʿ���ڥ��ȥ�ǡ������¤�� 3 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,-nm:nm,km). 
  !   * �� 1 ��������ʿ���ȿ�, �� 2 �������Ӿ��ȿ�, �� 3 ������ư�º�ɸ��ɽ��. 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, ���֥롼���� wq_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���.
  !
  ! * nr : ���ڥ��ȥ�ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,km). 
  !   * �� 1 ��������ʿ���ȿ���ɽ��. nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, 
  !     ���֥롼���� wq_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * wr : ��ʿ���ڥ��ȥ�, ư�³ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension((nm+1)*(nm+1),km).
  !
  ! * wq_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * xyr_ �ǻϤޤ�ؿ����֤��ͤ� 3 �����ʻ����ǡ�����Ʊ��.
  !
  ! * wr_ �ǻϤޤ�ؿ����֤��ͤϿ�ʿ���ڥ��ȥ�, ư�³ʻ����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  ! 
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! wq_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! x_Lon, y_Lat, r_Rad          :: �ʻ�����ɸ(����, ����, ư�º�ɸ)��
  !                                 ��Ǽ����1 ��������
  ! x_Lon_Weight, y_Lat_Weight, r_Rad_Weight :: �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! xyr_Lon, xyr_Lat, xyr_Rad    :: �ʻ����ǡ����η��١����١�ư�º�ɸ(X,Y,Z)
  !                                 (�ʻ����ǡ����� 3 ��������)
  !
  !==== �����Ѵ�
  !
  ! xyr_wq, wq_xyr :: ���ڥ��ȥ�ǡ����� 3 �����ʻҥǡ����δ֤��Ѵ�
  !                   (����Ĵ��ȡ��, �����ӥ������Ѵ�)
  !
  ! xyr_wr, wr_xyr :: 3 �����ʻҥǡ����ȿ�ʿ���ڥ��ȥ롦ư�³ʻҥǡ����Ȥ�
  !                   �֤��Ѵ� (����Ĵ��ȡ��)
  !
  ! wr_wq, wq_wr   :: ���ڥ��ȥ�ǡ����ȿ�ʿ���ڥ��ȥ롦ư�³ʻҥǡ����Ȥ�
  !                   �֤��Ѵ� (�����ӥ������Ѵ�)
  !
  ! w_xy, xy_w     :: ���ڥ��ȥ�ǡ����� 2 ������ʿ�ʻҥǡ�����
  !                   �֤��Ѵ�(����Ĵ��ȡ���Ѵ�) 
  !
  ! l_nm, nm_l     :: ���ڥ��ȥ�ǡ����γ�Ǽ���֤����ȿ����Ӿ��ȿ����Ѵ� 
  !
  !==== ��ʬ
  !
  ! wq_RadDRad_wq       :: ���ڥ��ȥ�ǡ�����ư����ʬ r��/��r ����Ѥ�����
  ! wr_DivRad_wq        :: ���ڥ��ȥ�ǡ�����ȯ����ư����ʬ
  !                        1/r^2 ��/��r r^2 = ��/��r + 2/r ����Ѥ�����
  ! wr_RotDRad_wq       :: ���ڥ��ȥ�ǡ����˲�ž��ư����ʬ
  !                        1/r ��/��rr = ��/��r + 1/r ����Ѥ�����
  ! wr_RotDRad_wr       :: ���ڥ��ȥ�ǡ����˲�ž��ư����ʬ
  !                        1/r ��/��rr = ��/��r + 1/r ����Ѥ�����
  ! wq_RotDRad_wr       :: ���ڥ��ȥ�ǡ����˲�ž��ư����ʬ
  !                        1/r ��/��rr = ��/��r + 1/r ����Ѥ�����
  ! wq_Lapla_wq         :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! xyr_GradLon_wq      :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  !                        1/rcos�ա���/�ߦˤ���Ѥ�����
  ! xyr_GradLat_wq      :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  !                        1/r����/�ߦդ���Ѥ�����
  ! wr_DivLon_xyr       :: �ʻҥǡ�����ȯ����������ʬ
  !                        1/rcos�ա���/�ߦˤ���Ѥ�����
  ! wr_DivLat_xyr       :: �ʻҥǡ�����ȯ����������ʬ
  !                        1/rcos�ա���(g cos��)/�ߦդ���Ѥ�����
  ! wr_Div_xyr_xyr_xyr  :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����
  !                        ȯ������Ѥ�����
  ! xyr_Div_xyr_xyr_xyr :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����
  !                        ȯ������Ѥ�����
  ! xyr_RotLon_wq_wq    :: �٥��ȥ��β�ž�η�����ʬ��׻�����
  ! xyr_RotLat_wq_wq    :: �٥��ȥ��β�ž�ΰ�����ʬ��׻�����
  ! wr_RotRad_xyr_xyr   :: �٥��ȥ��β�ž��ư����ʬ��׻�����
  !
  !==== �ȥ�����ݥ�����׻�����ʬ
  !
  ! wq_KxRGrad_wq     :: ���ڥ��ȥ�ǡ����˷�����ʬ
  !                      k��r���� = ��/�ߦˤ���Ѥ�����
  ! xyr_KGrad_wq      :: ���ڥ��ȥ�ǡ����˼�������ʬ
  !                      k���� = cos��/r ��/�ߦ� + sin�բ�/��r ����Ѥ�����
  ! wq_L2_wq          :: ���ڥ��ȥ�ǡ����� L2 �黻�� = -��ʿ��ץ饷�����
  !                      ���Ѥ�����
  ! wq_L2Inv_wq       :: ���ڥ��ȥ�ǡ����� L2 �黻�Ҥε� = -�տ�ʿ��ץ饷����
  !                      ����Ѥ�����
  ! wq_QOperator_wq   :: ���ڥ��ȥ�ǡ����˱黻��
  !                      Q=(k����-1/2(L2 k����+ k����L2)) ����Ѥ�����
  ! wr_RadRot_xyr_xyr :: �٥��ȥ� v �α��٤�ư�¥٥��ȥ� r ������ r��(����v) ��
  !                      �׻�����
  ! wr_RadRotRot_xyr_xyr_xyr :: �٥��ȥ�� v �� r��(���ߢ���v) ��׻�����
  ! wq_RadRotRot_xyr_xyr_xyr :: �٥��ȥ�� v �� r��(���ߢ���v) ��׻�����
  ! wq_Potential2Vector      :: �ȥ�����ݥ�����ݥƥ󥷥�뤫��
  !                             �٥��ȥ���׻�����
  ! wq_Potential2Rotation    :: �ȥ�����ݥ�����ݥƥ󥷥���ɽ�����
  !                             ��ȯ���٥��ȥ��β�ž�γ���ʬ��׻�����
  !
  !==== �ݥ�����/�ȥ������ǥ��ѥ��ڥ��ȥ����
  !
  ! nmr_ToroidalEnergySpectrum_wq, nr_ToroidalEnergySpectrum_wq  ::
  !     �ȥ�����ݥƥ󥷥�뤫�饨�ͥ륮���ε���Ĵ��ȡ������ʬ��׻�����
  ! nmr_PoloidalEnergySpectrum_wq, nr_PoloidalEnergySpectrum_wq  ::
  !     �ݥ�����ݥƥ󥷥�뤫�饨�ͥ륮���ε���Ĵ��ȡ������ʬ��׻�����
  !
  !==== ����������
  !
  ! wq_BoundaryTau, wr_BoundaryGrid, wq_Boundary                         ::
  !     �ǥ��ꥯ��, �Υ��ޥ󶭳�����Ŭ�Ѥ���(����ˡ, ����ˡ)
  ! wq_TorBoundaryTau, wr_TorBoundaryGrid, wq_TorBoundary                ::
  !     ®�٥ȥ�����ݥƥ󥷥��ζ�������Ŭ�Ѥ���(����ˡ,����ˡ)       
  ! wq_LaplaPol2Pol_wq, wq_LaplaPol2PolTau_wq                            ::
  !     ®�٥ݥ�����ݥƥ󥷥�릵��^2���������
  !     (�����Ϥ����줾��ʻ�������ӥ��ڥ��ȥ뷸��)
  ! wq_TorMagBoundaryTau, wr_TorMagBoundaryGrid, wq_TorMagBoundary       ::
  !     ����ȥ�����ݥƥ󥷥��ζ�������Ŭ�Ѥ���(����ˡ, ����ˡ)
  ! wq_PolMagBoundaryTau, wr_PolMagBoundaryGrid, wq_PolMagBoundary       ::
  !     ����ȥ�����ݥƥ󥷥�붭���ζ�������Ŭ�Ѥ���(����ˡ, ����ˡ)
  !
  !==== ��ʬ��ʿ��(3 �����ǡ���)
  !
  ! IntLonLatRad_xyr, AvrLonLatRad_xyr :: 3 �����ʻ����ǡ�����
  !                                       ���ΰ���ʬ�����ʿ��
  ! r_IntLonLat_xyr, r_AvrLonLat_xyr   :: 3 �����ʻ����ǡ�����
  !                                       ���ٷ���(��ʿ������)��ʬ�����ʿ��
  ! y_IntLonRad_xyr, y_AvrLonRad_xyr   :: 3 �����ʻ����ǡ�����
  !                                       ����ư����ʬ�����ʿ��
  ! r_IntLatRad_xyr, r_AvrLatRad_xyr   :: 3 �����ʻ����ǡ�����
  !                                       ����ư��(�Ҹ���)��ʬ�����ʿ��
  ! yr_IntLon_xyr, yr_AvrLon_xyr       :: 3 �����ʻ����ǡ�����
  !                                       ����������ʬ�����ʿ��
  ! xr_IntLat_xyr, xr_AvrLat_xyr       :: 3 �����ʻ����ǡ�����
  !                                       ����������ʬ�����ʿ��
  ! xr_IntRad_xyr, xr_AvrRad_xyr       :: 3 �����ʻ����ǡ�����
  !                                       ư��������ʬ�����ʿ��
  !
  !==== ��ʬ��ʿ��(2 �����ǡ���)
  !
  ! IntLonLat_xy, AvrLonLat_xy :: 2 �����ʻ����ǡ����ο�ʿ(����)��ʬ�����ʿ��
  ! IntLonRad_xr, AvrLonRad_xr :: 2 ����(XZ)�ʻ����ǡ����η���ư����ʬ
  !                               �����ʿ��
  ! IntLatRad_yr, AvrLatRad_yr :: 2 ����(YZ)�ʻ����ǡ����ΰ���ư��(�Ҹ���)
  !                               ��ʬ�����ʿ�� 
  ! y_IntLon_xy, y_AvrLon_xy   :: ��ʿ 2 ����(����)�ʻ����ǡ����η�������
  !                               ��ʬ�����ʿ��
  ! x_IntLat_xy, x_AvrLat_xy   :: ��ʿ2 ����(����)�ʻ����ǡ����ΰ���������ʬ
  !                               �����ʿ��
  ! r_IntLon_xr, r_AvrLon_xr   :: 2 ����(XZ)�ʻ����ǡ����η���������ʬ�����
  !                               ʿ��
  ! x_IntRad_xr, x_AvrRad_xr   :: 2 ����(XZ)�ʻ����ǡ�����ư��������ʬ�����
  !                               ʿ��
  ! r_IntLat_yr, r_AvrLat_yr   :: 2 ����(YZ)�ʻ����ǡ����ΰ���������ʬ�����
  !                               ʿ��
  ! y_IntRad_yr, y_AvrRad_yr   :: 2 ����(YZ)�ʻ����ǡ�����ư��������ʬ�����
  !                               ʿ��                  
  !
  !==== ��ʬ��ʿ��(1 �����ǡ���)
  !
  ! IntLon_x, AvrLon_x  :: 1 ����(X)�ʻ����ǡ����η���������ʬ�����ʿ��
  ! IntLat_y, AvrLat_y  :: 1 ����(Y)�ʻ����ǡ����ΰ���������ʬ�����ʿ��
  ! IntRad_r, AvrRad_r  :: 1 ����(Z)�ʻ����ǡ�����ư��������ʬ�����ʿ��
  !
  ! 
  use dc_message
  use lumatrix
  use wa_module_sjpack
  use aq_module, r_Rad => g_R, r_RAD_WEIGHT => g_R_WEIGHT, &
                 aq_ar => aq_ag, ar_aq => ag_aq, &
                 q_RadDRad_q => q_rDr_q, wq_RadDRad_wq => aq_rDr_aq, &
                 wq_Rad2_wq => aq_r2_aq, q_Rad2_q => q_r2_q, &
                 wq_Rad2Inv_wq => aq_r2Inv_aq, q_Rad2Inv_q => q_r2Inv_q
  implicit none
  private

  public wq_Initial

  public x_Lon, x_Lon_Weight
  public y_Lat, y_Lat_Weight
  public r_Rad, r_Rad_Weight
  public l_nm, nm_l
  public xy_Lon, xy_Lat
  public xyr_Lon, xyr_Lat, xyr_Rad
  public wr_Rad
  public wq_VMiss

  public w_xy, xy_w
  public wq_RadDRad_wq, q_RadDRad_q, wr_wq, wq_wr
  public wq_Rad2_wq, q_Rad2_q, wq_Rad2Inv_wq, q_Rad2Inv_q
  public xyr_wq, wq_xyr, xyr_wr, wr_xyr
  public wr_DivRad_wq, wr_RotDRad_wq, wr_RotDRad_wr, wq_Lapla_wq
  public wq_RotDRad_wr
  public xyr_GradLon_wq, xyr_GradLat_wq
  public wr_DivLon_xyr, wr_DivLat_xyr
  public wr_Div_xyr_xyr_xyr, xyr_Div_xyr_xyr_xyr
  public xyr_RotLon_wq_wq, xyr_RotLat_wq_wq, wr_RotRad_xyr_xyr

  public yr_IntLon_xyr, xr_IntLat_xyr, xy_IntRad_xyr
  public x_IntLatRad_xyr, y_IntLonRad_xyr, r_IntLonLat_xyr
  public IntLonLatRad_xyr

  public x_IntLat_xy, y_IntLon_xy, IntLonLat_xy
  public r_IntLat_yr, y_IntRad_yr, IntLatRad_yr
  public r_IntLon_xr, x_IntRad_xr, IntLonRad_xr
  public IntLon_x, IntLat_y, IntRad_r

  public yr_AvrLon_xyr, xr_AvrLat_xyr, xy_AvrRad_xyr
  public x_AvrLatRad_xyr, y_AvrLonRad_xyr, r_AvrLonLat_xyr
  public AvrLonLatRad_xyr

  public x_AvrLat_xy, y_AvrLon_xy, AvrLonLat_xy
  public r_AvrLat_yr, y_AvrRad_yr, AvrLatRad_yr
  public r_AvrLon_xr, x_AvrRad_xr, AvrLonRad_xr
  public AvrLon_x, AvrLat_y, AvrRad_r

  public wq_KxRGrad_wq, xyr_KGrad_wq, wq_L2_wq, wq_L2Inv_wq, wq_QOperator_wq
  public wr_RadRot_xyr_xyr, wr_RadRotRot_xyr_xyr_xyr
  public wq_RadRotRot_xyr_xyr_xyr
  public wq_Potential2vector, wq_Potential2Rotation

  public nmr_ToroidalEnergySpectrum_wq, nr_ToroidalEnergySpectrum_wq
  public nmr_PoloidalEnergySpectrum_wq, nr_PoloidalEnergySpectrum_wq

  public wq_Boundary, wq_TorBoundary, wq_LaplaPol2Pol_wq ! wr_LaplaPol2Pol_wr
  public wq_TormagBoundary, wq_PolmagBoundary

  public wq_BoundaryTau, wq_TorBoundaryTau, wq_LaplaPol2PolTau_wq
  public wq_TormagBoundaryTau, wq_PolmagBoundaryTau

  public wr_BoundaryGrid, wr_TorBoundaryGrid
  public wr_TormagBoundaryGrid, wr_PolmagBoundaryGrid

  interface wq_Boundary
     module procedure wq_BoundaryTau
  end interface

  interface wq_TorBoundary
     module procedure wq_TorBoundaryTau
  end interface

  interface wq_LaplaPol2Pol_wq
     module procedure wq_LaplaPol2PolTau_wq
  end interface

  interface wq_TorMagBoundary
     module procedure wq_TorMagBoundaryTau
  end interface

  interface wq_PolMagBoundary
     module procedure wq_PolMagBoundaryTau
  end interface

  integer            :: im=64, jm=32, km=16  ! �ʻ���������(����, ����, ư��)
  integer            :: nm=21, lm=31         ! �����ȿ�������(��ʿ, ư��)
  real(8)            :: ra=1.0               ! ��Ⱦ��
  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), parameter :: alpha = 1.0D0      ! Ÿ��¿�༰�ѥ�᥿��  0 < �� <= 1
  real(8), parameter :: beta  = 2.0D0      ! Ÿ��¿�༰�ѥ�᥿��  0 < ��

  real(8), dimension(:,:,:), allocatable :: xyr_LON, xyr_LAT, xyr_RAD ! ��ɸ
  real(8), dimension(:,:), allocatable   :: wr_RAD                    ! ��ɸ
  integer, dimension(:), allocatable     :: nd             ! �Ť� r^n �λؿ�

  real(8) :: wq_VMiss = -999.0        ! ��»��

  save im, jm, km, nm, lm, ra, nd, xyr_Lon, xyr_Lat, xyr_Rad, wr_Rad

  contains
  !--------------- ����� -----------------
   subroutine wq_Initial(i,j,k,n,l,r,np,wa_init)
     !
     ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, ư�º�ɸ���ϰϤ����ꤹ��.
     !
     ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ�ǽ�������
     ! ���ʤ���Фʤ�ʤ�. 
     !
     ! np �� 1 ����礭���ͤ���ꤹ��� ISPACK �ε���Ĵ��ȡ���Ѵ� 
     ! OPENMP ����׻��롼�����Ѥ�����. ����׻���¹Ԥ���ˤ�, 
     ! �¹Ի��˴Ķ��ѿ� OMP_NUM_THREADS �� np �ʲ��ο��������ꤹ������
     ! �����ƥ�˱�����������ɬ�פȤʤ�. 
     !
     ! np �� 1 ����礭���ͤ���ꤷ�ʤ��������׻��롼����ϸƤФ�ʤ�.
     !
     !
     integer,intent(in) :: i              ! �ʻ�����(���٦�)
     integer,intent(in) :: j              ! �ʻ�����(���٦�)
     integer,intent(in) :: k              ! �ʻ�����(ư�� r)
     integer,intent(in) :: n              ! �����ȿ�(��ʿ���ȿ�)
     integer,intent(in) :: l              ! �����ȿ�(ư���ȿ�)

     real(8),intent(in) :: r              ! ��Ⱦ��

     integer,intent(in), optional :: np   ! OPENMP �Ǥκ��祹��åɿ�
     logical,intent(in), optional :: wa_init   ! wa_initial �����å�

     logical    :: wa_initialize=.true.   ! wa_initial �����å�

     integer :: nn, mm

     im = i  ; jm = j ; km = k
     nm = n  ; lm = l
     ra = r

     if ( present(wa_init) ) then
        wa_initialize = wa_init
     else
        wa_initialize = .true.
     endif

     if ( wa_initialize ) then
        if ( present(np) ) then
           call wa_Initial(nm,im,jm,km,np)
        else
           call wa_Initial(nm,im,jm,km)
        endif
     endif

     allocate(nd((nm+1)*(nm+1)))

     do nn=0,nm
        do mm=-nn,nn
           nd(l_nm(nn,mm)) = nn                 ! ��ʿ n ���� r^n ����
        enddo
     enddo

     call aq_Initial(km,lm,ra,alpha,beta,nd)

     allocate(xyr_Lon(0:im-1,1:jm,km))
     allocate(xyr_Lat(0:im-1,1:jm,km))
     allocate(xyr_Rad(0:im-1,1:jm,km))

     allocate(wr_Rad((nm+1)*(nm+1),km))

     xyr_Lon = spread(xy_Lon,3,km)
     xyr_Lat = spread(xy_Lat,3,km)
     xyr_Rad = spread(spread(r_Rad,1,jm),1,im)

     wr_Rad = spread(r_Rad,1,(nm+1)*(nm+1))

    call MessageNotify('M','wq_initial','wq_module_sjpack (2009/12/06) is initialized')

   end subroutine wq_initial

  !--------------- �����Ѵ� -----------------

    function xyr_wq(wq)
      !
      ! ���ڥ��ȥ�ǡ������� 3 �����ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:im-1,1:jm,km)                       :: xyr_wq
      !(out) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      xyr_wq = xya_wa(wr_wq(wq))

    end function xyr_wq

    function wq_xyr(xyr)
      !
      ! 3 �����ʻ����ǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wq_xyr
      !(out) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,1:jm,km), intent(in)           :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      wq_xyr = wq_wr(wa_xya(xyr))

    end function wq_xyr

    function xyr_wr(wr)
      !
      ! ��ʿ���ڥ��ȥ롦ư�³ʻ����ǡ������� 3 �����ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:im-1,1:jm,km)                     :: xyr_wr
      !(out) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension((nm+1)*(nm+1),km), intent(in) :: wr
      !(in) 2 ��������Ĵ��ȡ�����ڥ��ȥ롦ư�³ʻ����ǡ���

      xyr_wr = xya_wa(wr)

    end function xyr_wr

    function wr_xyr(xyr)
      !
      ! 3 �����ʻҥǡ��������ʿ���ڥ��ȥ롦ư�³ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension((nm+1)*(nm+1),km)               :: wr_xyr
      !(out) 2 ��������Ĵ��ȡ�����ڥ��ȥ롦ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,1:jm,km), intent(in)         :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      wr_xyr = wa_xya(xyr)

    end function wr_xyr

    function wr_wq(wq)
      !
      ! ���ڥ��ȥ�ǡ��������ʿ���ڥ��ȥ롦ư�³ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension((nm+1)*(nm+1),km) :: wr_wq
      !(out) 2 ��������Ĵ��ȡ�����ڥ��ȥ롦ư�³ʻ����ǡ���
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      wr_wq = ar_aq(wq)

    end function wr_wq

    function wq_wr(wr)
      !
      ! ��ʿ���ڥ��ȥ롦ư�³ʻ����ǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm) :: wq_wr
      !(out) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���
      real(8), dimension((nm+1)*(nm+1),km), intent(in) :: wr
      !(in) 2 ��������Ĵ��ȡ�����ڥ��ȥ롦ư�³ʻ����ǡ���

      wq_wr = aq_ar(wr)
      
    end function wq_wr


  !--------------- ��ʬ�׻� -----------------
    function wr_DivRad_wq(wq)
      ! 
      ! ���ϥ��ڥ��ȥ�ǡ�����ȯ����ư����ʬ
      !
      !       1/r^2 ��/��r (r^2 .)= ��/��r + 2/r
      !
      ! ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ�����ȯ����ư����ʬ�Ȥ�, �б�����ʻ����ǡ�����
      ! ȯ����ư����ʬ����Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),km)             :: wr_DivRad_wq
      !(out) ȯ����ư����ʬ����Ѥ��줿��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���

      wr_DivRad_wq = wr_wq(wq_RadDRad_wq(wq))/wr_Rad + 2/wr_Rad * wr_wq(wq)

    end function wr_DivRad_wq

    function wr_RotDRad_wq(wq)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˲�ž��ư����ʬ
      !
      !      1/r ��(r.)/��r = ��(.)/��r + (.)/r
      !
      ! ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����β�ž��ư����ʬ�Ȥ�, �б�����ʻ����ǡ�����
      ! ��ž��ư����ʬ����Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),km)               :: wr_RotDRad_wq
      !(out) ��ž��ư����ʬ����Ѥ��줿��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���

      wr_RotDRad_wq = wr_wq(wq_RadDrad_wq(wq))/wr_Rad + wr_wq(wq)/wr_Rad

    end function wr_RotDRad_wq

    function wr_RotDRad_wr(wr)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˲�ž��ư����ʬ
      !
      !      1/r ��(r.)/��r = ��(.)/��r + (.)/r
      !
      ! ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����β�ž��ư����ʬ�Ȥ�, �б�����ʻ����ǡ�����
      ! ��ž��ư����ʬ����Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), dimension((nm+1)*(nm+1),km), intent(in) :: wr
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),km)               :: wr_RotDRad_wr
      !(out) ��ž��ư����ʬ����Ѥ��줿��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���

      wr_RotDRad_wr = wr_wq(wq_RadDRad_wq(wq_wr(wr*wr_Rad)))/wr_Rad**2

    end function wr_RotDRad_wr

    function wq_RotDRad_wr(wr)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˲�ž��ư����ʬ
      !
      !      1/r ��(r.)/��r = ��(.)/��r + (.)/r
      !
      ! ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����β�ž��ư����ʬ�Ȥ�, �б�����ʻ����ǡ�����
      ! ��ž��ư����ʬ����Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), dimension((nm+1)*(nm+1),km), intent(in) :: wr
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm)               :: wq_RotDRad_wr
      !(out) ��ž��ư����ʬ����Ѥ��줿��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���

      wq_RotDRad_wr = wq_Rad2Inv_wq(wq_RadDRad_wq(wq_wr(wr*wr_Rad)))

    end function wq_RotDRad_wr

    function wq_Lapla_wq(wq)
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷����
      !
      !     ��^2 =   1/r^2 cos^2�ա���^2/�ߦ�^2 
      !            + 1/r^2 cos�ա���/�ߦ�(cos�բ�/�ߦ�) 
      !            + 1/r^2 ��/��r (r^2 ��/��r) 
      !              [1/r^2 (r��/��r)(r��/��r) + 1/r^2(r��/��r)]
      !
      ! ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wq_Lapla_wq
      !(out) ��ץ饷�������Ѥ��줿��ʿ���ڥ��ȥ�ǡ���

      wq_Lapla_wq = wq_Rad2Inv_wq(  wq_RadDRad_wq(wq_RadDRad_wq(wq)) &
                                  + wq_RadDRad_wq(wq)+ wa_Lapla_wa(wq) )

    end function wq_Lapla_wq

    function xyr_GradLon_wq(wq)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/rcos�ա���/�ߦ�
      ! ����Ѥ�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,1:jm,km)    :: xyr_GradLon_wq
      !(out) ���۷�������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      xyr_GradLon_wq = xya_GradLon_wa(wr_wq(wq))/xyr_Rad

    end function xyr_GradLon_wq

    function xyr_GradLat_wq(wq) 
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/r ��/�ߦ� ����Ѥ�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,1:jm,km)    :: xyr_GradLat_wq
      !(out) ���۷�������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      xyr_GradLat_wq = xya_GradLat_wa(wr_wq(wq))/xyr_Rad

    end function xyr_GradLat_wq

    function wr_DivLon_xyr(xyr)
      ! 
      ! �ʻ����ǡ�����ȯ����������ʬ 1/rcos�ա���/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������֤�.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in)     :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension((nm+1)*(nm+1),km)         :: wr_DivLon_xyr
      !(out) ȯ����������ʬ����Ѥ��줿��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���

      wr_DivLon_xyr = wa_DivLon_xya(xyr/xyr_Rad)

    end function wr_DivLon_xyr

    function wr_DivLat_xyr(xyr)
      !
      ! �ʻҥǡ�����ȯ����������ʬ 1/rcos�ա���(f cos��)/�ߦ� ��
      ! ���Ѥ��������ڥ��ȥ�ǡ������֤�.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in)     :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension((nm+1)*(nm+1),km)         :: wr_DivLat_xyr
      !(out) ȯ����������ʬ����Ѥ��줿��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���

      wr_DivLat_xyr = wa_DivLat_xya(xyr/xyr_Rad)

    end function wr_DivLat_xyr

    function wr_Div_xyr_xyr_xyr(xyr_Vlon,xyr_Vlat,xyr_Vrad)
      !
      ! �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����ȯ������Ѥ�����
      ! ���ڥ��ȥ�ǡ������֤�.
      !
      ! �� 1, 2 ,3 ����(u,v,w)�����줾��٥��ȥ�η�����ʬ, ������ʬ, 
      ! ư����ʬ��ɽ��, ȯ���� 
      !
      !      1/rcos�ա���u/�ߦ� + 1/rcos�ա���(v cos��)/�ߦ� 
      !    + 1/r^2 ��/��r (r^2 w)
      !
      ! �ȷ׻������.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension((nm+1)*(nm+1),km)     :: wr_Div_xyr_xyr_xyr
      !(out) �٥��ȥ���ȯ��

      wr_Div_xyr_xyr_xyr =   wr_DivLon_xyr(xyr_Vlon) &
                           + wr_DivLat_xyr(xyr_Vlat) &
                           + wr_DivRad_wq(wq_xyr(xyr_Vrad))

    end function wr_Div_xyr_xyr_xyr

    function xyr_Div_xyr_xyr_xyr(xyr_Vlon,xyr_Vlat,xyr_Vrad)
      !
      ! �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����ȯ������Ѥ�����.
      !
      ! �� 1, 2 ,3 ����(u,v,w)�����줾��٥��ȥ�η�����ʬ, ������ʬ, 
      ! ư����ʬ��ɽ��.
      !
      ! �ˤ��ð�������򤹤뤿��˥٥��ȥ��� cos��/r �νŤߤ򤫤���
      ! �׻����Ƥ���. 
      !
      !      div V = (r/cos��)��div (Vcos��/r) + V_��tan��/r + V_r/r
      ! 
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension(0:im-1,1:jm,km)             :: xyr_Div_xyr_xyr_xyr
      !(out) �٥��ȥ���ȯ��

      xyr_Div_xyr_xyr_xyr &
           = xyr_Rad/cos(xyr_Lat) &
                * xyr_wr(wr_Div_xyr_xyr_xyr(xyr_VLon*cos(xyr_Lat)/xyr_Rad,  &
                                            xyr_VLat*cos(xyr_Lat)/xyr_Rad,  &
                                            xyr_VRad*cos(xyr_Lat)/xyr_Rad ))&
             + xyr_VLat*tan(xyr_Lat)/xyr_Rad &
             + xyr_VRad/xyr_Rad

    end function xyr_Div_xyr_xyr_xyr

    function xyr_RotLon_wq_wq(wq_Vrad,wq_Vlat) 
      !
      ! �٥��ȥ���ư����ʬ, ������ʬ�Ǥ����� 1, 2 ���� Vrad, Vlat ����
      ! ��ž�η�����ʬ 
      !
      !    1/r ��Vrad/�ߦ�-1/r ��(r Vlat)/��r ��׻�����.
      !
      ! ��׻�����
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,km)                 :: xyr_RotLon_wq_wq
      !(out) �٥��ȥ��β�ž�η�����ʬ

        xyr_RotLon_wq_wq =   xyr_GradLat_wq(wq_Vrad) &
                           - xyr_wr(wr_RotDRad_wq(wq_Vlat))

    end function xyr_RotLon_wq_wq

    function xyr_RotLat_wq_wq(wq_Vlon,wq_Vrad) 
      !
      ! �٥��ȥ��η�����ʬ, ư����ʬ�Ǥ����� 1, 2 ���� Vlon, Vrad ����
      ! ��ž�ΰ�����ʬ 
      !
      !    1/r ��(r Vlon)/��r - 1/rcos�ա���Vrad/�ߦ�
      !
      ! ��׻�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension(0:im-1,1:jm,km)                 :: xyr_RotLat_wq_wq
      !(out) �٥��ȥ��β�ž�ΰ�����ʬ

        xyr_RotLat_wq_wq =   xyr_wr(wr_RotDRad_wq(wq_Vlon)) &
                           - xyr_GradLon_wq(wq_Vrad) 

    end function xyr_RotLat_wq_wq

    function wr_RotRad_xyr_xyr(xyr_Vlat,xyr_Vlon) 
      !
      ! �٥��ȥ�ΰ�����ʬ, ������ʬ�Ǥ����� 1, 2 ���� Vlat, Vlon ���Ф���
      ! �٥��ȥ��β�ž��ư����ʬ 
      !
      !    1/rcos�ա���Vlat/�ߦ� - 1/rcos�ա���(Vlon cos��)/�ߦ�
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension((nm+1)*(nm+1),km)       :: wr_RotRad_xyr_xyr
      !(out) �٥��ȥ��β�ž��ư����ʬ

        wr_RotRad_xyr_xyr =   wr_DivLon_xyr(xyr_Vlat) &
                            - wr_DivLat_xyr(xyr_Vlon)

    end function wr_RotRad_xyr_xyr

  !--------------- ��ʬ�׻� -----------------
    !----(���ϥǡ��� xyr)---
    function yr_IntLon_xyr(xyr)  ! ����(�Ӿ�)��ʬ
      !
      ! 3 �����ʻ����ǡ����η�������(�Ӿ�)��ʬ.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��� ��f(��,��,r)d�� ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(1:jm,km)  :: yr_IntLon_xyr
      !(out) ��������(�Ӿ�)��ʬ���줿 2 �����Ҹ��̳ʻ����ǡ���

      integer :: i

      yr_IntLon_xyr = 0.0d0
      do i=0,im-1
         yr_IntLon_xyr(:,:) = yr_IntLon_xyr(:,:) &
                       + xyr(i,:,:) * x_Lon_Weight(i)
      enddo
    end function yr_IntLon_xyr

    function xr_IntLat_xyr(xyr)
      !
      ! 3 �����ʻ����ǡ����ΰ�����������ʬ.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��Ƣ�f(��,��,r) cos�� d�� ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,km)  :: xr_IntLat_xyr        
      !(out) ������ʬ���줿 2 ��������ư�³ʻ����ǡ���
      ! ���ٱ߳ʻ����ǡ���

      integer :: j

      xr_IntLat_xyr = 0.0d0
      do j=1,jm
         xr_IntLat_xyr(:,:) = xr_IntLat_xyr(:,:) &
                       + xyr(:,j,:) * y_Lat_Weight(j)
      enddo

    end function xr_IntLat_xyr

    function xy_IntRad_xyr(xyr)  ! ư����ʬ
      !
      ! 3 �����ʻ����ǡ�����ư����������ʬ.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��Ƣ�f(��,��,r) r^2dr ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,1:jm)  :: xy_IntRad_xyr
      !(out) ư����ʬ���줿 2 �������ٰ���(��ʿ, ����)�ʻ����ǡ���

      integer :: k

      xy_IntRad_xyr = 0.0d0
      do k=1,km
         xy_IntRad_xyr(:,:) = xy_IntRad_xyr(:,:) &
                       + xyr(:,:,k) * r_Rad_Weight(k) 
      enddo

    end function xy_IntRad_xyr

    function x_IntLatRad_xyr(xyr)
      !
      ! 3 �����ʻ����ǡ����ΰ���ư��(�Ҹ���)��ʬ
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,��,r) r^2cos�� d��dr 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1)     :: x_IntLatRad_xyr
      !(out) ����ư��(�Ҹ���)��ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: j, k

      x_IntLatRad_xyr = 0.0D0
      do k=1,km
         do j=1,jm
            x_IntLatRad_xyr = x_IntLatRad_xyr &
                 + xyr(:,j,k) * y_Lat_Weight(j) * r_Rad_Weight(k)
         enddo
      enddo

    end function x_IntLatRad_xyr

    function y_IntLonRad_xyr(xyr)
      !
      ! 3 �����ʻ����ǡ����η���ư��(���ٱ�)��ʬ.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��Ƣ�f(��,��,r) r^2d��dr ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(1:jm)       :: y_IntLonRad_xyr
      !(out) ����ư��(���ٱ�)��ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: i, k

      y_IntLonRad_xyr = 0.0D0
      do k=1,km
         do i=0,im-1
            y_IntLonRad_xyr = y_IntLonRad_xyr &
                 + xyr(i,:,k) * x_Lon_Weight(i) * r_Rad_Weight(k)
         enddo
      enddo

    end function y_IntLonRad_xyr

    function r_IntLonLat_xyr(xyr)  ! ���ٷ���(��ʿ)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ��ٷ���(��ʿ, ����)��ʬ
      ! 
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,��,r) cos�� d��d�� 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(km)     :: r_IntLonLat_xyr
      !(out) ���ٷ���(��ʿ, ����)��ʬ���줿 1 ����ư�³ʻ����ǡ���

      integer :: i, j

      r_IntLonLat_xyr = 0.0D0
      do j=1,jm
         do i=0,im-1
            r_IntLonLat_xyr = r_IntLonLat_xyr &
                 + xyr(i,j,:) * x_Lon_Weight(i) * y_Lat_Weight(j)
         enddo
      enddo
    end function r_IntLonLat_xyr

    function IntLonLatRad_xyr(xyr) ! ���ٷ���ư��(����)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ��ٷ���ư��(����)��ʬ
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !     ��f(��,��,r) r^2cos�� d��d��dr 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr 
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8)                     :: IntLonLatRad_xyr 
      !(out) ������ʬ��

      integer :: i, j, k

      IntLonLatRad_xyr = 0.0D0
      do k=1,km
         do j=1,jm
            do i=0,im-1
               IntLonLatRad_xyr = IntLonLatRad_xyr &
                    + xyr(i,j,k) * x_Lon_Weight(i) &
                         * y_Lat_Weight(j) * r_Rad_Weight(k)
            enddo
         enddo
      enddo

    end function IntLonLatRad_xyr

    !----(���ϥǡ��� yr)---
    function r_IntLat_yr(yr)  ! ������ʬ
      !
      ! 2 ����(YR)�ʻ����ǡ����ΰ�����������ʬ.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��Ƣ�f(��,r) cos�� d�� ��׻�����.
      !
      real(8), dimension(1:jm,km), intent(in) :: yr
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(km)  :: r_IntLat_yr
      !(out) ������ʬ���줿 1 ����ư�³ʻ����ǡ���

      integer :: j

      r_IntLat_yr = 0.0d0
      do j=1,jm
         r_IntLat_yr(:) = r_IntLat_yr(:) + yr(j,:) * y_Lat_Weight(j)
      enddo

    end function r_IntLat_yr

    function y_IntRad_yr(yr)  ! ư����ʬ
      !
      ! 2 ����(YR)�ʻ����ǡ�����ư����������ʬ.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��Ƣ�f(��,r) r^2dr ��׻�����.
      !
      real(8), dimension(1:jm,km), intent(in) :: yr
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(1:jm)  :: y_IntRad_yr
      !(out) ư����ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: k

      y_IntRad_yr = 0.0d0
      do k=1,km
         y_IntRad_yr(:) = y_IntRad_yr(:) &
                       + yr(:,k) * r_Rad_Weight(k) 
      enddo

    end function y_IntRad_yr

    function IntLatRad_yr(yr)
      !
      ! 2 ����(YR)�ʻ����ǡ����ΰ���ư����ʬ(�Ҹ���)�����ʿ��
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r) r^2cos�� d��dr ��׻�����.
      !
      real(8), dimension(1:jm,km), intent(in) :: yr
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8)                   :: IntLatRad_yr
      !(out) ��ʬ��
      integer :: j, k

      IntLatRad_yr = 0.0D0
      do k=1,km
         do j=1,jm
            IntLatRad_yr = IntLatRad_yr &
                 + yr(j,k) * y_Lat_Weight(j) * r_Rad_Weight(k)
         enddo
      enddo

    end function IntLatRad_yr

    !----(���ϥǡ��� xr)---
    function r_IntLon_xr(xr)
      !
      ! 2 ����(XR)�ʻ����ǡ����η���������ʬ.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r)d�� ��׻�����.
      !
      real(8), dimension(0:im-1,km), intent(in) :: xr
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8), dimension(km)  :: r_IntLon_xr
      !(out) ������ʬ���줿 1 ����ư�³ʻ����ǡ���

      integer :: i

      r_IntLon_xr = 0.0d0
      do i=0,im-1
         r_IntLon_xr(:) = r_IntLon_xr(:) + xr(i,:) * x_Lon_Weight(i)
      enddo

    end function r_IntLon_xr

    function x_IntRad_xr(xr)
      !
      ! 2 ����(XR)�ʻ����ǡ�����ư����������ʬ.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r) r^2dr ��׻�����.
      !
      real(8), dimension(0:im-1,km), intent(in) :: xr
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8), dimension(0:im-1)  :: x_IntRad_xr
      !(out) ư����ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: k

      x_IntRad_xr = 0.0d0
      do k=1,km
         x_IntRad_xr(:) = x_IntRad_xr(:) &
                       + xr(:,k) * r_Rad_Weight(k) 
      enddo

    end function x_IntRad_xr

    function IntLonRad_xr(xr)  ! ����ư��(���ٱ�)��ʬ
      !
      ! 2 ����(XR)�ʻ����ǡ����η���ư����ʬ
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��Ƣ�f(��,r) r^2d��dr ��׻�����.
      !
      real(8), dimension(0:im-1,km), intent(in) :: xr
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8)                                 :: IntLonRad_xr
      !(out) ��ʬ��

      integer :: i, k

      IntLonRad_xr = 0.0D0
      do k=1,km
         do i=0,im-1
            IntLonRad_xr = IntLonRad_xr &
                 + xr(i,k) * x_Lon_Weight(i) * r_Rad_Weight(k)
         enddo
      enddo

    end function IntLonRad_xr

    !----(���ϥǡ��� z)---
    function IntRad_r(z)  ! ư����ʬ
      !
      ! 1 ����(Z)�ʻ����ǡ�����ư����������ʬ.
      !
      ! 1 �����ǡ��� f(r) ���Ф��� ��f(r) r^2dr ��׻�����.
      !
      real(8), dimension(km), intent(in) :: z
      !(in) 1 ����ư�³ʻ����ǡ���

      real(8)                              :: IntRad_r
      !(out) ��ʬ��

      integer :: k

      IntRad_r = 0.0d0
      do k=1,km
         IntRad_r = IntRad_r + z(k) * r_Rad_Weight(k) 
      enddo

    end function IntRad_r

  !--------------- ʿ�ѷ׻� -----------------
    !----(���ϥǡ��� xyr)---
    function yr_AvrLon_xyr(xyr)  ! ����(�Ӿ�)��ʬ
      !
      ! 3 �����ʻ����ǡ����η�������(�Ӿ�)ʿ��.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��� ��f(��,��,r)d��/2�� ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(1:jm,km)  :: yr_AvrLon_xyr
      !(out) ��������(�Ӿ�)ʿ�Ѥ��줿 2 �����Ҹ��̳ʻ����ǡ���

      yr_AvrLon_xyr = yr_IntLon_xyr(xyr)/sum(x_Lon_Weight)

    end function yr_AvrLon_xyr

    function xr_AvrLat_xyr(xyr)  ! ������ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ���������ʿ��.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��� ��f(��,��,r)cos�� d��/2 ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,km)  :: xr_AvrLat_xyr
      !(out) ����ʿ�Ѥ��줿 2 ��������ư�³ʻ����ǡ���

      xr_AvrLat_xyr = xr_IntLat_xyr(xyr)/sum(y_Lat_Weight)

    end function xr_AvrLat_xyr

    function xy_AvrRad_xyr(xyr)
      !
      ! 3 �����ʻ����ǡ�����ư��������ʿ��.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��� 
      !
      !    ��f(��,��,r) r^2dr/((r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,1:jm)  :: xy_AvrRad_xyr          
      !(out) ư��ʿ�Ѥ��줿 2 �������ٰ���(��ʿ, ����)�ʻ����ǡ���
      ! ��ʿ�ʻ����ǡ���

      xy_AvrRad_xyr = xy_IntRad_xyr(xyr)/sum(r_Rad_Weight)

    end function xy_AvrRad_xyr

    function x_AvrLatRad_xyr(xyr)  ! ����ư��(�Ҹ���)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ���ư��(�Ҹ���)ʿ��
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,,r) r^2cos�� d��dr /(2(r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1)     :: x_AvrLatRad_xyr
      !(out) ����ư��(�Ҹ���)ʿ�Ѥ��줿 1 �������ٳʻ����ǡ���

      x_AvrLatRad_xyr = x_IntLatRad_xyr(xyr) &
                   /( sum(y_Lat_Weight)*sum(r_Rad_Weight) )

    end function x_AvrLatRad_xyr

    function y_AvrLonRad_xyr(xyr)  ! ����ư��(���ٱ�)��ʬ
      !
      ! 3 �����ʻ����ǡ����η���ư��(���ٱ�)ʿ��.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !     ��f(��,��,r) r^2d��dr /(2��(r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(1:jm)       :: y_AvrLonRad_xyr
      !(out) ����ư��(���ٱ�)ʿ�Ѥ��줿 1 �������ٳʻ����ǡ���

      y_AvrLonRad_xyr = y_IntLonRad_xyr(xyr) &
                 /(sum(x_Lon_Weight)*sum(r_Rad_Weight))

    end function y_AvrLonRad_xyr

    function r_AvrLonLat_xyr(xyr)  ! ���ٷ���(��ʿ)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ��ٷ���(��ʿ, ����)��ʬ
      ! 
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,��,r) cos�� d��d�� /4�� 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(km)     :: r_AvrLonLat_xyr
      !(out) ���ٷ���(��ʿ, ����)ʿ�Ѥ��줿 1 ����ư�³ʻ����ǡ���

      r_AvrLonLat_xyr = r_IntLonLat_xyr(xyr) &
                 /(sum(x_Lon_Weight)*sum(y_Lat_Weight))

    end function r_AvrLonLat_xyr

    function AvrLonLatRad_xyr(xyr) ! ���ٷ���ư��(����)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ��ٷ���ư��(����)��ʬ
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,��,r) r^2cos�� d��d��dr /(4��(r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8)                     :: AvrLonLatRad_xyr
      !(out) ����ʿ����

      AvrLonLatRad_xyr = IntLonLatRad_xyr(xyr) &
            /(sum(x_Lon_Weight)*sum(y_Lat_Weight) * sum(r_Rad_Weight))

    end function AvrLonLatRad_xyr

    !----(���ϥǡ��� yr)---
    function r_AvrLat_yr(yr)
      !
      ! 2 ����(YR)�ʻ����ǡ����ΰ���������ʿ��.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r) cos�� d��/2 ��׻�����.
      !
      real(8), dimension(1:jm,km), intent(in) :: yr
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(km)  :: r_AvrLat_yr
      !(out) ����ʿ�Ѥ��줿 1 ����ư�³ʻ����ǡ���

      r_AvrLat_yr = r_IntLat_yr(yr)/sum(y_Lat_Weight)

    end function r_AvrLat_yr

    function y_AvrRad_yr(yr)
      !
      ! 2 ����(YR)�ʻ����ǡ�����ư��������ʿ��.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r) r^2dr /((r[o]^3-r[i]^3)/3) 
      ! ��׻�����.
      !
      real(8), dimension(1:jm,km), intent(in) :: yr
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(1:jm)  :: y_AvrRad_yr
      !(out) ư��ʿ�Ѥ��줿 1 �������ٳʻ����ǡ���

      y_AvrRad_yr = y_IntRad_yr(yr)/sum(r_Rad_Weight)

    end function y_AvrRad_yr

    function AvrLatRad_yr(yr)  ! ����ư��(�Ҹ���)��ʬ
      !
      ! 2 ����(YR)�ʻ����ǡ����ΰ���ư��(�Ҹ���)ʿ��
      !
      ! 2 �����ǡ��� f(��,r) ���Ф���
      !
      !    ��f(��,r) r^2cos�� d��dr /(2(r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(1:jm,km), intent(in) :: yr
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8)                   :: AvrLatRad_yr
      !(out) ʿ����

      AvrLatRad_yr = IntLatRad_yr(yr)/(sum(y_Lat_Weight)*sum(r_Rad_Weight))

    end function AvrLatRad_yr

    !----(���ϥǡ��� xr)---
    function r_AvrLon_xr(xr)  ! ����(�Ӿ�)��ʬ
      !
      ! 2 ����(XR)�ʻ����ǡ����η�������ʿ��.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r)d��/2�� ��׻�����.
      !
      real(8), dimension(0:im-1,km), intent(in) :: xr
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8), dimension(km)  :: r_AvrLon_xr 
      !(out) ����ʿ�Ѥ��줿 1 ����ư�³ʻ����ǡ���

      r_AvrLon_xr = r_IntLon_xr(xr)/sum(x_Lon_Weight)

    end function r_AvrLon_xr

    function x_AvrRad_xr(xr)  ! ư����ʬ
      !
      ! 2 ����(XR)�ʻ����ǡ�����ư��������ʿ��.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф���
      !
      !   ��f(��,r) r^2dr /((r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,km), intent(in) :: xr
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8), dimension(0:im-1)  :: x_AvrRad_xr
      !(out) ư��ʿ�Ѥ��줿 1 �������ٳʻ����ǡ���

      x_AvrRad_xr = x_IntRad_xr(xr)/sum(r_Rad_Weight)

    end function x_AvrRad_xr

    function AvrLonRad_xr(xr)  ! ����ư��(���ٱ�)��ʬ
      !
      ! 2 ����(XR)�ʻ����ǡ����η���ư��ʿ��
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� 
      ! 
      !    ��f(��,r) r^2d��dr /(2��(r[o]^3-r[i]^3)/3)
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,km), intent(in) :: xr
      ! (in) 2 �����ʻ����ǡ���
      real(8)                                 :: AvrLonRad_xr      
      ! ��ʬ��

      AvrLonRad_xr = IntLonRad_xr(xr)/(sum(x_Lon_Weight)*sum(r_Rad_Weight))

    end function AvrLonRad_xr

    !----(���ϥǡ��� z)---
    function AvrRad_r(z)
      !
      ! 1 ����(Z)�ʻ����ǡ�����ư��������ʿ��.
      !
      ! 1 �����ǡ��� f(r) ���Ф��� ��f(r) r^2dr /((r[o]^3-r[i]^3)/3) ��
      ! �׻�����.
      !
      real(8), dimension(0:im-1,km), intent(in) :: z
      !(in) 1 ����ư�³ʻ����ǡ���
      real(8)                                 :: AvrRad_r
      !(out) ʿ����

      AvrRad_r = IntRad_r(z)/sum(r_Rad_Weight)

    end function AvrRad_r

  !--------------- �ݥ�����/�ȥ������ǥ�����ʬ -----------------

    function wq_KxRGrad_wq(wq)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˷�����ʬ k��r���� = ��/�ߦˤ���Ѥ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wq_KxRGrad_wq
      !(out) ������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wq_KxRGrad_wq =  wa_Dlon_wa(wq)

    end function wq_KxRGrad_wq

    function xyr_KGrad_wq(wq)    ! k���� = cos��/r ��/�ߦ� + sin�բ�/��r
      !
      ! ���ϥ��ڥ��ȥ�ǡ������б�����ʻҥǡ����˼�������ʬ 
      !
      !    k���� = cos��/r ��/�ߦ� + sin�բ�/��r 
      !
      ! ����Ѥ������ʻҥǡ������֤����. 
      ! �����ǥ٥��ȥ� k �ϵ���濴�����̶˸�����ñ�̥٥��ȥ�Ǥ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,1:jm,km)                 :: xyr_KGrad_wq
      !(out) ��������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      xyr_KGrad_wq =  cos(xyr_Lat)*xyr_GradLat_wq(wq) &
                    + sin(xyr_Lat)*xyr_wq(wq_RadDRad_wq(wq))/xyr_Rad

    end function xyr_KGrad_wq

    function wq_L2_wq(wq)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� L^2 �黻��(=-��ʿ��ץ饷����)����Ѥ���.
      !
      ! L^2 �黻�Ҥ�ñ�̵��̾�ο�ʿ��ץ饷����ε����ˤ�����. 
      !  ���ϥ��ڥ��ȥ�� �������б�����ʻ����ǡ����˱黻�� 
      !
      !     L^2 = -1/cos^2�ա���^2/�ߦ�^2 - 1/cos�ա���/�ߦ�(cos�բ�/�ߦ�)
      !
      ! ����Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ����֤����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wq_L2_wq
      !(out) L^2 �黻�Ҥ���Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wq_L2_wq = -wa_Lapla_wa(wq)

    end function wq_L2_wq

    function wq_L2Inv_wq(wq)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� L^2 �黻�Ҥεձ黻(-�տ�ʿ��ץ饷����)��
      ! ���Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� L^2 �黻�Ҥ���Ѥ�����ؿ� wq_L2_wq �εշ׻���
      ! �Ԥ��ؿ��Ǥ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wq_L2Inv_wq
      !(out) L^2 �黻�Ҥεձ黻����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wq_L2Inv_wq = -wa_LaplaInv_wa(wq)

    end function wq_L2Inv_wq

    function wq_QOperator_wq(wq)
      !
      ! ���ϥ��ڥ��ȥ�ǡ������б�����ʻ����ǡ����˱黻�� 
      !
      !    Q=(k����-1/2(L2 k����+ k����L2)) 
      !
      ! ����Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ����֤����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wq_QOperator_wq
      !(out) Q �黻�Ҥ���Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wq_QOperator_wq = &
             wq_xyr(xyr_KGrad_wq(wq) - xyr_KGrad_wq(wq_L2_wq(wq))/2) &
           - wq_L2_wq(wq_xyr(xyr_KGrad_wq(wq)))/2

    end function wq_QOperator_wq

    function wr_RadRot_xyr_xyr(xyr_VLON,xyr_VLAT)  ! r��(����v)
      !
      ! �٥��ȥ�α��٤�ư�¥٥��ȥ������ r��(����v) ��׻�����.
      !
      ! �� 1, 2 ����(v[��], v[��])�����줾��٥��ȥ�η�����ʬ, ������ʬ��ɽ��.
      !
      !    r��(����v) = 1/cos�ա���v[��]/�ߦ� - 1/cos�ա���(v[��] cos��)/�ߦ�
      !
      ! �Υ��ڥ��ȥ� �ǡ������֤����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VLON
      !(in) �٥��ȥ�η�����ʬ

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VLAT
      !(in) �٥��ȥ�ΰ�����ʬ

      real(8), dimension((nm+1)*(nm+1),km)     :: wr_RadRot_xyr_xyr
      !(out) �٥��ȥ�α��٤�ư�¥٥��ȥ������

      wr_RadRot_xyr_xyr = wa_DivLon_xya(xyr_VLAT) - wa_DivLat_xya(xyr_VLON)
      
    end function wr_RadRot_xyr_xyr

    function wr_RadRotRot_xyr_xyr_xyr(xyr_VLON,xyr_VLAT,xyr_VRAD) 
      ! 
      ! �٥��ȥ� v ���Ф��� r��(���ߢ���v) ��׻�����.
      !
      ! �� 1, 2, 3 ����(v[��], v[��], v[r])�����줾��٥��ȥ�η�����ʬ, 
      ! ������ʬ, ư����ʬ��ɽ��. 
      !
      !    r��(���ߢ���v)  = 1/r ��/��r (r��( 1/cos�ա���v[��]/�ߦ� 
      !                                  + 1/cos�ա���(v[��] cos��)/�ߦ� ) ) 
      !                     + L^2 v[r]/r 
      !
      ! �Υ��ڥ��ȥ�ǡ������֤����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VLON
      !(in) �٥��ȥ�η�����ʬ

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VLAT
      !(in) �٥��ȥ�ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VRAD
      !(in) �٥��ȥ��ư����ʬ

      real(8), dimension((nm+1)*(nm+1),km)     :: wr_RadRotRot_xyr_xyr_xyr
      !(out) �٥��ȥ� v �� r��(���ߢ���v) 

      wr_RadRotRot_xyr_xyr_xyr = &
                   wr_RotDRad_wr( &
                      wa_DivLon_xya(xyr_VLON)+ wa_DivLat_xya(xyr_VLAT)) &
             - wa_Lapla_wa(wr_xyr(xyr_VRAD/xyr_RAD))

    end function wr_RadRotRot_xyr_xyr_xyr

    function wq_RadRotRot_xyr_xyr_xyr(xyr_VLON,xyr_VLAT,xyr_VRAD) 
      ! 
      ! �٥��ȥ� v ���Ф��� r��(���ߢ���v) ��׻�����.
      !
      ! �� 1, 2, 3 ����(v[��], v[��], v[r])�����줾��٥��ȥ�η�����ʬ, 
      ! ������ʬ, ư����ʬ��ɽ��. 
      !
      !    r��(���ߢ���v)  = 1/r ��/��r (r��( 1/cos�ա���v[��]/�ߦ� 
      !                                  + 1/cos�ա���(v[��] cos��)/�ߦ� ) ) 
      !                     + L^2 v[r]/r 
      !
      ! �Υ��ڥ��ȥ�ǡ������֤����.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VLON
      !(in) �٥��ȥ�η�����ʬ

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VLAT
      !(in) �٥��ȥ�ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VRAD
      !(in) �٥��ȥ��ư����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm)     :: wq_RadRotRot_xyr_xyr_xyr
      !(out) �٥��ȥ� v �� r��(���ߢ���v) 

      wq_RadRotRot_xyr_xyr_xyr = &
                   wq_RotDRad_wr( &
                      wa_DivLon_xya(xyr_VLON)+ wa_DivLat_xya(xyr_VLAT)) &
             - wa_Lapla_wa(wq_xyr(xyr_VRAD/xyr_RAD))

    end function wq_RadRotRot_xyr_xyr_xyr

    subroutine wq_Potential2Vector(&
         xyr_VLON,xyr_VLAT,xyr_VRAD,wq_TORPOT,wq_POLPOT)
      !
      ! �ȥ�����ݥ�����ݥƥ󥷥�릷,����ɽ�������ȯ���٥��ȥ��
      !
      !     v = ��x(��r) + ��x��x(��r) 
      !
      ! �γ���ʬ��׻�����
      !
      real(8), dimension(0:im-1,1:jm,km)     :: xyr_VLON
      !(out) �٥��ȥ��η�����ʬ

      real(8), dimension(0:im-1,1:jm,km)     :: xyr_VLAT
      !(out) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,km)     :: xyr_VRAD
      !(out) �٥��ȥ���ư����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      xyr_VLON =   xyr_RAD * xyr_GradLat_wq(wq_TORPOT) &
                 + xya_GradLon_wa(wr_RotDRad_wq(wq_POLPOT))
      xyr_VLAT = - xyr_RAD * xyr_GradLon_wq(wq_TORPOT) &
                 + xya_GradLat_wa(wr_RotDRad_wq(wq_POLPOT))
      xyr_VRAD = xyr_wq(wq_L2_wq(wq_POLPOT))/xyr_RAD

    end subroutine wq_Potential2Vector

    subroutine wq_Potential2Rotation(&
       xyr_RotVLON,xyr_RotVLAT,xyr_RotVRAD,wq_TORPOT,wq_POLPOT)
      !
      ! �ȥ�����ݥ�����ݥƥ󥷥�릷,����ɽ�������ȯ���٥��ȥ��
      !
      !     v = ��x(��r) + ��x��x(��r) 
      !
      ! ���Ф���, ���β�ž
      !
      !     ��xv = ��x��x(��r) + ��x��x��x(��r) = ��x��x(��r) - ��x((��^2��)r)
      !
      ! ��׻�����. 
      
      ! �٥��ȥ��β�ž
      real(8), dimension(0:im-1,1:jm,km), intent(OUT) :: xyr_RotVLON
      !(out) ��ž�η�����ʬ

      real(8), dimension(0:im-1,1:jm,km), intent(OUT) :: xyr_RotVLAT
      !(out) ��ž�ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,km), intent(OUT) :: xyr_RotVRAD
      !(out) ��ž��ư����ʬ

      ! ���ϥ٥��ȥ���ɽ���ݥƥ󥷥��
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      call wq_Potential2Vector( &
           xyr_RotVLON,xyr_RotVLAT,xyr_RotVRAD, &
           -wq_Lapla_wq(wq_POLPOT), wq_TORPOT)

    end subroutine wq_Potential2Rotation

  !--------------- �ݥ�����/�ȥ������ǥ��ѥ��ڥ��ȥ���� ----------------

    function nmr_ToroidalEnergySpectrum_wq(wq_TORPOT)
      !
      ! �ȥ�����ݥƥ󥷥�뤫��, �ȥ����륨�ͥ륮����
      ! ����Ĵ��ȡ�����ȿ� n, �Ӿ��ȿ� m �γ���ʬ��׻�����
      !
      !  * ���ȿ� n, �Ӿ��ȿ� m �Υȥ�����ݥƥ󥷥��Υ��ڥ��ȥ���ʬ
      !    ��(n,m,r)�������ȿ� n, �Ӿ��ȿ� m ��ʬ�Υȥ����륨�ͥ륮��
      !    ���ڥ��ȥ��  (1/2)n(n+1)4��r^2|��(n,m,r)|^2  �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�ư����ʬ�������(r^2�νŤ�̵��)
      !    �������Ǥ������ͥ륮����������.
      !    
      !  * �ǡ�����¸�ߤ��ʤ����ȿ� n, �Ӿ��ȿ� m ������ˤϷ�»�ͤ���Ǽ�����.
      !    wq_VMiss �ˤ�ä�����Ǥ��� (����ͤ� -999.0)
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,-nm:nm,km) :: nmr_ToroidalEnergySpectrum_wq
      !(out) ���ͥ륮�����ڥ��ȥ�ȥ�������ʬ

      real(8), dimension((nm+1)*(nm+1),km) ::wr_DATA   ! ����ΰ�
      integer :: n, m

      nmr_ToroidalEnergySpectrum_wq = wq_VMiss

      wr_DATA = wr_wq(wq_TORPOT)

      do n=0,nm
         nmr_ToroidalEnergySpectrum_wq(n,0,:) &
              = 0.5 * n*(n+1)* (4*pi) * r_Rad**2 &
                * wr_DATA(l_nm(n,0),:)**2
         do m=1,n
            nmr_ToroidalEnergySpectrum_wq(n,m,:) &
              = 0.5 * n*(n+1)* (4*pi) * r_Rad**2 &
                * (wr_DATA(l_nm(n,m),:)**2+wr_DATA(l_nm(n,-m),:)**2)
            nmr_ToroidalEnergySpectrum_wq(n,-m,:) &
                 = nmr_ToroidalEnergySpectrum_wq(n,m,:) 
         enddo
      enddo

    end function nmr_ToroidalEnergySpectrum_wq

    function nr_ToroidalEnergySpectrum_wq(wq_TORPOT)
      !
      ! �ȥ�����ݥƥ󥷥�뤫��, �ȥ����륨�ͥ륮����
      ! ����Ĵ��ȡ�����ȿ��γ���ʬ��׻�����.
      !
      !  * ���ȿ� n, �Ӿ��ȿ� m �Υȥ�����ݥƥ󥷥��Υ��ڥ��ȥ���ʬ
      !    ��(n,m,r)�������ȿ� n ��ʬ�Υȥ����륨�ͥ륮�����ڥ��ȥ��
      !    ��[m=-n]^n(1/2)n(n+1)4��r^2|��(n,m,r)|^2 �ȷ׻������.
      !
      ! * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�ư����ʬ�������(r^2�νŤ�̵��)
      !    �������Ǥ������ͥ륮����������.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,km) :: nr_ToroidalEnergySpectrum_wq 
      !(out) ���ͥ륮�����ڥ��ȥ�ȥ�������ʬ

      real(8), dimension((nm+1)*(nm+1),km) ::wr_DATA   ! ����ΰ�
      integer :: n, m

      wr_DATA = wr_wq(wq_TORPOT)
      do n=0,nm
         nr_ToroidalEnergySpectrum_wq(n,:) &
              = 0.5 * n*(n+1)* (4*pi) * r_Rad**2 * wr_Data(l_nm(n,0),:)**2
         do m=1,n
            nr_ToroidalEnergySpectrum_wq(n,:) &
                 = nr_ToroidalEnergySpectrum_wq(n,:) &
                 + 0.5 * n*(n+1)* (4*pi) * r_Rad**2  &
                 * 2* (wr_Data(l_nm(n,m),:)**2 + wr_Data(l_nm(n,-m),:)**2)
         enddo
      enddo

    end function nr_ToroidalEnergySpectrum_wq

    function nmr_PoloidalEnergySpectrum_wq(wq_POLPOT)
      !
      ! �ݥ�����ݥƥ󥷥�뤫��, �ݥ����륨�ͥ륮����
      ! ����Ĵ��ȡ�����ȿ� n, �Ӿ��ȿ� m �γ���ʬ��׻�����.
      !
      !  * ���ȿ� n, �Ӿ��ȿ� m �Υݥ�����ݥƥ󥷥��Υ��ڥ��ȥ���ʬ
      !    ��(n,m,r)�������ȿ� n, �Ӿ��ȿ� m ��ʬ�Υݥ����륨�ͥ륮��
      !    ���ڥ��ȥ�� 
      !
      !      (1/2)n(n+1)4��{|d(r��(n,m,r))/dr|^2 + n(n+1)|��(n,m,r)|^2} 
      !
      !    �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�ư����ʬ�������(r^2�νŤ�̵��)
      !    �������Ǥ������ͥ륮����������.
      !
      !  * �ǡ�����¸�ߤ��ʤ����ȿ� n, �Ӿ��ȿ� m ������ˤϷ�»�ͤ���Ǽ�����.
      !    ��»�ͤ��ͤϥ⥸�塼���ѿ� wq_VMiss �ˤ�ä�����Ǥ���
      !    (����ͤ� -999.0)
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,-nm:nm,km) :: nmr_PoloidalEnergySpectrum_wq 
      !(out) ���ͥ륮�����ڥ��ȥ�ݥ�������ʬ


      real(8), dimension((nm+1)*(nm+1),km) ::wr_DATA1   ! ����ΰ�
      real(8), dimension((nm+1)*(nm+1),km) ::wr_DATA2   ! ����ΰ�
      integer :: n, m

      nmr_PoloidalEnergySpectrum_wq = wq_VMiss

      wr_Data1 = wr_wq(wq_POLPOT)
      wr_Data2 = wr_wq(wq_RadDRad_wq(wq_POLPOT)+wq_POLPOT)  ! d(r��)/dr
                                                            ! = rd��/dr+��

      do n=0,nm
         nmr_PoloidalEnergySpectrum_wq(n,0,:) = &
                 + 0.5* n*(n+1)* (4*pi) &
                 *( wr_Data2(l_nm(n,0),:)**2  &
                   + n*(n+1)*wr_Data1(l_nm(n,0),:)**2 )
         do m=1,n
            nmr_PoloidalEnergySpectrum_wq(n,m,:) = &
                 + 0.5* n*(n+1)* (4*pi) &
                 *( wr_Data2(l_nm(n,m),:)**2 + wr_Data2(l_nm(n,-m),:)**2 &
                 + n*(n+1)* ( wr_Data1(l_nm(n,m),:)**2 + wr_Data1(l_nm(n,-m),:)**2))
            nmr_PoloidalEnergySpectrum_wq(n,-m,:) = &
                 nmr_PoloidalEnergySpectrum_wq(n,m,:)
         enddo
      enddo

    end function nmr_PoloidalEnergySpectrum_wq

    function nr_PoloidalEnergySpectrum_wq(wq_POLPOT)
      !
      ! �ݥ�����ݥƥ󥷥�뤫��, �ݥ����륨�ͥ륮����
      ! ����Ĵ��ȡ�����ȿ��γ���ʬ��׻�����
      !
      !  * ���ȿ� n, �Ӿ��ȿ� m �Υݥ�����ݥƥ󥷥��Υ��ڥ��ȥ���ʬ
      !    ��(n,m,r)�������ȿ� n ��ʬ�Υݥ����륨�ͥ륮�����ڥ��ȥ��
      !
      !      ��[m=-n]^n ((1/2)n(n+1)4��{|d(r��(n,m,r))/dr|^2 
      !                 + n(n+1)|��(n,m,r)|^2} 
      !
      !    �ȷ׻������.
      !
      !  * ���Ƥ����ȿ����Ф��ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�ư����ʬ�������
      !    (r^2�νŤ�̵��)�������Ǥ������ͥ륮����������.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,km) :: nr_PoloidalEnergySpectrum_wq
      !(out) ���ͥ륮�����ڥ��ȥ�ݥ�������ʬ

      real(8), dimension((nm+1)*(nm+1),km) ::wr_DATA1   ! ����ΰ�
      real(8), dimension((nm+1)*(nm+1),km) ::wr_DATA2   ! ����ΰ�
      integer :: n, m

      wr_Data1 = wr_wq(wq_POLPOT)
      wr_Data2 = wr_wq(wq_RadDRad_wq(wq_POLPOT)+wq_POLPOT)  ! d(r��)/dr
                                                            ! = rd��/dr+��

      do n=0,nm
         nr_PoloidalEnergySpectrum_wq(n,:) &
              = 0.5* n*(n+1)* (4*pi) &
              *( wr_Data2(l_nm(n,0),:)**2  + n*(n+1)*wr_Data1(l_nm(n,0),:)**2 )
         do m=1,n
            nr_PoloidalEnergySpectrum_wq(n,:) &
                 = nr_PoloidalEnergySpectrum_wq(n,:) &
                 + 2 * 0.5* n*(n+1)* (4*pi) &
                 *( wr_Data2(l_nm(n,m),:)**2 + wr_Data2(l_nm(n,-m),:)**2 &
                 + n*(n+1)*(wr_Data1(l_nm(n,m),:)**2 +wr_Data1(l_nm(n,-m),:)**2))
         enddo
      enddo

    end function nr_PoloidalEnergySpectrum_wq

  !--------------- ���������� -----------------

    subroutine wq_BoundaryTau(wq,value,cond)
      !
      ! ���ڥ��ȥ�ǡ����˥ǥ��ꥯ�졦�Υ��ޥ󶭳�����Ŭ�Ѥ���
      ! Chebyshev ���֤Ǥζ������Ŭ��(����ˡ)
      !
      ! �����ӥ����ն��֤ˤ����ƶ��������������٤��⼡�η�����
      ! ������ˡ��ȤäƤ���(����ˡ).
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)      :: wq
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension((nm+1)*(nm+1)), intent(in), optional   :: value
              !(in) �����Ǥ� ��/���� ʬ�ۤ��ʿ���ڥ��ȥ��Ѵ�������Τ�Ϳ����. 
              !     ��ά������/���� 0 �Ȥʤ�. 

      character(len=1), intent(in), optional                    :: cond
              !(in) �������. ��ά���� 'D'
              !        D : ��¦�ǥ��ꥯ����
              !        N : ��¦�Υ��ޥ���

      if (.not. present(cond)) then
         if (present(value)) then
            call aq_BoundaryTau_D(wq,value)
         else
            call aq_BoundaryTau_D(wq)
         endif
         return
      endif

      select case(cond)
      case ('N')
         if (present(value)) then
            call aq_BoundaryTau_N(wq,value)
         else
            call aq_BoundaryTau_N(wq)
         endif
      case ('D')
         if (present(value)) then
            call aq_BoundaryTau_D(wq,value)
         else
            call aq_BoundaryTau_D(wq)
         endif
      case default
         call MessageNotify('E','wq_BoundaryTau','B.C. not supported')
      end select

    end subroutine wq_BoundaryTau

    subroutine wr_BoundaryGrid(wr,value,cond)
      !
      ! ���ڥ��ȥ�ǡ����˥ǥ��ꥯ�졦�Υ��ޥ󶭳�����Ŭ�Ѥ���
      ! �¶��֤Ǥζ������Ŭ��
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). 
      !
      real(8), dimension((nm+1)*(nm+1),km),intent(inout)      :: wr
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension((nm+1)*(nm+1)), intent(in), optional   :: value
              !(in) �����Ǥ� ��/���� ʬ�ۤ��ʿ���ڥ��ȥ��Ѵ�������Τ�Ϳ����. 
              !    ��ά������/���� 0 �Ȥʤ�. 

      character(len=1), intent(in), optional             :: cond
              !(in) �������. ��ά���� 'D'
              !        D : ��¦�ǥ��ꥯ����
              !        N : ��¦�Υ��ޥ���

      if (.not. present(cond)) then
         if (present(value)) then
            call ag_BoundaryGrid_D(wr,value)
         else
            call ag_BoundaryGrid_D(wr)
         endif
         return
      endif

      select case(cond)
      case ('N')
         if (present(value)) then
            call ag_BoundaryGrid_N(wr,value)
         else
            call ag_BoundaryGrid_N(wr)
         endif
      case ('D')
         if (present(value)) then
            call ag_BoundaryGrid_D(wr,value)
         else
            call ag_BoundaryGrid_D(wr)
         endif
      case default
         call MessageNotify('E','wr_BoundaryGrid','B.C. not supported')
      end select

    end subroutine wr_BoundaryGrid

    subroutine wq_TorBoundaryTau(wq_TORPOT,value,cond,new) 
      !
      ! ®�٥ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���. 
      ! Chebyshev ���֤Ǥζ������Ŭ��. 
      !
      ! ®�٥ȥ�����ݥƥ󥷥�릷���Ф���Ϳ�����붭������
      !
      !   * Ǵ���� : �� = ��b(lon,lat). ��b �϶������̤Ǥ�®��ʬ��. 
      !                                   default �� 0(�Ż߾���).
      !
      !   * ���Ϥʤ���� : ��(��/r)/��r = 0.
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)      :: wq_TORPOT
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension((nm+1)*(nm+1)), intent(in), optional :: value
              !(in) ξü�����ǤΥȥ�����ݥƥ󥷥��
              !     Ǵ����λ��Τ�ͭ��

      character(len=1), intent(in), optional  :: cond
              !(in) ������凉���å�. ��ά���� 'R'
              !     R    : ��¦Ǵ����
              !     F    : ��¦���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp
      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wq_data
      real(8), dimension((nm+1)*(nm+1),km)    :: wr_data
      logical                                 :: rigid        ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: n, l, lend
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid=.TRUE.
      else
         select case (cond)
         case ('R')
            rigid = .TRUE.
         case ('F')
            rigid = .FALSE.
         case default
            call MessageNotify('E','wq_TorBoundaryTau','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((nm+1)*(nm+1),0:lm,0:lm),kp((nm+1)*(nm+1),0:lm))

         alu = 0.0D0
         do l=0,lm
            alu(:,l,l)=1.0D0
         enddo

         ! �ϳ�Ū���

         do l=0,lm
            wq_data = 0.0 ; wq_data(:,l) = 1.0D0

            if ( rigid ) then          ! �ϳ�Ū���Ǵ��
               wr_data = wr_wq(wq_data)
            else                       ! �ϳ�Ū��Ｋͳ���٤�
               wr_data = wr_wq(wq_RadDRad_wq(wq_data)- wq_data)/wr_Rad
            endif

            do n=1,(nm+1)**2
               if ( mod(nd(n),2) .eq. mod(lm,2) ) then
                  alu(n,lm,l) = wr_data(n,km)
               else
                  alu(n,lm-1,l) = wr_data(n,km)
               endif
            end do
         enddo

         ! �ط��ʤ��Ȥ���� 0 ������.
         do n=1,(nm+1)**2
            if ( mod(nd(n),2) .eq. mod(lm,2) ) then
               lend = lm
            else
               lend = lm-1
            endif

            do l=0,nd(n)-1
               alu(n,lend,l) = 0.0D0
            enddo
            do l=nd(n)+1,lm,2
               alu(n,lend,l) = 0.0D0
            enddo
         enddo

         call ludecomp(alu,kp)

         if ( rigid .AND. present(value) ) then 
            call MessageNotify('M','wq_TorBoundaryTau',&
                 'Toroidal potential at k=km was given by the optional variable.')
         else if ( rigid .AND. (.NOT.present(value)) ) then
            call MessageNotify('M','wq_TorBoundaryTau',&
                 'Toroidal potential at k=km was set to zero.')
         else if ( (.NOT. rigid) .AND. present(value) ) then
            call MessageNotify('W','wq_TorBoundaryTau',&
                 'Boundary value k=km cannot be set under stress-free condition.')
         endif

         call MessageNotify('M','wq_TorBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      do n=1,(nm+1)**2
         if ( mod(nd(n),2) .eq. mod(lm,2) ) then
            lend = lm
         else
            lend = lm-1
         endif

         if ( rigid .AND. present(value) ) then
            wq_torpot(n,lend) = value(n)
         else
            wq_torpot(n,lend) = 0.0D0
         endif
      enddo

      wq_torpot = lusolve(alu,kp,wq_TORPOT)

    end subroutine wq_TorBoundaryTau

    subroutine wr_TorBoundaryGrid(wr_TORPOT,value,cond,new) 
      !
      ! ®�٥ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! �¶��֤Ǥζ������Ŭ��
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). 
      !
      ! ®�٥ȥ�����ݥƥ󥷥�릷���Ф���Ϳ�����붭������
      !
      !   * Ǵ���� : �� = ��b(lon,lat). ��b �϶������̤Ǥ�®��ʬ��. 
      !                                   default �� 0 (�Ż߾���).
      !
      !   * ���Ϥʤ���� : ��(��/r)/��r = 0.
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension((nm+1)*(nm+1),km),intent(inout)    :: wr_TORPOT
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension((nm+1)*(nm+1)), intent(in), optional :: value
              !(in) ξü�����ǤΥȥ�����ݥƥ󥷥��
              !     Ǵ����λ��Τ�ͭ��

      character(len=1), intent(in), optional  :: cond
              !(in) ������凉���å�. ��ά���� 'R'
              !     R    : ��¦Ǵ����
              !     F    : ��¦���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp
      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wq_data
      real(8), dimension((nm+1)*(nm+1),km)    :: wr_data
      logical                                 :: rigid   ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid=.TRUE.
      else
         select case (cond)
         case ('R')
            rigid = .TRUE.
         case ('F')
            rigid = .FALSE.
         case default
            call MessageNotify('E','wr_TorBoundaryGrid','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((nm+1)*(nm+1),km,km),kp((nm+1)*(nm+1),km))

         alu = 0.0D0
         do k=1,km
            wr_data = 0.0D0
            wr_data(:,k)=1.0D0
            alu(:,:,k) = wr_data
         enddo

         if ( .not. rigid ) then
            do k=1,km
               wr_data = 0.0D0
               wr_data(:,k)=1.0D0
               wq_data = wq_wr(wr_data)
               wr_data = wr_wq(wq_RadDRad_wq(wq_data) - wq_data)/wr_Rad
               alu(:,km,k) = wr_data(:,km)
            enddo
         endif

         call ludecomp(alu,kp)

         if ( rigid .AND. present(value) ) then 
            call MessageNotify('M','wr_TorBoundaryGrid',&
                 'Toroidal potential at k=km was given by the optional variable.')
         else if ( rigid .AND. (.NOT.present(value)) ) then
            call MessageNotify('M','wr_TorBoundaryGrid',&
                 'Toroidal potential at k=km was set to zero.')
         else if ( (.NOT. rigid) .AND. present(value) ) then
            call MessageNotify('W','wr_TorBoundaryGrid',&
                 'Boundary value at k=km cannot be set under stress-free condition.')
         endif

         call MessageNotify('M','wr_TorBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      if ( rigid .AND. present(value) ) then
         wr_TorPot(:,km)  = value
      else
         wr_TorPot(:,km)  = 0.0D0
      endif

      wr_TorPot = lusolve(alu,kp,wr_TorPot)

    end subroutine wr_TorBoundaryGrid

    function wq_LaplaPol2PolTau_wq(wq,cond,new)
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵��^2������׻�����.
      !
      ! ���ڥ��ȥ���֤Ƕ�������Ŭ�Ѥ��Ƥ���(����ˡ). 
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵�� f = ��^2���������뼰��
      !
      !   ��^2�� = f
      !     �� = const. at Boundary.
      !     �ߦ�/��r = 0 at Boundary           (Ǵ����) 
      !     or ��^2��/��r^2 = 0 at Boundary    (���Ϥʤ����)
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(in)  :: wq
              !(in) ���Ϣ�^2��ʬ��

      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wq_LaplaPol2PolTau_wq
              !(out) ���ϥݥ�����ݥƥ󥷥��ʬ��

      character(len=1), intent(in), optional  :: cond
              !(in) ������凉���å�. ��ά���� 'R'
              !     R    : ��¦Ǵ����
              !     F    : ��¦���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu     ! �����ΰ�׻���
      integer, dimension(:,:), allocatable    :: kp      ! �����ΰ�׻���

      real(8), dimension(:,:,:), allocatable  :: alub    ! �������׻���
      integer, dimension(:,:), allocatable    :: kpb     ! �������׻���

      real(8), dimension((nm+1)*(nm+1),km)    :: wr_work
      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wq_work
      logical                                 :: rigid   ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l,n, lend
      save    :: alu, kp, first
      save    :: alub, kpb

      if (.not. present(cond)) then
         rigid=.TRUE. 
      else
         select case (cond)
         case ('R')
            rigid = .TRUE.
         case ('F')
            rigid = .FALSE.
         case default
            call MessageNotify('E','wq_laplapol2pol_wq','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(alub) ) deallocate(alub)
         if ( allocated(kpb) ) deallocate(kpb)
         allocate(alu((nm+1)*(nm+1),0:lm,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(alub((nm+1)*(nm+1),0:lm,0:lm),kpb((nm+1)*(nm+1),0:lm))


         !---- �����ΰ�׻��ѹ��� -----
         do l=0,lm
            wq_work = 0.0
            wq_work(:,l) = 1.0D0
            alu(:,:,l) = wq_Lapla_wq(wq_work)
         enddo

         ! 0 ��ʬ�ΤȤ���� 1 ������.
         do n=1,(nm+1)**2
            do l=0,nd(n)-1
               alu(n,l,l) = 1.0D0
            enddo
            do l=nd(n)+1,lm,2
               alu(n,l,l) = 1.0D0
            enddo
         enddo

         ! alu(:,:,nd(n)) ��� 0 �ʤΤ� 1 �򤤤�Ƥ���. 
         ! l=nd(n) ��ʬ�϶������Ƿ���. 
         do n=1,(nm+1)**2
            if ( mod(nd(n),2) .eq. mod(lm,2) ) then
               alu(n,lm,nd(n)) = 1.0D0
            else
               alu(n,lm-1,nd(n)) = 1.0D0
            endif
         enddo

         call ludecomp(alu,kp)

         !---- �������׻��ѹ��� -----

         alub = 0.0D0
         do l=0,lm
            alub(:,l,l) = 1.0D0
         enddo

         do l=0,lm
            wq_work=0.0D0 ; wq_work(:,l)=1.0D0
            wr_work = wr_wq(wq_work)

            ! ��ư��Ū���. ή���϶����ǰ���
            !     l=nd(n) ��ʬ�򶭳����Ƿ���. 
            do n=1,(nm+1)**2
               alub(n,nd(n),l) = wr_work(n,km)
            enddo

            ! �ϳ�Ū���Ǵ���� 
            !     l=lend ��ʬ�򶭳����Ƿ���. 
            if ( rigid ) then
               wr_work=wr_wq(wq_RadDRad_wq(wq_work))/wr_Rad
            else
               wr_work=wr_wq(wq_RadDRad_wq(wq_RadDRad_wq(wq_work)) &
                                           -wq_RadDRad_wq(wq_work) )&
                            /wr_Rad**2
            endif
            
            do n=1,(nm+1)**2
               if ( mod(nd(n),2) .eq. mod(lm,2) ) then
                  lend = lm
               else
                  lend = lm-1
               endif
               alub(n,lend,l) = wr_work(n,km)
            enddo
         enddo

         ! �ط��ʤ��Ȥ���� 0 ������.
         do n=1,(nm+1)**2
            if ( mod(nd(n),2) .eq. mod(lm,2) ) then
               lend = lm
            else
               lend = lm-1
            endif

            do l=0,nd(n)-1
               alub(n,nd(n),l) = 0.0D0
               alub(n,lend,l) = 0.0D0
            enddo
            do l=nd(n)+1,lm,2
               alub(n,nd(n),l) = 0.0D0
               alub(n,lend,l) = 0.0D0
            enddo
         enddo

         call ludecomp(alub,kpb)

         if ( rigid ) then
            call MessageNotify('M','wq_LaplaPol2PolTau_wq',&
                              'Matrix to apply rigid b.c. newly produced.')
         else
            call MessageNotify('M','wq_LaplaPol2PolTau_wq',&
                              'Matrix to apply stress-free b.c. newly produced.')
         endif
      endif

      ! �����ΰ�׻�
      wq_work = wq

      wq_work = lusolve(alu,kp,wq_work)

      ! �������׻�
      do n=1,(nm+1)**2
         wq_work(n,nd(n)) = 0
         if ( mod(nd(n),2) .eq. mod(lm,2) ) then
            wq_work(n,lm)   = 0
         else
            wq_work(n,lm-1) = 0
         endif
      enddo

      wq_laplapol2polTau_wq = lusolve(alub,kpb,wq_work)

    end function wq_LaplaPol2PolTau_wq

    function wr_LaplaPol2Pol_wr(wr,cond,new)
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵��^2������׻�����.
      !
      ! �ʻ������֤Ƕ�������Ŭ�Ѥ��Ƥ���. 
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵�� f = ��^2���������뼰��
      !
      !   ��^2�� = f
      !     �� = const. at Boundary.
      !     �ߦ�/��r = 0 at Boundary           (Ǵ����) 
      !     or ��^2��/��r^2 = 0 at Boundary    (���Ϥʤ����)
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      ! ��� : ���δؿ��ϴ������Ƥ��ʤ�. ���Ѷػ�. 
      !
      real(8), dimension((nm+1)*(nm+1),km),intent(in)  :: wr
              !(in) ���Ϣ�^2��ʬ��

      real(8), dimension((nm+1)*(nm+1),km)         :: wr_LaplaPol2Pol_wr
              !(out) ���ϥݥ�����ݥƥ󥷥��ʬ��

      character(len=1), intent(in), optional  :: cond
              !(in) ������凉���å�. ��ά���� 'R'
              !     R    : ��¦Ǵ����
              !     F    : ��¦���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:,:), allocatable  :: alub
      integer, dimension(:,:), allocatable    :: kpb

      real(8), dimension((nm+1)*(nm+1),km)    :: wr_work
      logical                                 :: rigid   ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k
      save    :: alu, kp, first
      save    :: alub, kpb

      if (.not. present(cond)) then
         rigid=.TRUE. 
      else
         select case (cond)
         case ('R')
            rigid = .TRUE.
         case ('F')
            rigid = .FALSE.
         case default
            call MessageNotify('E','wr_laplapol2pol_wr','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((nm+1)*(nm+1),0:lm,0:lm),kp((nm+1)*(nm+1),0:lm))

         if ( allocated(alub) ) deallocate(alub)
         if ( allocated(kpb) ) deallocate(kpb)
         allocate(alub((nm+1)*(nm+1),km,km),kpb((nm+1)*(nm+1),km))

         !---- �����ΰ�׻��ѹ��� -----
         do k=1,km
            wr_work = 0.0D0 ; wr_work(:,k) = 1.0D0

            ! �ƿ�ʿ�ȿ��˴ؤ�����Ω�μ�
            alu(:,:,k) = wr_wq(wq_Lapla_wq(wq_wr(wr_work)))
         enddo

         do k=1,km
            wr_work=0.0D0 ; wr_work(:,k)=1.0D0
            alu(:,km,k)  = wr_work(:,km)
         enddo


         !---- �������׻��� ----
         alub = 0.0D0
         do k=1,km
            alub(:,k,k) = 1.0D0
         enddo

         ! ��ư��Ū���. ή���϶����ǰ���
         !   k=km ���ͤ�����
         do k=1,km
            wr_work=0.0D0 ; wr_work(:,k)=1.0D0
            alub(:,km,k)  = wr_work(:,km)
         enddo

         ! �ϳ�Ū���Ǵ���� 
         !   k=km-1 ���ͤ�����
         if ( rigid ) then
            do k=1,km
               wr_work = 0.0D0 ; wr_work(:,k) = 1.0D0
               wr_work=wr_wq(wq_RadDRad_wq(wq_wr(wr_work)))/wr_Rad
               alub(:,km-1,k) = wr_work(:,km)
            enddo
         else
            do k=1,km
               wr_work = 0.0D0 ; wr_work(:,k) = 1.0D0
               wr_work=wr_wq(wq_RadDRad_wq(wq_RadDRad_wq(wq_wr(wr_work))) &
                                          -wq_RadDRad_wq(wq_wr(wr_work)))&
                       /wr_Rad**2
               alub(:,km-1,k) = wr_work(:,km)
            enddo
         endif

         call ludecomp(alub,kpb)

         if ( rigid ) then
            call MessageNotify('M','wr_LaplaPol2Pol_wr',&
                              'Matrix to apply rigid b.c. newly produced.')
         else
            call MessageNotify('M','wr_LaplaPol2Pol_wr',&
                              'Matrix to apply stress-free b.c. newly produced.')
         endif
      endif

      wr_work         = wr
      wr_work         = lusolve(alu,kp,wr_work)

      wr_work(:,km-1) = 0.0D0               ! �ϳ�Ū���
      wr_work(:,km)   = 0.0D0               ! ��ư��Ū���

      wr_laplapol2pol_wr = lusolve(alub,kpb,wr_work)

    end function wr_LaplaPol2Pol_wr

    subroutine wq_TormagBoundaryTau(wq_TOR,new)
      
      ! ����ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! Chebyshev ���֤Ǥζ������Ŭ��
      !
      ! �����ӥ����ն��֤ˤ����ƶ��������������٤��⼡�η�����������ˡ��
      ! �ȤäƤ���(����ˡ). ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ�
      ! �б����Ƥ���. ���ξ��, ����ȥ�����ݥƥ󥷥��ζ�������
      !
      ! ��¦
      !    wq_psi = 0   at the outer boundary
      ! 
      ! �Ǥ��뤫�� wq_Boundary ���б���ǽ����, ����Τ������Ӻ������Ƥ���.
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)   :: wq_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wq_I
      real(8), dimension(:,:), allocatable    :: wr_PSI

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: n, l, lend
      save    :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wq_I) ) deallocate(wq_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:lm,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(wq_I((nm+1)*(nm+1),0:lm),wr_PSI((nm+1)*(nm+1),km))

         alu = 0.0D0
         do l=0,lm
            alu(:,l,l) = 1.0D0
         enddo

         do l=0,lm
            wq_I = 0.0 ; wq_I(:,l) = 1.0
            ! ���ŵ���Ƴ��
            wr_PSI = wr_wq(wq_I)

            do n=1,(nm+1)**2
               if ( mod(nd(n),2) .eq. mod(lm,2) ) then
                  alu(n,lm,l) = wr_Psi(n,km)
                  lend = lm
               else
                  alu(n,lm-1,l) = wr_Psi(n,km)
               endif
            enddo
         enddo

         ! �ط��ʤ��Ȥ���� 0 ������.
         do n=1,(nm+1)**2
            if ( mod(nd(n),2) .eq. mod(lm,2) ) then
               lend = lm
            else
               lend = lm-1
            endif

            do l=0,nd(n)-1
               alu(n,lend,l) = 0.0D0
            enddo
            do l=nd(n)+1,lm,2
               alu(n,lend,l) = 0.0D0
            enddo
         enddo

         call ludecomp(alu,kp)

         deallocate(wq_I,wr_PSI)

         call MessageNotify('M','TormagBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      do n=1,(nm+1)**2
         if ( mod(nd(n),2) .eq. mod(lm,2) ) then
            wq_TOR(n,lm)   = 0.0
         else
            wq_TOR(n,lm-1) = 0.0
         endif
      enddo
      wq_TOR = lusolve(alu,kp,wq_TOR)

    end subroutine wq_TormagBoundaryTau

    subroutine wr_TormagBoundaryGrid(wr_TOR,new)
      !
      ! ����ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! ��ľ�¶��֤Ǥζ������Ŭ��.
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). 
      !
      ! ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ��б����Ƥ���. 
      ! ���ξ��, ����ȥ�����ݥƥ󥷥��ζ�������
      !
      ! ��¦
      !    wq_psi = 0   at the outer boundary
      ! 
      ! �Ǥ���Τ� wq_Boundary ���б���ǽ����, ����Τ������Ӻ������Ƥ���
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension((nm+1)*(nm+1),km),intent(inout)   :: wr_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 
      
      logical, intent(IN), optional :: new
              !(in) (���ߡ�) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      wr_TOR(:,km)  = 0.0D0

    end subroutine wr_TormagBoundaryGrid

    subroutine wq_PolmagBoundaryTau(wq_POL,new)
      !
      ! ����ݥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! Chebyshev ���֤Ǥζ������Ŭ��
      !
      ! �����ӥ����ն��֤ˤ����ƶ��������������٤��⼡�η�����������ˡ��
      ! �ȤäƤ���(����ˡ). ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ�
      ! �б����Ƥ���. ���ξ��, ����ݥ�����ݥƥ󥷥��γƿ�ʿ���ڥ��ȥ�
      ! ��ʬ h �ˤ������ƶ�����郎Ϳ�����,
      !
      !  * ��¦���� : dh/dr + (n+1)h/r = 0
      !
      ! �Ǥ���. ������ n �� h �ο�ʿ���ȿ��Ǥ���. 
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)   :: wq_POL
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wq_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l, n, nn(2), lend
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wq_I) ) deallocate(wq_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         if ( allocated(wr_DPSIDR) ) deallocate(wr_DPSIDR)

         allocate(alu((nm+1)*(nm+1),0:lm,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(wq_I((nm+1)*(nm+1),0:lm))
         allocate(wr_PSI((nm+1)*(nm+1),km),wr_DPSIDR((nm+1)*(nm+1),km))

         alu = 0.0D0
         do l=0,lm
            alu(:,l,l) = 1.0D0
         enddo

         ! ���ŵ���Ƴ��
         do l=0,lm
            wq_I = 0.0D0  ; wq_I(:,l) = 1.0D0
            wr_PSI = wr_wq(wq_I)
            wr_DPSIDR = wr_wq(wq_RadDRad_wq(wq_I))/wr_Rad

            do n=1,(nm+1)**2
               if ( mod(nd(n),2) .eq. mod(lm,2) ) then
                  lend = lm
               else
                  lend = lm-1
               endif
               nn=nm_l(n)
               alu(n,lend,l) = wr_DPSIDR(n,km) + (nn(1)+1) * wr_PSI(n,km)/r_RAD(km)
            enddo
         enddo

         ! �ط��ʤ��Ȥ���� 0 ������.
         do n=1,(nm+1)**2
            if ( mod(nd(n),2) .eq. mod(lm,2) ) then
               lend = lm
            else
               lend = lm-1
            endif

            do l=0,nd(n)-1
               alu(n,lend,l) = 0.0D0
            enddo
            do l=nd(n)+1,lm,2
               alu(n,lend,l) = 0.0D0
            enddo
         enddo

         call ludecomp(alu,kp)

         deallocate(wq_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','PolmagBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      do n=1,(nm+1)**2
         if ( mod(nd(n),2) .eq. mod(lm,2) ) then
            wq_POL(n,lm)   = 0.0
         else
            wq_POL(n,lm-1) = 0.0
         endif
      enddo
      wq_POL = lusolve(alu,kp,wq_POL)

    end subroutine wq_PolmagBoundaryTau

    subroutine wr_PolmagBoundaryGrid(wr_POL,new)
      !
      ! ����ݥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! ��ľ�¶��֤Ǥζ������Ŭ��. 
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). 
      !
      ! ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ��б����Ƥ���. 
      ! ���ξ��, ����ݥ�����ݥƥ󥷥��γƿ�ʿ���ڥ��ȥ���ʬ h ��
      ! �������ƶ�����郎Ϳ�����,
      !
      !  * ��¦���� : dh/dr + (n+1)h/r = 0
      !
      ! �Ǥ���. ������ n �� h �ο�ʿ���ȿ��Ǥ���. 
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension((nm+1)*(nm+1),km),intent(inout)   :: wr_POL
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wr_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: k, n, nn(2)
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wr_I) ) deallocate(wr_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         if ( allocated(wr_DPSIDR) ) deallocate(wr_DPSIDR)

         allocate(alu((nm+1)*(nm+1),km,km),kp((nm+1)*(nm+1),km))
         allocate(wr_I((nm+1)*(nm+1),km))
         allocate(wr_PSI((nm+1)*(nm+1),km),wr_DPSIDR((nm+1)*(nm+1),km))

         do k=1,km
            wr_I = 0.0D0
            wr_I(:,k)=1.0D0
            alu(:,:,k) = wr_I                 ! �����ΰ���ͤ��Τޤ�.
         enddo

         ! ���ŵ���Ƴ��
         do k=1,km
            wr_I = 0.0D0
            wr_I(:,k) = 1.0D0
            wr_PSI = wr_I
            wr_DPSIDR = wr_wq(wq_RadDRad_wq(wq_wr(wr_I)))/wr_Rad

            do n=1,(nm+1)*(nm+1)
               nn=nm_l(n)
               alu(n,km,k) = wr_DPSIDR(n,km) + (nn(1)+1) * wr_PSI(n,km)/r_RAD(km)
            enddo
         end do

         call ludecomp(alu,kp)

         deallocate(wr_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','PolmagBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wr_POL(:,km)  = 0.0D0
      wr_POL = lusolve(alu,kp,wr_POL)

    end subroutine wr_PolmagBoundaryGrid

end module wq_module_sjpack
