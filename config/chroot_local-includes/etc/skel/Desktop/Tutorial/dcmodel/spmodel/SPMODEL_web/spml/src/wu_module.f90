!--
!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wu_module
!
!    spml/wu_module �⥸�塼��ϵ���Ǥ�ή�α�ư�򥹥ڥ��ȥ�ˡ�ˤ�ä�
!    ���ͷ׻����뤿��� Fortran90 �ؿ����󶡤����ΤǤ���. 
!
!    ��ʿ�����˵���Ĵ��ȡ���Ѵ������ư�������˥����ӥ������Ѵ����Ѥ���
!    ���Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤʴؿ����󶡤���. 
!
!    ������ wa_module, au_module ���Ѥ��Ƥ���. �ǲ����Ǥϵ���Ĵ���Ѵ�
!    ����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� ISPACK �� Fortran77 
!    ���֥롼������Ѥ��Ƥ���.
!
!
!����  2008/01/01  �ݹ�����  wt_module ������
!      2008/01/05  �ݹ�����  wr_Drad2_wu, r_Drad2_u �ɲ�
!      2008/01/10  �ݹ�����  wu_TormagBoundaryGrid �Х�fix
!      2008/01/13  �ݹ�����  wa_initial �����å�Ƴ��
!      2008/07/01  ��������ʿ ��ʿ�����γʻ����� 0:im-1,1:jm �˽���
!      2008/07/04  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!
!����
!      �ǡ�������� index
!        x : ����         y : ����        r : ư��
!        w : ����Ĵ�´ؿ����ڥ��ȥ�
!        n : ����Ĵ�´ؿ����ڥ��ȥ�(��ʿ���ȿ�)
!        m : ����Ĵ�´ؿ����ڥ��ȥ�(�Ӿ��ȿ�)
!        u : �����ӥ����մؿ����ڥ��ȥ�
!        a : Ǥ�դμ���
!
!        xyr : 3 �����ʻ����ǡ���
!        xy  : ��ʿ 2 �����ʻ����ǡ���
!        yr  : �Ҹ��� 2 �����ʻ����ǡ���
!        xr  : ������ 2 �����ʻ����ǡ���
!
!        wr  : ��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���
!        wu  : ���ڥ��ȥ�ǡ���
!
!++
module wu_module
  !
  != wu_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wu_module.f90,v 1.6 2009-02-28 21:33:48 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/wu_module �⥸�塼��ϵ���Ǥ�ή�α�ư�򥹥ڥ��ȥ�ˡ�ˤ�ä�
  ! ���ͷ׻����뤿��� Fortran90 �ؿ����󶡤����ΤǤ���. 
  !
  ! ��ʿ�����˵���Ĵ��ȡ���Ѵ������ư�������˥����ӥ������Ѵ����Ѥ���
  ! ���Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤʴؿ����󶡤���. 
  !
  ! ������ wa_module, au_module ���Ѥ��Ƥ���. �ǲ����Ǥϵ���Ĵ���Ѵ�
  ! ����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� ISPACK �� Fortran77 
  ! ���֥롼������Ѥ��Ƥ���.
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (wu_, nmr_, nr_, xyr_, wr_, w_, xy_, x_, y_, r_, a_) ��, 
  !   �֤��ͤη��򼨤��Ƥ���.
  !   wu_  :: ���ڥ��ȥ�ǡ���(����Ĵ��ȡ���������ӥ������Ѵ�)
  !   nmr_ :: ��ʿ���ڥ��ȥ�ǡ���(���ȿ� n, �Ӿ��ȿ�����ʬ, ư��)
  !   nr_  :: ��ʿ���ڥ��ȥ�ǡ���(���ȿ� n, ư��)
  !   xyr_ :: 3 �����ʻ����ǡ���(���١����١�ư��)
  !   wr_  :: ��ʿ���ڥ��ȥ�, ư�³ʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(DLon, GradLat, GradLat, DivLon, DivLat, Lapla,..)
  !   ��, ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (wu_, xyz_, wr_, w_, xy_, x_, y_, r_, a_) ��, �����ѿ���
  !   �������ڥ��ȥ�ǡ�������ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _wu      :: ���ڥ��ȥ�ǡ���
  !   _xyr     :: 3 �����ʻ����ǡ���
  !   _xyr_xyr :: 2 �Ĥ�3 �����ʻ����ǡ���, ...
  !
  !=== �ƥǡ����μ��������
  !
  ! * xyr : 3 �����ʻ����ǡ���(���١����١�ư��)
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:im-1,1:jm,0:km). 
  !   * im, jm, km �Ϥ��줾�����, ����, ư�º�ɸ�γʻ������Ǥ���, 
  !     ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * wu : ���ڥ��ȥ�ǡ���
  !   * �ѿ��μ���ȼ����� real(8), dimension((nm+1)*(nm+1),0:lm). 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ�, lm �ϥ����ӥ�����¿�༰�κ��缡��
  !     �Ǥ���, ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !   * ��ʿ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ϴؿ� l_nm, nm_l �ˤ�ä�Ĵ�٤�
  !     ���Ȥ��Ǥ���.
  !
  ! * nmr : ��ʿ���ڥ��ȥ�ǡ������¤�� 3 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,-nm:nm,0:km). 
  !   * �� 1 ��������ʿ���ȿ�, �� 2 �������Ӿ��ȿ�, �� 3 ������ư�º�ɸ��ɽ��. 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, ���֥롼���� wt_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���.
  !
  ! * nr : ���ڥ��ȥ�ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,0:km). 
  !   * �� 1 ��������ʿ���ȿ���ɽ��. nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, 
  !     ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * wr : ��ʿ���ڥ��ȥ�, ư�³ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension((nm+1)*(nm+1),0:km).
  !
  ! * wu_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
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
  ! wu_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
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
  ! xyr_wu, wu_xyr :: ���ڥ��ȥ�ǡ����� 3 �����ʻҥǡ�����
  !                   �֤��Ѵ� (����Ĵ��ȡ��, �����ӥ������Ѵ�)
  ! xyr_wr, wr_xyr :: 3 �����ʻҥǡ����ȿ�ʿ���ڥ��ȥ롦ư�³ʻҥǡ����Ȥ�
  !                   �֤��Ѵ� (����Ĵ��ȡ��)
  ! wr_wu, wu_wr   :: ���ڥ��ȥ�ǡ����ȿ�ʿ���ڥ��ȥ롦ư�³ʻҥǡ����Ȥ�
  !                   �֤��Ѵ� (�����ӥ������Ѵ�)
  ! w_xy, xy_w     :: ���ڥ��ȥ�ǡ����� 2 ������ʿ�ʻҥǡ�����
  !                   �֤��Ѵ�(����Ĵ��ȡ���Ѵ�) 
  ! l_nm, nm_l     :: ���ڥ��ȥ�ǡ����γ�Ǽ���֤����ȿ����Ӿ��ȿ����Ѵ� 
  !
  !==== ��ʬ
  !
  ! wr_DRad_wu          :: ���ڥ��ȥ�ǡ�����ư����ʬ
  !                        ��/��r ����Ѥ�����
  ! wr_DRad2_wu         :: ���ڥ��ȥ�ǡ�����ư����ʬ
  !                        ��^2/��r^2 ����Ѥ�����
  ! wr_DivRad_wu        :: ���ڥ��ȥ�ǡ�����ȯ����ư����ʬ
  !                        1/r^2 ��/��r r^2 = ��/��r + 2/r ����Ѥ�����
  ! wr_RotRad_wu        :: ���ڥ��ȥ�ǡ����˲�ž��ư����ʬ
  !                        1/r ��/��rr = ��/��r + 1/r ����Ѥ�����
  ! wr_Lapla_wu         :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! xyr_GradLon_wu      :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  !                        1/rcos�ա���/�ߦˤ���Ѥ�����
  ! xyr_GradLat_wu      :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  !                        1/r����/�ߦդ���Ѥ�����
  ! wr_DivLon_xyr       :: �ʻҥǡ�����ȯ����������ʬ
  !                        1/rcos�ա���/�ߦˤ���Ѥ�����
  ! wr_DivLat_xyr       :: �ʻҥǡ�����ȯ����������ʬ
  !                        1/rcos�ա���(g cos��)/�ߦդ���Ѥ�����
  ! wr_Div_xyr_xyr_xyr  :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����
  !                        ȯ������Ѥ�����
  ! xyr_Div_xyr_xyr_xyr :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����
  !                        ȯ������Ѥ�����
  ! xyr_RotLon_wu_wu    :: �٥��ȥ��β�ž�η�����ʬ��׻�����
  ! xyr_RotLat_wu_wu    :: �٥��ȥ��β�ž�ΰ�����ʬ��׻�����
  ! wr_RotRad_xyr_xyr   :: �٥��ȥ��β�ž��ư����ʬ��׻�����
  !
  !==== �ȥ�����ݥ�����׻�����ʬ
  !
  ! wu_KxRGrad_wu            :: ���ڥ��ȥ�ǡ����˷�����ʬ
  !                             k��r���� = ��/�ߦˤ���Ѥ�����
  ! xyr_KGrad_wu             :: ���ڥ��ȥ�ǡ����˼�������ʬ
  !                             k���� = cos��/r ��/�ߦ� + sin�բ�/��r ��
  !                             ���Ѥ�����
  ! wu_L2_wu                 :: ���ڥ��ȥ�ǡ�����
  !                             L2 �黻�� = -��ʿ��ץ饷�������Ѥ�����
  ! wu_L2Inv_wu              :: ���ڥ��ȥ�ǡ�����
  !                             L2 �黻�Ҥε� = -�տ�ʿ��ץ饷�����
  !                             ���Ѥ�����
  ! wu_QOperator_wu          :: ���ڥ��ȥ�ǡ����˱黻��
  !                             Q=(k����-1/2(L2 k����+ k����L2)) ��
  !                             ���Ѥ�����
  ! wu_RadRot_xyr_xyr        :: �٥��ȥ� v �α��٤�ư�¥٥��ȥ� r ������
  !                             r��(����v) ��׻�����
  ! wr_RadRotRot_xyr_xyr_xyr :: �٥��ȥ�� v �� r��(���ߢ���v) ��׻�����
  ! wu_Potential2Vector      :: �ȥ�����ݥ�����ݥƥ󥷥�뤫��
  !                             �٥��ȥ���׻�����
  ! wu_Potential2Rotation    :: �ȥ�����ݥ�����ݥƥ󥷥���ɽ�����
  !                             ��ȯ���٥��ȥ��β�ž�γ���ʬ��׻�����
  !
  !==== �ݥ�����/�ȥ������ǥ��ѥ��ڥ��ȥ����
  !
  ! nmr_ToroidalEnergySpectrum_wu, nr_ToroidalEnergySpectrum_wu   :: 
  !     �ȥ�����ݥƥ󥷥�뤫�饨�ͥ륮���ε���Ĵ��ȡ������ʬ��׻�����
  ! nmr_PoloidalEnergySpectrum_wu, nr_PoloidalEnergySpectrum_wu   :: 
  !     �ݥ�����ݥƥ󥷥�뤫�饨�ͥ륮���ε���Ĵ��ȡ������ʬ��׻�����
  !
  !==== ����������
  !
  ! wt_BoundariesTau, wt_BoundariesGrid, wt_Boundaries                   ::
  !     ����ˡ, ����ˡ
  ! wt_TorBoundariesTau, wt_TorBoundariesGrid, wt_TorBoundaries          :: 
  !     ®�٥ȥ�����ݥƥ󥷥��ζ����������ΰ��Ŭ�Ѥ���
  !     (����ˡ,����ˡ)
  ! wz_LaplaPol2Pol_wz, wt_LaplaPol2Pol_wt                               :: 
  !     ����ΰ��®�٥ݥ�����ݥƥ󥷥�릵��^2���������
  !     (�����Ϥ����줾������ӥ����ճʻ���,�����ӥ����շ���)
  ! wt_TorMagBoundariesTau, wt_TorMagBoundariesGrid, wt_TorMagBoundaries ::
  !     ����ȥ�����ݥƥ󥷥��ζ����������ΰ��Ŭ�Ѥ���
  !     (����ˡ, ����ˡ)
  ! wt_PolMagBoundariesTau, wt_PolMagBoundariesGrid, wt_PolMagBoundaries ::
  !     ����ȥ�����ݥƥ󥷥�붭���ζ����������ΰ��Ŭ�Ѥ���
  !     (����ˡ, ����ˡ)
  ! wu_BoundaryTau, wu_BoundaryGrid, wu_Boundary                         ::
  !     ���ΰ�˥ǥ��ꥯ��, �Υ��ޥ󶭳�����Ŭ�Ѥ��� (����ˡ, ����ˡ)
  ! wu_TorBoundaryTau, wu_TorBoundaryGrid, wu_TorBoundary                ::
  !     ���ΰ��®�٥ȥ�����ݥƥ󥷥��ζ�������Ŭ�Ѥ���(����ˡ,����ˡ)
  ! wr_LaplaPol2Pol_wr, wu_LaplaPol2Pol_wu                               ::
  !     ���ΰ��®�٥ݥ�����ݥƥ󥷥�릵��^2���������
  !     (�����Ϥ����줾������ӥ����ճʻ���,�����ӥ����շ���)
  ! wu_TorMagBoundaryTau, wu_TorMagBoundaryGrid, wu_TorMagBoundary       ::
  !    ����ȥ�����ݥƥ󥷥��ζ���������ΰ��Ŭ�Ѥ���(����ˡ, ����ˡ)
  ! wu_PolMagBoundaryTau, wu_PolMagBoundaryGrid, wu_PolMagBoundary       ::
  !     ����ȥ�����ݥƥ󥷥�붭���ζ���������ΰ��Ŭ�Ѥ���
  !     (����ˡ, ����ˡ)
  !
  !==== ��ʬ��ʿ��(3 �����ǡ���)
  !
  ! IntLonLatRad_xyz, AvrLonLatRad_xyz :: 3 �����ʻ����ǡ�����
  !                                       ���ΰ���ʬ�����ʿ��
  ! z_IntLonLat_xyz, z_AvrLonLat_xyz   :: 3 �����ʻ����ǡ�����
  !                                       ���ٷ���(��ʿ������)��ʬ�����ʿ��
  ! y_IntLonRad_xyz, y_AvrLonRad_xyz   :: 3 �����ʻ����ǡ�����
  !                                       ����ư����ʬ�����ʿ��
  ! z_IntLatRad_xyz, z_AvrLatRad_xyz   :: 3 �����ʻ����ǡ�����
  !                                       ����ư��(�Ҹ���)��ʬ�����ʿ��
  ! yz_IntLon_xyz, yz_AvrLon_xyz       :: 3 �����ʻ����ǡ�����
  !                                       ����������ʬ�����ʿ��
  ! xz_IntLat_xyz, xz_AvrLat_xyz       :: 3 �����ʻ����ǡ�����
  !                                       ����������ʬ�����ʿ��
  ! xz_IntRad_xyz, xz_AvrRad_xyz       :: 3 �����ʻ����ǡ�����
  !                                       ư��������ʬ�����ʿ��
  ! IntLonLatRad_xyr, AvrLonLatRad_xyr :: 3 �����ʻ����ǡ�����
  !                                       ���ΰ���ʬ�����ʿ��
  ! z_IntLonLat_xyr, z_AvrLonLat_xyr   :: 3 �����ʻ����ǡ�����
  !                                       ���ٷ���(��ʿ������)��ʬ�����ʿ��
  ! y_IntLonRad_xyr, y_AvrLonRad_xyr   :: 3 �����ʻ����ǡ�����
  !                                       ����ư����ʬ�����ʿ��
  ! z_IntLatRad_xyr, z_AvrLatRad_xyr   :: 3 �����ʻ����ǡ�����
  !                                       ����ư��(�Ҹ���)��ʬ�����ʿ��
  ! yz_IntLon_xyr, yz_AvrLon_xyr       :: 3 �����ʻ����ǡ�����
  !                                       ����������ʬ�����ʿ��
  ! xz_IntLat_xyr, xz_AvrLat_xyr       :: 3 �����ʻ����ǡ�����
  !                                       ����������ʬ�����ʿ��
  ! xz_IntRad_xyr, xz_AvrRad_xyr       :: 3 �����ʻ����ǡ�����
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
  use wa_module
  use au_module, r_Rad => g_R, r_RAD_WEIGHT => g_R_WEIGHT, &
                 wu_wr => au_ag, wr_wu => ag_au, &
                 r_DRad_t => g_Dr_u, wr_DRad_wu => ag_Dr_au, &
                 r_DRad2_t => g_Dr2_u, wr_DRad2_wu => ag_Dr2_au
  implicit none
  private

  public wu_Initial

  public x_Lon, x_Lon_Weight
  public y_Lat, y_Lat_Weight
  public r_Rad, r_Rad_Weight
  public l_nm, nm_l
  public xy_Lon, xy_Lat
  public xyr_Lon, xyr_Lat, xyr_Rad
  public wr_Rad
  public wu_VMiss

  public w_xy, xy_w
  public wr_DRad_wu, r_DRad_t, wr_wu, wu_wr
  public wr_DRad2_wu, r_DRad2_t
  public xyr_wu, wu_xyr, xyr_wr, wr_xyr
  public wr_DivRad_wu, wr_RotRad_wu, wr_Lapla_wu
  public xyr_GradLon_wu, xyr_gradlat_wu
  public wr_DivLon_xyr, wr_DivLat_xyr
  public wr_Div_xyr_xyr_xyr, xyr_Div_xyr_xyr_xyr
  public xyr_RotLon_wu_wu, xyr_RotLat_wu_wu, wr_RotRad_xyr_xyr

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

  public wu_KxRGrad_wu, xyr_KGrad_wu, wu_L2_wu, wu_L2Inv_wu, wu_QOperator_wu
  public wu_RadRot_xyr_xyr, wr_RadRotRot_xyr_xyr_xyr
  public wu_Potential2vector, wu_Potential2Rotation

  public nmr_ToroidalEnergySpectrum_wu, nr_ToroidalEnergySpectrum_wu
  public nmr_PoloidalEnergySpectrum_wu, nr_PoloidalEnergySpectrum_wu

  public wu_Boundary, wu_TorBoundary, wr_LaplaPol2Pol_wr
  public wu_TormagBoundary, wu_PolmagBoundary

  public wu_BoundaryTau, wu_TorBoundaryTau
  public wu_TormagBoundaryTau, wu_PolmagBoundaryTau

  public wu_BoundaryGrid, wu_TorBoundaryGrid, wu_LaplaPol2PolGrid_wu
  public wu_TormagBoundaryGrid, wu_PolmagBoundaryGrid

  interface wu_Boundary
     module procedure wu_BoundaryTau
  end interface

  interface wu_TorBoundary
     module procedure wu_TorBoundaryTau
  end interface

  interface wu_TorMagBoundary
     module procedure wu_TorMagBoundaryTau
  end interface

  interface wu_PolMagBoundary
     module procedure wu_PolMagBoundaryTau
  end interface

  integer            :: im=64, jm=32, km=16  ! �ʻ���������(����, ����, ư��)
  integer            :: nm=21, lm=16         ! �����ȿ�������(��ʿ, ư��)
  real(8)            :: ra=1.0               ! ��Ⱦ��
  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), dimension(:,:,:), allocatable :: xyr_LON, xyr_LAT, xyr_RAD ! ��ɸ
  real(8), dimension(:,:), allocatable   :: wr_RAD                    ! ��ɸ
  integer, dimension(:), allocatable     :: nd             ! �Ť� r^n �λؿ�

  real(8) :: wu_VMiss = -999.0        ! ��»��

  save im, jm, km, nm, lm, ra, nd

  contains
  !--------------- ����� -----------------
   subroutine wu_Initial(i,j,k,n,l,r,np,wa_init)
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
           call wa_Initial(nm,im,jm,km+1,np)
        else
           call wa_Initial(nm,im,jm,km+1)
        endif
     endif

     allocate(nd((nm+1)*(nm+1)))

     do nn=0,nm
        do mm=-nn,nn
!!$           nd(l_nm(nn,mm)) = nn                 ! ��ʿ n ���� r^n ����
           nd(l_nm(nn,mm)) = (1-(-1)**nn)/2     ! ����� r, ������ r^0
        enddo
     enddo

     call au_Initial(km,lm,ra,nd)

     allocate(xyr_Lon(0:im-1,1:jm,0:km))
     allocate(xyr_Lat(0:im-1,1:jm,0:km))
     allocate(xyr_Rad(0:im-1,1:jm,0:km))

     allocate(wr_Rad((nm+1)*(nm+1),0:km))

     xyr_Lon = spread(xy_Lon,3,km+1)
     xyr_Lat = spread(xy_Lat,3,km+1)
     xyr_Rad = spread(spread(r_Rad,1,jm),1,im)

     wr_Rad = spread(r_Rad,1,(nm+1)*(nm+1))

     r_Rad_Weight = r_Rad_Weight * r_Rad**2       ! r^2 dr ����ʬ�Ť�

   end subroutine wu_initial

  !--------------- �����Ѵ� -----------------

    function xyr_wu(wu)
      !
      ! ���ڥ��ȥ�ǡ������� 3 �����ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���
      real(8), dimension(0:im-1,1:jm,0:km)               :: xyr_wu
      !(out) 3 �������ٰ���ư�³ʻ����ǡ���

      xyr_wu = xya_wa(wr_wu(wu))

    end function xyr_wu

    function wu_xyr(xyr)
      !
      ! 3 �����ʻ����ǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���
      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wu_xyr
      !(out) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      wu_xyr = wu_wr(wa_xya(xyr))

    end function wu_xyr

    function xyr_wr(wr)
      !
      ! ��ʿ���ڥ��ȥ롦ư�³ʻ����ǡ������� 3 �����ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:km), intent(in) :: wr
      !(in) 2 ��������Ĵ��ȡ�����ڥ��ȥ롦ư�³ʻ����ǡ���
      real(8), dimension(0:im-1,1:jm,0:km)               :: xyr_wr
      !(out) 3 �������ٰ���ư�³ʻ����ǡ���

      xyr_wr = xya_wa(wr)

    end function xyr_wr

    function wr_xyr(xyr)
      !
      ! 3 �����ʻҥǡ��������ʿ���ڥ��ȥ롦ư�³ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���
      real(8), dimension((nm+1)*(nm+1),0:km)             :: wr_xyr
      !(out) 2 ��������Ĵ��ȡ�����ڥ��ȥ롦ư�³ʻ����ǡ���

      wr_xyr = wa_xya(xyr)

    end function wr_xyr

  !--------------- ��ʬ�׻� -----------------
    function wr_DivRad_wu(wu)
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
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:km)             :: wr_DivRad_wu
      !(out) ȯ����ư����ʬ����Ѥ��줿��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���

!!$      wr_DivRad_wu = wr_DRad_wu(wu) + 2/wr_Rad*wr_wu(wu)
      wr_DivRad_wu = wr_Drad_wu(wu_wr(wr_Rad**2*wr_wu(wu)))/wr_Rad**2

    end function wr_DivRad_wu

    function wr_RotRad_wu(wu)
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
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:km)             :: wr_RotRad_wu
      !(out) ��ž��ư����ʬ����Ѥ��줿��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���

      wr_RotRad_wu = wr_Drad_wu(wu) + wr_wu(wu)/wr_Rad
!!$      wr_RotRad_wu = wr_Drad_wu(wu_wr(wr_Rad*wr_wu(wu)))/wr_Rad

    end function wr_RotRad_wu

    function wr_RotRad_wr(wr)
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
      real(8), dimension((nm+1)*(nm+1),0:km), intent(in) :: wr
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:km)             :: wr_RotRad_wr
      !(out) ��ž��ư����ʬ����Ѥ��줿��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���

!!$      wr_RotRad_wu = wr_Drad_wu(wu) + wr_wu(wu)/wr_Rad
      wr_RotRad_wr = wr_Drad_wu(wu_wr(wr_Rad*wr))/wr_Rad

    end function wr_RotRad_wr

    function wr_Lapla_wu(wu)
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷����
      !
      !     ��^2 =   1/r^2 cos^2�ա���^2/�ߦ�^2 
      !            + 1/r^2 cos�ա���/�ߦ�(cos�բ�/�ߦ�) 
      !            + 1/r^2 ��/��r (r^2 ��/��r) 
      !              (��^2/��r^2 + 2/r��/��r)
      !
      ! ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:km)             :: wr_Lapla_wu
      !(out) ��ץ饷�������Ѥ��줿��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���

!!$      wr_Lapla_wu = wr_DivRad_wu(wu_wr(wr_Drad_wu(wu))) &
!!$                   + wr_wu(wa_Lapla_wa(wu))/wr_Rad**2
      wr_Lapla_wu = wr_DRad2_wu(wu) + 2 * wr_Drad_wu(wu)/wr_Rad & 
                   + wr_wu(wa_Lapla_wa(wu))/wr_Rad**2

    end function wr_Lapla_wu

    function xyr_GradLon_wu(wu)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/rcos�ա���/�ߦ�
      ! ����Ѥ�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyr_GradLon_wu
      !(out) ���۷�������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      xyr_GradLon_wu = xya_GradLon_wa(wr_wu(wu))/xyr_Rad

    end function xyr_GradLon_wu

    function xyr_GradLat_wu(wu) 
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/r ��/�ߦ� ����Ѥ�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyr_GradLat_wu
      !(out) ���۷�������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      xyr_GradLat_wu = xya_GradLat_wa(wr_wu(wu))/xyr_Rad

    end function xyr_GradLat_wu

    function wr_DivLon_xyr(xyr)
      ! 
      ! �ʻ����ǡ�����ȯ����������ʬ 1/rcos�ա���/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������֤�.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension((nm+1)*(nm+1),0:km)       :: wr_DivLon_xyr
      !(out) ȯ����������ʬ����Ѥ��줿��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���

      wr_DivLon_xyr = wa_DivLon_xya(xyr/xyr_Rad)

    end function wr_DivLon_xyr

    function wr_DivLat_xyr(xyr)
      !
      ! �ʻҥǡ�����ȯ����������ʬ 1/rcos�ա���(f cos��)/�ߦ� ��
      ! ���Ѥ��������ڥ��ȥ�ǡ������֤�.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension((nm+1)*(nm+1),0:km)       :: wr_DivLat_xyr
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension((nm+1)*(nm+1),0:km)     :: wr_Div_xyr_xyr_xyr
      !(out) �٥��ȥ���ȯ��

      wr_Div_xyr_xyr_xyr =   wr_DivLon_xyr(xyr_Vlon) &
                           + wr_DivLat_xyr(xyr_Vlat) &
                           + wr_DivRad_wu(wu_xyr(xyr_Vrad))

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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension(0:im-1,1:jm,0:km)             :: xyr_Div_xyr_xyr_xyr
      !(out) �٥��ȥ���ȯ��

      xyr_Div_xyr_xyr_xyr &
           = xyr_Rad/cos(xyr_Lat) &
                * xyr_wr(wr_Div_xyr_xyr_xyr(xyr_VLon*cos(xyr_Lat)/xyr_Rad,  &
                                            xyr_VLat*cos(xyr_Lat)/xyr_Rad,  &
                                            xyr_VRad*cos(xyr_Lat)/xyr_Rad ))&
             + xyr_VLat*tan(xyr_Lat)/xyr_Rad &
             + xyr_VRad/xyr_Rad

    end function xyr_Div_xyr_xyr_xyr

    function xyr_RotLon_wu_wu(wu_Vrad,wu_Vlat) 
      !
      ! �٥��ȥ���ư����ʬ, ������ʬ�Ǥ����� 1, 2 ���� Vrad, Vlat ����
      ! ��ž�η�����ʬ 
      !
      !    1/r ��Vrad/�ߦ�-1/r ��(r Vlat)/��r ��׻�����.
      !
      ! ��׻�����
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyr_RotLon_wu_wu
      !(out) �٥��ȥ��β�ž�η�����ʬ

        xyr_RotLon_wu_wu =   xyr_GradLat_wu(wu_Vrad) &
                           - xyr_wr(wr_RotRad_wu(wu_Vlat))

    end function xyr_RotLon_wu_wu

    function xyr_RotLat_wu_wu(wu_Vlon,wu_Vrad) 
      !
      ! �٥��ȥ��η�����ʬ, ư����ʬ�Ǥ����� 1, 2 ���� Vlon, Vrad ����
      ! ��ž�ΰ�����ʬ 
      !
      !    1/r ��(r Vlon)/��r - 1/rcos�ա���Vrad/�ߦ�
      !
      ! ��׻�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyr_RotLat_wu_wu
      !(out) �٥��ȥ��β�ž�ΰ�����ʬ

        xyr_RotLat_wu_wu =   xyr_wr(wr_RotRad_wu(wu_Vlon)) &
                           - xyr_GradLon_wu(wu_Vrad) 

    end function xyr_RotLat_wu_wu

    function wr_RotRad_xyr_xyr(xyr_Vlat,xyr_Vlon) 
      !
      ! �٥��ȥ�ΰ�����ʬ, ������ʬ�Ǥ����� 1, 2 ���� Vlat, Vlon ���Ф���
      ! �٥��ȥ��β�ž��ư����ʬ 
      !
      !    1/rcos�ա���Vlat/�ߦ� - 1/rcos�ա���(Vlon cos��)/�ߦ�
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension((nm+1)*(nm+1),0:km)     :: wr_RotRad_xyr_xyr
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(1:jm,0:km)  :: yr_IntLon_xyr
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,0:km)  :: xr_IntLat_xyr
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,1:jm)  :: xy_IntRad_xyr
      !(out) ư����ʬ���줿 2 �������ٰ���(��ʿ, ����)�ʻ����ǡ���

      integer :: k

      xy_IntRad_xyr = 0.0d0
      do k=0,km
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1)     :: x_IntLatRad_xyr
      !(out) ����ư��(�Ҹ���)��ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: j, k

      x_IntLatRad_xyr = 0.0D0
      do k=0,km
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(1:jm)       :: y_IntLonRad_xyr
      !(out) ����ư��(���ٱ�)��ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: i, k

      y_IntLonRad_xyr = 0.0D0
      do k=0,km
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:km)     :: r_IntLonLat_xyr
      !(out) ���ٷ���(��ʿ, ����)��ʬ���줿 1 ����ư�³ʻ����ǡ���

      integer :: i, j

      r_IntLonLat_xyr = 0
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr 
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8)                     :: IntLonLatRad_xyr 
      !(out) ������ʬ��

      integer :: i, j, k

      IntLonLatRad_xyr = 0
      do k=0,km
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
      real(8), dimension(jm,0:km), intent(in) :: yr
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(0:km)  :: r_IntLat_yr
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
      real(8), dimension(1:jm,0:km), intent(in) :: yr
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(1:jm)  :: y_IntRad_yr
      !(out) ư����ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: k

      y_IntRad_yr = 0.0d0
      do k=0,km
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
      real(8), dimension(1:jm,0:km), intent(in) :: yr
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���
      real(8)                   :: IntLatRad_yr
      !(out) ��ʬ��
      integer :: j, k

      IntLatRad_yr = 0.0D0
      do k=0,km
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
      real(8), dimension(0:im-1,0:km), intent(in) :: xr
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8), dimension(0:km)  :: r_IntLon_xr
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
      real(8), dimension(0:im-1,0:km), intent(in) :: xr
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8), dimension(0:im-1)  :: x_IntRad_xr
      !(out) ư����ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: k

      x_IntRad_xr = 0.0d0
      do k=0,km
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
      real(8), dimension(0:im-1,0:km), intent(in) :: xr
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8)                                 :: IntLonRad_xr
      !(out) ��ʬ��

      integer :: i, k

      IntLonRad_xr = 0.0D0
      do k=0,km
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
      real(8), dimension(0:km), intent(in) :: z
      !(in) 1 ����ư�³ʻ����ǡ���

      real(8)                              :: IntRad_r
      !(out) ��ʬ��

      integer :: k

      IntRad_r = 0.0d0
      do k=0,km
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(1:jm,0:km)  :: yr_AvrLon_xyr
      !(out) ��������(�Ӿ�)ʿ�Ѥ��줿 2 �����Ҹ��̳ʻ����ǡ���

      yr_AvrLon_xyr = yr_IntLon_xyr(xyr)/sum(x_Lon_Weight)

    end function yr_AvrLon_xyr

    function xr_AvrLat_xyr(xyr)  ! ������ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ���������ʿ��.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��� ��f(��,��,r)cos�� d��/2 ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,0:km)  :: xr_AvrLat_xyr
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,1:jm)  :: xy_AvrRad_xyr          
      ! ��ʿ�ʻ����ǡ���
      !(out) ư��ʿ�Ѥ��줿 2 �������ٰ���(��ʿ, ����)�ʻ����ǡ���

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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:km)     :: r_AvrLonLat_xyr
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
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
      real(8), dimension(1:jm,0:km), intent(in) :: yr
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(0:km)  :: r_AvrLat_yr
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
      real(8), dimension(1:jm,0:km), intent(in) :: yr
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
      real(8), dimension(1:jm,0:km), intent(in) :: yr
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
      real(8), dimension(0:im-1,0:km), intent(in) :: xr
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8), dimension(0:km)  :: r_AvrLon_xr 
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
      real(8), dimension(0:im-1,0:km), intent(in) :: xr
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
      real(8), dimension(0:im-1,0:km), intent(in) :: xr    
      ! (in)2 �����ʻ����ǡ���
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
      real(8), dimension(0:im-1,0:km), intent(in) :: z
      !(in) 1 ����ư�³ʻ����ǡ���
      real(8)                                 :: AvrRad_r
      !(out) ʿ����

      AvrRad_r = IntRad_r(z)/sum(r_Rad_Weight)

    end function AvrRad_r

  !--------------- �ݥ�����/�ȥ������ǥ�����ʬ -----------------

    function wu_KxRGrad_wu(wu)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˷�����ʬ k��r���� = ��/�ߦˤ���Ѥ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wu_KxRGrad_wu
      !(out) ������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wu_KxRGrad_wu =  wa_Dlon_wa(wu)

    end function wu_KxRGrad_wu

    function xyr_KGrad_wu(wu)    ! k���� = cos��/r ��/�ߦ� + sin�բ�/��r
      !
      ! ���ϥ��ڥ��ȥ�ǡ������б�����ʻҥǡ����˼�������ʬ 
      !
      !    k���� = cos��/r ��/�ߦ� + sin�բ�/��r 
      !
      ! ����Ѥ������ʻҥǡ������֤����. 
      ! �����ǥ٥��ȥ� k �ϵ���濴�����̶˸�����ñ�̥٥��ȥ�Ǥ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyr_KGrad_wu
      !(out) ��������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      xyr_KGrad_wu =  cos(xyr_Lat)*xyr_GradLat_wu(wu) &
                    + sin(xyr_Lat)*xyr_wr(wr_DRad_wu(wu))

    end function xyr_KGrad_wu

    function wu_L2_wu(wu)
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
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wu_L2_wu
      !(out) L^2 �黻�Ҥ���Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wu_L2_wu = -wa_Lapla_wa(wu)

    end function wu_L2_wu

    function wu_L2Inv_wu(wu)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� L^2 �黻�Ҥεձ黻(-�տ�ʿ��ץ饷����)��
      ! ���Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� L^2 �黻�Ҥ���Ѥ�����ؿ� wu_L2_wu �εշ׻���
      ! �Ԥ��ؿ��Ǥ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wu_L2Inv_wu
      !(out) L^2 �黻�Ҥεձ黻����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wu_L2Inv_wu = -wa_LaplaInv_wa(wu)

    end function wu_L2Inv_wu

    function wu_QOperator_wu(wu)
      !
      ! ���ϥ��ڥ��ȥ�ǡ������б�����ʻ����ǡ����˱黻�� 
      !
      !    Q=(k����-1/2(L2 k����+ k����L2)) 
      !
      ! ����Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ����֤����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wu_QOperator_wu
      !(out) Q �黻�Ҥ���Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wu_QOperator_wu = &
             wu_xyr(xyr_KGrad_wu(wu) - xyr_KGrad_wu(wu_L2_wu(wu))/2) &
           - wu_L2_wu(wu_xyr(xyr_KGrad_wu(wu)))/2

    end function wu_QOperator_wu

    function wu_RadRot_xyr_xyr(xyr_VLON,xyr_VLAT)  ! r��(����v)
      !
      ! �٥��ȥ�α��٤�ư�¥٥��ȥ������ r��(����v) ��׻�����.
      !
      ! �� 1, 2 ����(v[��], v[��])�����줾��٥��ȥ�η�����ʬ, ������ʬ��ɽ��.
      !
      !    r��(����v) = 1/cos�ա���v[��]/�ߦ� - 1/cos�ա���(v[��] cos��)/�ߦ�
      !
      ! �Υ��ڥ��ȥ� �ǡ������֤����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_VLON
      !(in) �٥��ȥ�η�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_VLAT
      !(in) �٥��ȥ�ΰ�����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm)     :: wu_RadRot_xyr_xyr
      !(out) �٥��ȥ�α��٤�ư�¥٥��ȥ������

      wu_RadRot_xyr_xyr = wu_wr(wa_DivLon_xya(xyr_VLAT) &
                                - wa_DivLat_xya(xyr_VLON))
      
    end function wu_RadRot_xyr_xyr

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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_VLON
      !(in) �٥��ȥ�η�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_VLAT
      !(in) �٥��ȥ�ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_VRAD
      !(in) �٥��ȥ��ư����ʬ

      real(8), dimension((nm+1)*(nm+1),0:km)     :: wr_RadRotRot_xyr_xyr_xyr
      !(out) �٥��ȥ� v �� r��(���ߢ���v) 

      wr_RadRotRot_xyr_xyr_xyr = &
                   wr_RotRad_wr( &
                      wa_DivLon_xya(xyr_VLON)+ wa_DivLat_xya(xyr_VLAT)) &
             - wa_Lapla_wa(wr_xyr(xyr_VRAD/xyr_RAD))

    end function wr_RadRotRot_xyr_xyr_xyr

    subroutine wu_Potential2Vector(&
         xyr_VLON,xyr_VLAT,xyr_VRAD,wu_TORPOT,wu_POLPOT)
      !
      ! �ȥ�����ݥ�����ݥƥ󥷥�릷,����ɽ�������ȯ���٥��ȥ��
      !
      !     v = ��x(��r) + ��x��x(��r) 
      !
      ! �γ���ʬ��׻�����
      !
      real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VLON
      !(out) �٥��ȥ��η�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VLAT
      !(out) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VRAD
      !(out) �٥��ȥ���ư����ʬ

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      xyr_VLON =   xyr_RAD * xyr_GradLat_wu(wu_TORPOT) &
                 + xya_GradLon_wa(wr_RotRad_wu(wu_POLPOT))
      xyr_VLAT = - xyr_RAD * xyr_GradLon_wu(wu_TORPOT) &
                 + xya_GradLat_wa(wr_RotRad_wu(wu_POLPOT))
      xyr_VRAD = xyr_wu(wu_L2_wu(wu_POLPOT))/xyr_RAD

    end subroutine wu_Potential2Vector

    subroutine wu_Potential2Rotation(&
       xyr_RotVLON,xyr_RotVLAT,xyr_RotVRAD,wu_TORPOT,wu_POLPOT)
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(OUT) :: xyr_RotVLON
      !(out) ��ž�η�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(OUT) :: xyr_RotVLAT
      !(out) ��ž�ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(OUT) :: xyr_RotVRAD
      !(out) ��ž��ư����ʬ

      ! ���ϥ٥��ȥ���ɽ���ݥƥ󥷥��
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      call wu_Potential2Vector( &
           xyr_RotVLON,xyr_RotVLAT,xyr_RotVRAD, &
           -wu_wr(wr_Lapla_wu(wu_POLPOT)), wu_TORPOT)

    end subroutine wu_Potential2Rotation

  !--------------- �ݥ�����/�ȥ������ǥ��ѥ��ڥ��ȥ���� ----------------

    function nmr_ToroidalEnergySpectrum_wu(wu_TORPOT)
      !
      ! �ȥ�����ݥƥ󥷥�뤫��, �ȥ����륨�ͥ륮����
      ! ����Ĵ��ȡ�����ȿ� n, �Ӿ��ȿ� m �γ���ʬ��׻�����
      !
      !  * ���ȿ� n, �Ӿ��ȿ� m �Υȥ�����ݥƥ󥷥��Υ��ڥ��ȥ���ʬ
      !    ��(n,m,r)�������ȿ� n, �Ӿ��ȿ� m ��ʬ�Υȥ����륨�ͥ륮��
      !    ���ڥ��ȥ��  (1/2)n(n+1)4��r^2��(n,m,r)^2  �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�ư����ʬ�������(r^2�νŤ�̵��)
      !    �������Ǥ������ͥ륮����������.
      !    
      !  * �ǡ�����¸�ߤ��ʤ����ȿ� n, �Ӿ��ȿ� m ������ˤϷ�»�ͤ���Ǽ�����.
      !    wu_VMiss �ˤ�ä�����Ǥ��� (����ͤ� -999.0)
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,-nm:nm,0:km) :: nmr_ToroidalEnergySpectrum_wu
      !(out) ���ͥ륮�����ڥ��ȥ�ȥ�������ʬ

      real(8), dimension((nm+1)*(nm+1),0:km) ::wr_DATA   ! ����ΰ�
      integer :: n, m

      nmr_ToroidalEnergySpectrum_wu = wu_VMiss

      wr_DATA = wr_wu(wu_TORPOT)
      do n=0,nm
         do m=-n,n
            nmr_ToroidalEnergySpectrum_wu(n,m,:) &
              = 0.5 * n*(n+1)* (4*pi) * r_Rad**2 &
                * wr_DATA(l_nm(n,m),:)**2
         enddo
      enddo

    end function nmr_ToroidalEnergySpectrum_wu

    function nr_ToroidalEnergySpectrum_wu(wu_TORPOT)
      !
      ! �ȥ�����ݥƥ󥷥�뤫��, �ȥ����륨�ͥ륮����
      ! ����Ĵ��ȡ�����ȿ��γ���ʬ��׻�����.
      !
      !  * ���ȿ� n, �Ӿ��ȿ� m �Υȥ�����ݥƥ󥷥��Υ��ڥ��ȥ���ʬ
      !    ��(n,m,r)�������ȿ� n ��ʬ�Υȥ����륨�ͥ륮�����ڥ��ȥ��
      !    ��[m=-n]^n(1/2)n(n+1)4��r^2��(n,m,r)^2 �ȷ׻������.
      !
      ! * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�ư����ʬ�������(r^2�νŤ�̵��)
      !    �������Ǥ������ͥ륮����������.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,0:km) :: nr_ToroidalEnergySpectrum_wu 
      !(out) ���ͥ륮�����ڥ��ȥ�ȥ�������ʬ

      real(8), dimension((nm+1)*(nm+1),0:km) ::wr_DATA   ! ����ΰ�
      integer :: n, m

      wr_DATA = wr_wu(wu_TORPOT)
      do n=0,nm
         nr_ToroidalEnergySpectrum_wu(n,:) &
              = 0.5 * n*(n+1)* (4*pi) * r_Rad**2 &
                * sum(wr_Data(l_nm(n,(/(m,m=-n,n)/)),:)**2,1)
      enddo

    end function nr_ToroidalEnergySpectrum_wu

    function nmr_PoloidalEnergySpectrum_wu(wu_POLPOT)
      !
      ! �ݥ�����ݥƥ󥷥�뤫��, �ݥ����륨�ͥ륮����
      ! ����Ĵ��ȡ�����ȿ� n, �Ӿ��ȿ� m �γ���ʬ��׻�����.
      !
      !  * ���ȿ� n, �Ӿ��ȿ� m �Υݥ�����ݥƥ󥷥��Υ��ڥ��ȥ���ʬ
      !    ��(n,m,r)�������ȿ� n, �Ӿ��ȿ� m ��ʬ�Υݥ����륨�ͥ륮��
      !    ���ڥ��ȥ�� 
      !
      !      (1/2)n(n+1)4��r^2{[d(r��(n,m,r))/dr]^2 + n(n+1)��(n,m,r)^2} 
      !
      !    �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�ư����ʬ�������(r^2�νŤ�̵��)
      !    �������Ǥ������ͥ륮����������.
      !
      !  * �ǡ�����¸�ߤ��ʤ����ȿ� n, �Ӿ��ȿ� m ������ˤϷ�»�ͤ���Ǽ�����.
      !    ��»�ͤ��ͤϥ⥸�塼���ѿ� wu_VMiss �ˤ�ä�����Ǥ���
      !    (����ͤ� -999.0)
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,-nm:nm,0:km) :: nmr_PoloidalEnergySpectrum_wu 
      !(out) ���ͥ륮�����ڥ��ȥ�ݥ�������ʬ


      real(8), dimension((nm+1)*(nm+1),0:km) ::wr_DATA1   ! ����ΰ�
      real(8), dimension((nm+1)*(nm+1),0:km) ::wr_DATA2   ! ����ΰ�
      integer :: n, m

      nmr_PoloidalEnergySpectrum_wu = wu_VMiss

      wr_Data1 = wr_wu(wu_POLPOT)
      wr_Data2 = wr_Rad*wr_DRad_wu(wu_POLPOT) &           ! d(r��)/dr
               + wr_wu(wu_POLPOT)                         ! = rd��/dr+��

      do n=0,nm
         do m=-n,n
            nmr_PoloidalEnergySpectrum_wu(n,m,:) = &
                 + 0.5* n*(n+1)* (4*pi) &
                 *( wr_Data2(l_nm(n,m),:)**2  &
                   + n*(n+1)*wr_Data1(l_nm(n,m),:)**2 )
         enddo
      enddo

    end function nmr_PoloidalEnergySpectrum_wu

    function nr_PoloidalEnergySpectrum_wu(wu_POLPOT)
      !
      ! �ݥ�����ݥƥ󥷥�뤫��, �ݥ����륨�ͥ륮����
      ! ����Ĵ��ȡ�����ȿ��γ���ʬ��׻�����
      !
      !  * ���ȿ� n, �Ӿ��ȿ� m �Υݥ�����ݥƥ󥷥��Υ��ڥ��ȥ���ʬ
      !    ��(n,m,r)�������ȿ� n ��ʬ�Υݥ����륨�ͥ륮�����ڥ��ȥ��
      !
      !      ��[m=-n]^n ((1/2)n(n+1)4��r^2{[d(r��(n,m,r))/dr]^2 
      !                 + n(n+1)��(n,m,r)^2} 
      !
      !    �ȷ׻������.
      !
      !  * ���Ƥ����ȿ����Ф��ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�ư����ʬ�������
      !    (r^2�νŤ�̵��)�������Ǥ������ͥ륮����������.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,0:km) :: nr_PoloidalEnergySpectrum_wu
      !(out) ���ͥ륮�����ڥ��ȥ�ݥ�������ʬ

      real(8), dimension((nm+1)*(nm+1),0:km) ::wr_DATA1   ! ����ΰ�
      real(8), dimension((nm+1)*(nm+1),0:km) ::wr_DATA2   ! ����ΰ�
      integer :: n, m

      wr_Data1 = wr_wu(wu_POLPOT)
      wr_Data2 = wr_Rad*wr_DRad_wu(wu_POLPOT) &           ! d(r��)/dr
               + wr_wu(wu_POLPOT)                         ! = rd��/dr+��

      do n=0,nm
         nr_PoloidalEnergySpectrum_wu(n,:) = &
              + 0.5* n*(n+1)* (4*pi) &
              *( sum(wr_Data2(l_nm(n,(/(m,m=-n,n)/)),:)**2,1)  &
                + n*(n+1)*sum(wr_Data1(l_nm(n,(/(m,m=-n,n)/)),:)**2,1) )
      enddo

    end function nr_PoloidalEnergySpectrum_wu

  !--------------- ���������� -----------------

    subroutine wu_BoundaryTau(wu,value,cond)
      !
      ! ���ڥ��ȥ�ǡ����˥ǥ��ꥯ�졦�Υ��ޥ󶭳�����Ŭ�Ѥ���
      ! Chebyshev ���֤Ǥζ������Ŭ��(����ˡ)
      !
      ! �����ӥ����ն��֤ˤ����ƶ��������������٤��⼡�η�����
      ! ������ˡ��ȤäƤ���(����ˡ).
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)      :: wu
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
            call au_BoundaryTau_D(wu,value)
         else
            call au_BoundaryTau_D(wu)
         endif
         return
      endif

      select case(cond)
      case ('N')
         if (present(value)) then
            call au_BoundaryTau_N(wu,value)
         else
            call au_BoundaryTau_N(wu)
         endif
      case ('D')
         if (present(value)) then
            call au_BoundaryTau_D(wu,value)
         else
            call au_BoundaryTau_D(wu)
         endif
      case default
         call MessageNotify('E','wu_BoundaryTau','B.C. not supported')
      end select

    end subroutine wu_BoundaryTau

    subroutine wu_BoundaryGrid(wu,value,cond)
      !
      ! ���ڥ��ȥ�ǡ����˥ǥ��ꥯ�졦�Υ��ޥ󶭳�����Ŭ�Ѥ���
      ! �¶��֤Ǥζ������Ŭ��
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! wu_Initial �ˤ����ꤹ������ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��
      ! ���������Ƥ���ɬ�פ�����.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)      :: wu
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
            call au_BoundaryGrid_D(wu,value)
         else
            call au_BoundaryGrid_D(wu)
         endif
         return
      endif

      select case(cond)
      case ('N')
         if (present(value)) then
            call au_BoundaryGrid_N(wu,value)
         else
            call au_BoundaryGrid_N(wu)
         endif
      case ('D')
         if (present(value)) then
            call au_BoundaryGrid_D(wu,value)
         else
            call au_BoundaryGrid_D(wu)
         endif
      case default
         call MessageNotify('E','wu_BoundaryGrid','B.C. not supported')
      end select

    end subroutine wu_BoundaryGrid

    subroutine wu_TorBoundaryTau(wu_TORPOT,value,cond,new) 
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
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)      :: wu_TORPOT
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
      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wu_data
      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_data
      logical                                 :: rigid        ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l
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
            call MessageNotify('E','wu_TorBoundaryTau','B.C. not supported')
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

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid ) then
            do l=0,lm
               wu_data = 0.0D0
               wu_data(:,l) = 1.0D0
               wr_data = wr_wu(wu_data)
               alu(:,lm,l) = wr_data(:,0)
            enddo
         else
            do l=0,lm
               wu_data = 0.0D0
               wu_data(:,l) = 1.0D0
               wr_data = wr_DRad_wu(wu_data) - wr_wu(wu_data)/wr_Rad
               alu(:,lm,l) = wr_data(:,0)
            enddo
         endif

         call ludecomp(alu,kp)

         if ( rigid .AND. present(value) ) then 
            call MessageNotify('M','wu_TorBoundaryTau',&
                 'Toroidal potential at k=0 was given by the optional variable.')
         else if ( rigid .AND. (.NOT.present(value)) ) then
            call MessageNotify('M','wu_TorBoundaryTau',&
                 'Toroidal potential at k=0 was set to zero.')
         else if ( (.NOT. rigid) .AND. present(value) ) then
            call MessageNotify('W','wu_TorBoundaryTau',&
                 'Boundary value k=0 cannot be set under stress-free condition.')
         endif

         call MessageNotify('M','wu_TorBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      if ( rigid .AND. present(value) ) then
         wu_torpot(:,lm) = value
      else
         wu_torpot(:,lm) = 0.0D0
      endif

      wu_torpot = lusolve(alu,kp,wu_TORPOT)

    end subroutine wu_TorBoundaryTau

    subroutine wu_TorBoundaryGrid(wu_TORPOT,value,cond,new) 
      !
      ! ®�٥ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! �¶��֤Ǥζ������Ŭ��
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! wu_Initial �ˤ����ꤹ������ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��
      ! ���������Ƥ���ɬ�פ�����. 
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
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)    :: wu_TORPOT
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

      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_TORPOT
      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp
      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wu_data
      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_data
      logical                                 :: rigid   ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l
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
            call MessageNotify('E','wu_TorBoundaryGrid','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lm /= km ) then
            call MessageNotify('E','TorBoundaryGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((nm+1)*(nm+1),0:km,0:lm),kp((nm+1)*(nm+1),0:lm))

         alu = 0.0D0
         do l=0,lm
            wu_data = 0.0D0
            wu_data(:,l)=1.0D0
            alu(:,:,l) = wr_wu(wu_data)
         enddo

         if ( .not. rigid ) then
            do l=0,lm
               wu_data = 0.0D0
               wu_data(:,l)=1.0D0
               wr_data = wr_DRad_wu(wu_data) - wr_wu(wu_data)/wr_Rad
               alu(:,0,l) = wr_data(:,0)
            enddo
         endif

         call ludecomp(alu,kp)

         if ( rigid .AND. present(value) ) then 
            call MessageNotify('M','wu_TorBoundaryGrid',&
                 'Toroidal potential at k=0 was given by the optional variable.')
         else if ( rigid .AND. (.NOT.present(value)) ) then
            call MessageNotify('M','wu_TorBoundaryGrid',&
                 'Toroidal potential at k=0 was set to zero.')
         else if ( (.NOT. rigid) .AND. present(value) ) then
            call MessageNotify('W','wu_TorBoundaryGrid',&
                 'Boundary value at k=0 cannot be set under stress-free condition.')
         endif

         call MessageNotify('M','wu_TorBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wr_TorPot       = wr_wu(wu_TorPot)

      if ( rigid .AND. present(value) ) then
         wr_TorPot(:,0)  = value
      else
         wr_TorPot(:,0)  = 0.0D0
      endif

      wu_torpot = lusolve(alu,kp,wr_TorPot)

    end subroutine wu_TorBoundaryGrid

    function wr_LaplaPol2Pol_wr(wr,cond,new)
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵��^2������׻�����.
      !
      ! �����ӥ����ճʻ������֤Ƕ�������Ŭ�Ѥ��Ƥ���. 
      ! ���δؿ����Ѥ��뤿��ˤ� wu_Initial �ˤ����ꤹ��
      ! �����ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��������
      ! ���Ƥ���ɬ�פ�����. 
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
      real(8), dimension((nm+1)*(nm+1),0:km),intent(in)  :: wr
              !(in) ���Ϣ�^2��ʬ��

      real(8), dimension((nm+1)*(nm+1),0:km)             :: wr_LaplaPol2Pol_wr
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

      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_work
      real(8), dimension(0:km,0:km)           :: gg
      logical                                 :: rigid   ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k,n
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
            call MessageNotify('E','wu_laplapol2pol_wu','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lm /= km ) then
            call MessageNotify('E','wr_LaplaPol2Pol_wr', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((nm+1)*(nm+1),0:km,0:km),kp((nm+1)*(nm+1),0:km))

         do k=0,km
            wr_work = 0.0D0 ; wr_work(:,k) = 1.0D0

            ! �ƿ�ʿ�ȿ��˴ؤ�����Ω�μ�
            alu(:,:,k) = wr_Lapla_wu(wu_wr(wr_work))
         enddo

         ! ��ư��Ū���. ή���϶����ǰ���

         gg = 0.0D0
         do k=0,km
            gg(k,k)=1.0D0
         enddo
         do n=1,(nm+1)*(nm+1)
            alu(n,0,:)   = gg(:,0)
            alu(n,km,:)  = gg(:,km)
         enddo

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid ) then
            do k=0,km
               wr_work = 0.0D0 ; wr_work(:,k) = 1.0D0
               wr_work=wr_DRad_wu(wu_wr(wr_work))
               alu(:,1,k) = wr_work(:,0)
            enddo
         else
            do k=0,km
               wr_work = 0.0D0 ; wr_work(:,k) = 1.0D0
               wr_work=wr_DRad2_wu(wu_wr(wr_work))
               alu(:,1,k) = wr_work(:,0)
            enddo
         endif

         call ludecomp(alu,kp)

         call MessageNotify('M','wr_LaplaPol2Pol_wr',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wr_work         = wr
      wr_work(:,1)    = 0.0D0               ! �ϳ�Ū���
      wr_work(:,0)    = 0.0D0               ! ��ư��Ū���

      wr_laplapol2pol_wr = lusolve(alu,kp,wr_work)

    end function wr_LaplaPol2Pol_wr

    function wu_LaplaPol2PolGrid_wu(wu,cond,new)
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵��^2������׻�����.
      ! �����ӥ����ճʻ������֤Ƕ�������Ŭ�Ѥ��Ƥ���. 
      !
      ! ���δؿ����Ѥ��뤿��ˤ� wu_Initial �ˤ����ꤹ��
      ! �����ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��������
      ! ���Ƥ���ɬ�פ�����. 
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵�� f = ��^2���������뼰��
      !
      !    ��^2�� = f
      !      �� = const. at Boundary.
      !      �ߦ�/��r = 0 at Boundary          (Ǵ����) 
      !      or ��^2��/��r^2 = 0 at Boundary   (���Ϥʤ����)
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      ! �ǽ�Ū�˥����ӥ����շ����β��ߤ������ˤ�, wr_LaplaPol2Pol_wr ��
      ! ��٤ƥ����ӥ����� -- �ʻ����Ѵ��� 1 ��ʬ���ʤ��ƺѤ�.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(in)  :: wu
              !(in) ���Ϣ�^2��ʬ��

      real(8), dimension((nm+1)*(nm+1),0:lm)         :: wu_LaplaPol2PolGrid_wu
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

      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_work
      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wu_work
      logical                                 :: rigid   ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l
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
            call MessageNotify('E','wu_LaplaPol2PolGrid_wu','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lm /= km ) then
            call MessageNotify('E','wu_LaplaPol2PolGrid_wu', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((nm+1)*(nm+1),0:km,0:lm),kp((nm+1)*(nm+1),0:lm))

         do l=0,lm
            wu_work = 0.0D0 ; wu_work(:,l) = 1.0D0

            ! �ƿ�ʿ�ȿ��˴ؤ�����Ω�μ�
            alu(:,:,l) = wr_Lapla_wu(wu_work)
         enddo

         ! ��ư��Ū���. ή���϶����ǰ���
         do l=0,lm
            wu_work=0.0D0
            wu_work(:,l)=1.0D0
            wr_work=wr_wu(wu_work)
            alu(:,0,l)=wr_work(:,0)
         enddo

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid ) then
            do l=0,lm
               wu_work=0.0D0
               wu_work(:,l)=1.0D0
               wr_work=wr_DRad_wu(wu_work)
               alu(:,1,l)=wr_work(:,0)
            enddo
         else
            do l=0,lm
               wu_work=0.0D0
               wu_work(:,l)=1.0D0
               wr_work=wr_DRad2_wu(wu_work)
               alu(:,1,l)=wr_work(:,0)
            enddo
         endif

         call ludecomp(alu,kp)

         call MessageNotify('M','wu_LaplaPol2PolGrid_wu',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wr_work         = wr_wu(wu)
      wr_work(:,1)    = 0.0D0               ! �ϳ�Ū���
      wr_work(:,0)    = 0.0D0               ! ��ư��Ū���

      wu_LaplaPol2PolGrid_wu = lusolve(alu,kp,wr_work)

    end function wu_LaplaPol2PolGrid_wu

    subroutine wu_TormagBoundaryTau(wu_TOR,new)
      
      ! ����ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! Chebyshev ���֤Ǥζ������Ŭ��
      !
      ! �����ӥ����ն��֤ˤ����ƶ��������������٤��⼡�η�����������ˡ��
      ! �ȤäƤ���(����ˡ). ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ�
      ! �б����Ƥ���. ���ξ��, ����ȥ�����ݥƥ󥷥��ζ�������
      !
      ! ��¦
      !    wu_psi = 0   at the outer boundary
      ! 
      ! �Ǥ��뤫�� wu_Boundary ���б���ǽ����, ����Τ������Ӻ������Ƥ���.
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)   :: wu_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l
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
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:lm,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(wu_I((nm+1)*(nm+1),0:lm),wr_PSI((nm+1)*(nm+1),0:km))

         alu = 0.0D0
         do l=0,lm
            alu(:,l,l) = 1.0D0
         enddo

         ! ���ŵ���Ƴ��
         do l=0,lm
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            alu(:,lm,l) = wr_Psi(:,0)
         enddo

         call ludecomp(alu,kp)

         deallocate(wu_I,wr_PSI)

         call MessageNotify('M','TormagBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wu_TOR(:,lm)   = 0.0D0
      wu_TOR = lusolve(alu,kp,wu_TOR)

    end subroutine wu_TormagBoundaryTau

    subroutine wu_TormagBoundaryGrid(wu_TOR,new)
      !
      ! ����ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! ��ľ�¶��֤Ǥζ������Ŭ��.
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! wu_Initial �ˤ����ꤹ������ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��
      ! ���������Ƥ���ɬ�פ�����. 
      !
      ! ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ��б����Ƥ���. 
      ! ���ξ��, ����ȥ�����ݥƥ󥷥��ζ�������
      !
      ! ��¦
      !    wu_psi = 0   at the outer boundary
      ! 
      ! �Ǥ���Τ� wu_Boundary ���б���ǽ����, ����Τ������Ӻ������Ƥ���
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)   :: wu_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_TOR

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lm /= km ) then
            call MessageNotify('E','TorMagBoundaryGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:km,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(wu_I((nm+1)*(nm+1),0:lm),wr_PSI((nm+1)*(nm+1),0:km))

         do l=0,lm
            wu_I = 0.0D0
            wu_I(:,l)=1.0D0
            alu(:,:,l) = wr_wu(wu_I)              ! �����ΰ���ͤ��Τޤ�.
         enddo

         ! ���ŵ���Ƴ��

         call ludecomp(alu,kp)

         deallocate(wu_I,wr_PSI)

         call MessageNotify('M','TormagBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif
      
      wr_TOR       = wr_wu(wu_TOR)
      wr_TOR(:,0)  = 0.0D0
      wu_TOR = lusolve(alu,kp,wr_TOR)

    end subroutine wu_TormagBoundaryGrid

    subroutine wu_PolmagBoundaryTau(wu_POL,new)
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
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)   :: wu_POL
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l, n, nn(2)
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
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         if ( allocated(wr_DPSIDR) ) deallocate(wr_DPSIDR)

         allocate(alu((nm+1)*(nm+1),0:lm,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(wu_I((nm+1)*(nm+1),0:lm))
         allocate(wr_PSI((nm+1)*(nm+1),0:km),wr_DPSIDR((nm+1)*(nm+1),0:km))

         alu = 0.0D0
         do l=0,lm
            alu(:,l,l) = 1.0D0
         enddo

         ! ���ŵ���Ƴ��
         do l=0,lm
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            wr_DPSIDR = wr_DRad_wu(wu_I)

            do n=1,(nm+1)*(nm+1)
               nn=nm_l(n)
               alu(n,lm,l) = wr_DPSIDR(n,0) + (nn(1)+1) * wr_PSI(n,0)/r_RAD(0)
            enddo
         enddo

         call ludecomp(alu,kp)

         deallocate(wu_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','PolmagBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wu_POL(:,lm)   = 0.0D0
      wu_POL = lusolve(alu,kp,wu_POL)

    end subroutine wu_PolmagBoundaryTau

    subroutine wu_PolmagBoundaryGrid(wu_POL,new)
      !
      ! ����ݥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! ��ľ�¶��֤Ǥζ������Ŭ��. 
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! wu_Initial �ˤ����ꤹ������ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��
      ! ���������Ƥ���ɬ�פ�����. 
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
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)   :: wu_POL
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR
      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_POL

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l, n, nn(2)
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lm /= km ) then
            call MessageNotify('E','PolMagBoundaryGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         if ( allocated(wr_DPSIDR) ) deallocate(wr_DPSIDR)

         allocate(alu((nm+1)*(nm+1),0:km,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(wu_I((nm+1)*(nm+1),0:lm))
         allocate(wr_PSI((nm+1)*(nm+1),0:km),wr_DPSIDR((nm+1)*(nm+1),0:km))

         do l=0,lm
            wu_I = 0.0D0
            wu_I(:,l)=1.0D0
            alu(:,:,l) = wr_wu(wu_I)              ! �����ΰ���ͤ��Τޤ�.
         enddo

         ! ���ŵ���Ƴ��
         do l=0,lm
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            wr_DPSIDR = wr_DRad_wu(wu_I)

            do n=1,(nm+1)*(nm+1)
               nn=nm_l(n)
               alu(n,0,l) = wr_DPSIDR(n,0) + (nn(1)+1) * wr_PSI(n,0)/r_RAD(0)
            enddo
         end do

         call ludecomp(alu,kp)

         deallocate(wu_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','PolmagBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wr_POL       = wr_wu(wu_POL)
      wr_POL(:,0)  = 0.0D0
      wu_POL = lusolve(alu,kp,wr_POL)

    end subroutine wu_PolmagBoundaryGrid

  end module wu_module
