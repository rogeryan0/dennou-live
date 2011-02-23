!--
!----------------------------------------------------------------------
! Copyright(c) 2002-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wt_zonal_module
!
!    spml/wt_zonal_module �⥸�塼��ϵ��̾太��ӵ����Ǥη���������
!    ���ͤ��Ӿ�Ū���о� 2 ����ή�α�ư�򥹥ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻�����
!    ����� Fortran90 �ؿ����󶡤����ΤǤ���. 
!
!    ��ʿ�����˥른���ɥ�¿�༰�Ѵ�����Ӿ岼�ζ����ɤ򰷤������
!    �����ӥ�����¿�༰�Ѵ����Ѥ�����Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤ�
!    �ؿ����󶡤���. 
!
!    ������ wa_zonal_module, at_module ���Ѥ��Ƥ���. �ǲ����Ǥ�
!    �른���ɥ�¿�༰����ӥ����ӥ�����¿�༰�Ѵ��Υ��󥸥�Ȥ��� 
!    ISPACK �� Fortran77 ���֥롼������Ѥ��Ƥ���.
!
!    wt_zonal_modulde ���󶡤����ؿ������֥롼����� 2 ����Ūή�α�ư�򰷤� 
!    wt_module �⥸�塼����Ѥ����Ƥ����Τ�̾������ӥ��󥿡��ե�������
!    ���̤ˤʤ�褦���߷פ��Ƥ���. �������ä�, wt_module ���Ѥ��ƹ������줿
!    3 ������ǥ���Ӿ������˰��ͤ� 2 �������оΥ�ǥ�ؤȲ�¤����ˤ�
!    ���μ�礬ɬ�פȤʤ�. 
!
!      * use ʸ�Ǥ� wt_module �ΰ��Ѥ� wt_zonal_module ���ѹ�����. 
!      * ������礭������������ʻ����� im -> 1 ��, 
!        ��ʿ�ȿ��� (nm+1)**2 -> nm+1 ���ѹ�����. 
!      * DO ʸ�ǿ�ʿ�ȿ��˴ؤ��ƥ롼�פ�󤷤Ƥ���Ȥ����
!        (nm+1)**2 -> nm+1 ���ѹ�����. 
!      * gtool ���Ϥμ����ѿ��ѹ�����. 
!
!����  2008/12/29  �ݹ�����  wt_module ��꼴�оΥ�ǥ��Ѥ˲�¤
!      2009/01/08  �ݹ�����  w_Initial ��å����������դ��ɲ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/07/31  �ݹ�����  �������׻�������� threadprivate ����(OpenMP)
!      2010/03/10  ��������ʿ  threadprivate ���(����ѥ����¸)
!
!����
!      �ǡ�������� index
!        x : ����         y : ����        z : ư��
!        w : ����Ĵ�´ؿ����ڥ��ȥ�
!        n : ����Ĵ�´ؿ����ڥ��ȥ�(��ʿ���ȿ�)
!        m : ����Ĵ�´ؿ����ڥ��ȥ�(�Ӿ��ȿ�)
!        t : �����ӥ����մؿ����ڥ��ȥ�
!        a : Ǥ�դμ���
!
!        xyz : 3 �����ʻ����ǡ���
!        xy  : ��ʿ 2 �����ʻ����ǡ���
!        yz  : �Ҹ��� 2 �����ʻ����ǡ���
!        xz  : ������ 2 �����ʻ����ǡ���
!
!        wz  : ��ʿ���ڥ��ȥ�ư�³ʻ����ǡ���
!        wt  : ���ڥ��ȥ�ǡ���
!
!++
module wt_zonal_module
  !
  != wt_zonal_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wt_zonal_module.f90,v 1.6 2010-03-02 10:09:07 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/wt_zonal_module �⥸�塼��ϵ��̾太��ӵ����Ǥη���������
  ! ���ͤ��Ӿ�Ū���о� 2 ����ή�α�ư�򥹥ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻�����
  ! ����� Fortran90 �ؿ����󶡤����ΤǤ���. 
  !
  ! ��ʿ�����˥른���ɥ�¿�༰�Ѵ�����Ӿ岼�ζ����ɤ򰷤������
  ! �����ӥ�����¿�༰�Ѵ����Ѥ�����Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤ�
  ! �ؿ����󶡤���. 
  !
  ! ������ wa_zonal_module, at_module ���Ѥ��Ƥ���. �ǲ����Ǥ�
  ! �른���ɥ�¿�༰����ӥ����ӥ�����¿�༰�Ѵ��Υ��󥸥�Ȥ��� 
  ! ISPACK �� Fortran77 ���֥롼������Ѥ��Ƥ���.
  !
  ! wt_zonal_modulde ���󶡤����ؿ������֥롼����� 2 ����Ūή�α�ư�򰷤� 
  ! wt_module �⥸�塼����Ѥ����Ƥ����Τ�̾������ӥ��󥿡��ե�������
  ! ���̤ˤʤ�褦���߷פ��Ƥ���. �������ä�, wt_module ���Ѥ��ƹ������줿
  ! 3 ������ǥ���Ӿ������˰��ͤ� 2 �������оΥ�ǥ�ؤȲ�¤����ˤ�
  ! ���μ�礬ɬ�פȤʤ�. 
  !
  ! 1. use ʸ�Ǥ� wt_module �ΰ��Ѥ� wt_zonal_module ���ѹ�����. 
  ! 2. ������礭������������ʻ����� im -> 1 ��, 
  !    ��ʿ�ȿ��� (nm+1)**2 -> nm+1 ���ѹ�����. 
  ! 3. DO ʸ�ǿ�ʿ�ȿ��˴ؤ��ƥ롼�פ�󤷤Ƥ���Ȥ����
  !    (nm+1)**2 -> nm+1 ���ѹ�����. 
  ! 4. gtool ���Ϥμ����ѿ��ѹ�����. 
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (wt_, nmz_, nz_, xyz_, wz_, w_, xy_, x_, y_, z_, a_) ��, 
  !   �֤��ͤη��򼨤��Ƥ���.
  !   wt_  :: ���ڥ��ȥ�ǡ���(�른���ɥ�¿�༰�������ӥ�����¿�༰�Ѵ�)
  !   nmz_ :: ��ʿ���ڥ��ȥ�ǡ���(���ȿ� n, �Ӿ��ȿ�����ʬ, ư��)
  !   nz_  :: ��ʿ���ڥ��ȥ�ǡ���(���ȿ� n, ư��)
  !   xyz_ :: 3 �����ʻ����ǡ���(���١����١�ư��)
  !   wz_  :: ��ʿ���ڥ��ȥ�, ư�³ʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(DLon, GradLat, GradLat, DivLon, DivLat, Lapla,..)
  !   ��, ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (wt_, xyz_, wz_, w_, xy_, x_, y_, z_, a_) ��, �����ѿ���
  !   �������ڥ��ȥ�ǡ�������ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _wt      :: ���ڥ��ȥ�ǡ���
  !   _xyz     :: 3 �����ʻ����ǡ���
  !   _xyz_xyz :: 2 �Ĥ�3 �����ʻ����ǡ���, ...
  !
  !=== �ƥǡ����μ��������
  !
  ! * xyz : 3 �����ʻ����ǡ���(���١����١�ư��)
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:im-1,1:jm,0:km). 
  !   * im, jm, km �Ϥ��줾�����, ����, ư�º�ɸ�γʻ������Ǥ���, 
  !     ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * wt : ���ڥ��ȥ�ǡ���
  !   * �ѿ��μ���ȼ����� real(8), dimension(nm+1,0:lm). 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ�, lm �ϥ����ӥ�����¿�༰�κ��缡��
  !     �Ǥ���, ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !   * ��ʿ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ϴؿ� l_nm, nm_l �ˤ�ä�Ĵ�٤�
  !     ���Ȥ��Ǥ���.
  !
  ! * nmz : ��ʿ���ڥ��ȥ�ǡ������¤�� 3 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,-nm:nm,0:km). 
  !   * �� 1 ��������ʿ���ȿ�, �� 2 �������Ӿ��ȿ�, �� 3 ������ư�º�ɸ��ɽ��. 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, ���֥롼���� wt_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���.
  !
  ! * nz : ���ڥ��ȥ�ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,0:km). 
  !   * �� 1 ��������ʿ���ȿ���ɽ��. 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, 
  !     ���֥롼���� wt_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * wz : ��ʿ���ڥ��ȥ�, ư�³ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(nm+1,0:km).
  !
  ! * wt_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * xyz_ �ǻϤޤ�ؿ����֤��ͤ� 3 �����ʻ����ǡ�����Ʊ��.
  !
  ! * wz_ �ǻϤޤ�ؿ����֤��ͤϿ�ʿ���ڥ��ȥ�, ư�³ʻ����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  ! 
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! wt_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! x_Lon, y_Lat, z_Rad          :: �ʻ�����ɸ(����, ����, ư�º�ɸ)��
  !                                 ��Ǽ����1 ��������
  ! x_Lon_Weight, y_Lat_Weight, z_Rad_Weight :: �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! xyz_Lon, xyz_Lat, xyz_Rad    :: �ʻ����ǡ����η��١����١�ư�º�ɸ(X,Y,Z)
  !                                 (�ʻ����ǡ����� 3 ��������)
  !
  !==== �����Ѵ�
  !
  ! xyz_wt, wt_xyz :: ���ڥ��ȥ�ǡ����� 3 �����ʻҥǡ����δ֤��Ѵ�
  !                   (����Ĵ��ȡ��, �����ӥ������Ѵ�)
  ! xyz_wz, wz_xyz :: 3 �����ʻҥǡ����ȿ�ʿ���ڥ��ȥ롦ư�³ʻҥǡ����Ȥ�
  !                   �֤��Ѵ� (����Ĵ��ȡ��)
  ! wz_wt, wt_wz   :: ���ڥ��ȥ�ǡ����ȿ�ʿ���ڥ��ȥ롦ư�³ʻҥǡ����Ȥ�
  !                   �֤��Ѵ� (�����ӥ������Ѵ�)
  ! w_xy, xy_w     :: ���ڥ��ȥ�ǡ����� 2 ������ʿ�ʻҥǡ����δ֤��Ѵ�
  !                   (����Ĵ��ȡ���Ѵ�) 
  ! az_at, at_az   :: Ʊ����ʣ���ĹԤ� (�����ӥ������Ѵ�)�ʻҥǡ�����
  !                   �����ӥ����եǡ����δ֤��Ѵ���
  ! l_nm, nm_l     :: ���ڥ��ȥ�ǡ����γ�Ǽ���֤����ȿ����Ӿ��ȿ����Ѵ� 
  !
  !==== ��ʬ
  !
  ! wt_DRad_wt          :: ���ڥ��ȥ�ǡ�����ư����ʬ��/��r ����Ѥ�����
  ! wt_DivRad_wt        :: ���ڥ��ȥ�ǡ�����ȯ����ư����ʬ
  !                        1/r^2 ��/��r r^2 = ��/��r + 2/r ����Ѥ�����
  ! wt_RotRad_wt        :: ���ڥ��ȥ�ǡ����˲�ž��ư����ʬ
  !                        1/r ��/��rr = ��/��r + 1/r ����Ѥ�����
  ! wt_Lapla_wt         :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! xyz_GradLon_wt      :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  !                        1/rcos�ա���/�ߦˤ���Ѥ�����
  ! xyz_GradLat_wt      :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  !                        1/r����/�ߦդ���Ѥ�����
  ! wt_DivLon_xyz       :: �ʻҥǡ�����ȯ����������ʬ
  !                        1/rcos�ա���/�ߦˤ���Ѥ�����
  ! wt_DivLat_xyz       :: �ʻҥǡ�����ȯ����������ʬ
  !                        1/rcos�ա���(g cos��)/�ߦդ���Ѥ�����
  ! wt_Div_xyz_xyz_xyz  :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����
  !                        ȯ������Ѥ�����
  ! xyz_Div_xyz_xyz_xyz :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����
  !                        ȯ������Ѥ�����
  ! xyz_RotLon_wt_wt    :: �٥��ȥ��β�ž�η�����ʬ��׻�����
  ! xyz_RotLat_wt_wt    :: �٥��ȥ��β�ž�ΰ�����ʬ��׻�����
  ! wt_RotRad_xyz_xyz   :: �٥��ȥ��β�ž��ư����ʬ��׻�����
  !
  !==== �ȥ�����ݥ�����׻�����ʬ
  !
  ! wt_KxRGrad_wt            :: ���ڥ��ȥ�ǡ����˷�����ʬ
  !                             k��r���� = ��/�ߦˤ���Ѥ�����
  ! xyz_KGrad_wt             :: ���ڥ��ȥ�ǡ����˼�������ʬ
  !                             k���� = cos��/r ��/�ߦ� + sin�բ�/��r ��
  !                             ���Ѥ�����
  ! wt_L2_wt                 :: ���ڥ��ȥ�ǡ����� 
  !                             L2 �黻�� = -��ʿ��ץ饷�������Ѥ�����
  ! wt_L2Inv_wt              :: ���ڥ��ȥ�ǡ�����
  !                             L2 �黻�Ҥε� = -�տ�ʿ��ץ饷�������Ѥ�����
  ! wt_QOperator_wt          :: ���ڥ��ȥ�ǡ����˱黻��
  !                             Q=(k����-1/2(L2 k����+ k����L2)) ����Ѥ�����
  ! wt_RadRot_xyz_xyz        :: �٥��ȥ� v �α��٤�ư�¥٥��ȥ� r ������
  !                             r��(����v) ��׻�����
  ! wt_RadRotRot_xyz_xyz_xyz :: �٥��ȥ�� v �� r��(���ߢ���v) ��׻�����
  ! wt_Potential2Vector      :: �ȥ�����ݥ�����ݥƥ󥷥�뤫��
  !                             �٥��ȥ���׻�����
  ! wt_Potential2Rotation    :: �ȥ�����ݥ�����ݥƥ󥷥���ɽ�����
  !                             ��ȯ���٥��ȥ��β�ž�γ���ʬ��׻�����
  !
  !==== �������׻�
  !
  ! wt_VGradV    ::  �٥��ȥ� v ���� v����v ��׻�����
  !
  !==== �ݥ�����/�ȥ������ǥ��ѥ��ڥ��ȥ����
  !
  ! nmz_ToroidalEnergySpectrum_wt, nz_ToroidalEnergySpectrum_wt   ::
  !     �ȥ�����ݥƥ󥷥�뤫�饨�ͥ륮���ε���Ĵ��ȡ������ʬ��׻�����
  ! nmz_PoloidalEnergySpectrum_wt, nz_PoloidalEnergySpectrum_wt   :: 
  !     �ݥ�����ݥƥ󥷥�뤫�饨�ͥ륮���ε���Ĵ��ȡ������ʬ��׻�����
  !
  !==== ����������
  !
  ! wt_BoundariesTau, wt_BoundariesGrid, wt_Boundaries                   ::
  !     �ǥ��ꥯ��, �Υ��ޥ󶭳�����Ŭ�Ѥ���(����ˡ, ����ˡ)
  ! wt_TorBoundariesTau, wt_TorBoundariesGrid, wt_TorBoundaries          ::
  !     ®�٥ȥ�����ݥƥ󥷥��ζ�������Ŭ�Ѥ���(����ˡ,����ˡ)
  ! wz_LaplaPol2Pol_wz, wt_LaplaPol2Pol_wt                               ::
  !     ®�٥ݥ�����ݥƥ󥷥�릵��^2���������
  !     (�����Ϥ����줾������ӥ����ճʻ���,�����ӥ����շ���)
  ! wt_TorMagBoundariesTau, wt_TorMagBoundariesGrid, wt_TorMagBoundaries ::
  !     ����ȥ�����ݥƥ󥷥��ζ�������Ŭ�Ѥ���(����ˡ, ����ˡ)
  ! wt_PolMagBoundariesTau, wt_PolMagBoundariesGrid, wt_PolMagBoundaries ::
  !     ����ȥ�����ݥƥ󥷥�붭���ζ�������Ŭ�Ѥ���(����ˡ, ����ˡ)
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
  !
  !==== ��ʬ��ʿ��(2 �����ǡ���)
  !
  ! IntLonLat_xy, AvrLonLat_xy :: 2 �����ʻ����ǡ����ο�ʿ(����)��ʬ�����ʿ��
  ! IntLonRad_xz, AvrLonRad_xz :: 2 ����(XZ)�ʻ����ǡ����η���ư����ʬ
  !                               �����ʿ��
  ! IntLatRad_yz, AvrLatRad_yz :: 2 ����(YZ)�ʻ����ǡ����ΰ���ư��(�Ҹ���)
  !                               ��ʬ�����ʿ�� 
  ! y_IntLon_xy, y_AvrLon_xy   :: ��ʿ 2 ����(����)�ʻ����ǡ����η�������
  !                               ��ʬ�����ʿ��
  ! x_IntLat_xy, x_AvrLat_xy   :: ��ʿ2 ����(����)�ʻ����ǡ����ΰ���������ʬ
  !                               �����ʿ��
  ! z_IntLon_xz, z_AvrLon_xz   :: 2 ����(XZ)�ʻ����ǡ����η���������ʬ�����
  !                               ʿ��
  ! x_IntRad_xz, x_AvrRad_xz   :: 2 ����(XZ)�ʻ����ǡ�����ư��������ʬ�����
  !                               ʿ��
  ! z_IntLat_yz, z_AvrLat_yz   :: 2 ����(YZ)�ʻ����ǡ����ΰ���������ʬ�����
  !                               ʿ��
  ! y_IntRad_yz, y_AvrRad_yz   :: 2 ����(YZ)�ʻ����ǡ�����ư��������ʬ�����
  !                               ʿ��                  
  !
  !==== ��ʬ��ʿ��(1 �����ǡ���)
  !
  ! IntLon_x, AvrLon_x  :: 1 ����(X)�ʻ����ǡ����η���������ʬ�����ʿ��
  ! IntLat_y, AvrLat_y  :: 1 ����(Y)�ʻ����ǡ����ΰ���������ʬ�����ʿ��
  ! IntRad_z, AvrRad_z  :: 1 ����(Z)�ʻ����ǡ�����ư��������ʬ�����ʿ��
  !
  !==== ��ַ׻�
  !
  ! Interpolate_wt :: ���ڥ��ȥ�ǡ�������Ǥ�դ������ͤ���֤���. 
  ! 
  use dc_message
  use lumatrix
  use wa_zonal_module
  use at_module, z_RAD => g_X, z_RAD_WEIGHT => g_X_WEIGHT, &
                 at_az => at_ag, az_at => ag_at, &
                 t_Dr_t => t_Dx_t, at_Dr_at => at_Dx_at
  implicit none
  private

  public wt_Initial

  public x_Lon, x_Lon_Weight
  public y_Lat, y_Lat_Weight
  public z_Rad, z_Rad_Weight
  public l_nm, nm_l
  public xy_Lon, xy_Lat
  public xyz_Lon, xyz_Lat, xyz_Rad
  public wz_Rad
  public wt_VMiss

  public w_xy, xy_w
  public at_Dr_at, t_Dr_t, az_at, at_az
  public xyz_wt, wt_xyz, xyz_wz, wz_xyz, wz_wt, wt_wz
  public wt_DRad_wt, wt_DivRad_wt, wt_RotRad_wt, wt_Lapla_wt
  public xyz_GradLon_wt, xyz_gradlat_wt
  public wt_DivLon_xyz, wt_DivLat_xyz
  public wt_Div_xyz_xyz_xyz, xyz_Div_xyz_xyz_xyz
  public xyz_RotLon_wt_wt, xyz_RotLat_wt_wt, wt_RotRad_xyz_xyz

  public yz_IntLon_xyz, xz_IntLat_xyz, xy_IntRad_xyz
  public x_IntLatRad_xyz, y_IntLonRad_xyz, z_IntLonLat_xyz
  public IntLonLatRad_xyz

  public x_IntLat_xy, y_IntLon_xy, IntLonLat_xy
  public z_IntLat_yz, y_IntRad_yz, IntLatRad_yz
  public z_IntLon_xz, x_IntRad_xz, IntLonRad_xz
  public IntLon_x, IntLat_y, IntRad_z

  public yz_AvrLon_xyz, xz_AvrLat_xyz, xy_AvrRad_xyz
  public x_AvrLatRad_xyz, y_AvrLonRad_xyz, z_AvrLonLat_xyz
  public AvrLonLatRad_xyz

  public x_AvrLat_xy, y_AvrLon_xy, AvrLonLat_xy
  public z_AvrLat_yz, y_AvrRad_yz, AvrLatRad_yz
  public z_AvrLon_xz, x_AvrRad_xz, AvrLonRad_xz
  public AvrLon_x, AvrLat_y, AvrRad_z

  public wt_KxRGrad_wt, xyz_KGrad_wt, wt_L2_wt, wt_L2Inv_wt, wt_QOperator_wt
  public wt_RadRot_xyz_xyz, wt_RadRotRot_xyz_xyz_xyz
  public wt_Potential2vector, wt_Potential2Rotation
  public wt_VGradV

  public Interpolate_wt

  public nmz_ToroidalEnergySpectrum_wt, nz_ToroidalEnergySpectrum_wt
  public nmz_PoloidalEnergySpectrum_wt, nz_PoloidalEnergySpectrum_wt

  public wt_Boundaries, wt_TorBoundaries, wz_LaplaPol2Pol_wz
  public wt_TormagBoundaries, wt_PolmagBoundaries

  public wt_BoundariesTau, wt_TorBoundariesTau
  public wt_TormagBoundariesTau, wt_PolmagBoundariesTau

  public wt_BoundariesGrid, wt_TorBoundariesGrid, wt_LaplaPol2PolGrid_wt
  public wt_TormagBoundariesGrid, wt_PolmagBoundariesGrid

  interface wt_Boundaries
     module procedure wt_BoundariesTau
  end interface

  interface wt_TorBoundaries
     module procedure wt_TorBoundariesTau
  end interface

  interface wt_TorMagBoundaries
     module procedure wt_TorMagBoundariesTau
  end interface

  interface wt_PolMagBoundaries
     module procedure wt_PolMagBoundariesTau
  end interface

  integer            :: im=1, jm=32, km=16   ! �ʻ���������(����, ����, ư��)
  integer            :: nm=21, lm=16         ! �����ȿ�������(��ʿ, ư��)
  real(8)            :: ri=0.0, ro=1.0       ! ����⳰Ⱦ��
  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), dimension(:,:,:), allocatable :: xyz_LON, xyz_LAT, xyz_RAD ! ��ɸ
  real(8), dimension(:,:), allocatable   :: wz_RAD                    ! ��ɸ

  real(8) :: wt_VMiss = -999.0        ! ��»��

  save im, jm, km, nm, lm, ri, ro

  contains
  !--------------- ����� -----------------
   subroutine wt_Initial(i,j,k,n,l,r_in,r_out,np,wa_init)
     !
     ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, ư�º�ɸ���ϰϤ����ꤹ��.
     !
     ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ�ǽ�������
     ! ���ʤ���Фʤ�ʤ�. 
     !
     ! np_in �� wt_module �Ȥθߴ����Τ���ˤ����Ƥ�������Ǥ���. 
     ! OPENMP �׻��ϼ�������Ƥ��ʤ�. 
     !
     integer,intent(in) :: i              ! �ʻ�����(���٦�)
     integer,intent(in) :: j              ! �ʻ�����(���٦�)
     integer,intent(in) :: k              ! �ʻ�����(ư�� r)
     integer,intent(in) :: n              ! �����ȿ�(��ʿ���ȿ�)
     integer,intent(in) :: l              ! �����ȿ�(ư���ȿ�)

     real(8),intent(in) :: r_in           ! �����Ⱦ��
     real(8),intent(in) :: r_out          ! ��̳�Ⱦ��

     integer,intent(in), optional :: np   ! OPENMP �Ǥκ��祹��åɿ�
     logical,intent(in), optional :: wa_init   ! wa_initial �����å�

     logical    :: wa_initialize=.true.   ! wa_initial �����å�
     
     im = i  ; jm = j ; km = k
     nm = n  ; lm = l
     ri = r_in ; ro = r_out

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

     call at_Initial(km,lm,r_in,r_out)

     allocate(xyz_Lon(0:im-1,1:jm,0:km))
     allocate(xyz_Lat(0:im-1,1:jm,0:km))
     allocate(xyz_Rad(0:im-1,1:jm,0:km))

     allocate(wz_Rad(nm+1,0:km))

     xyz_Lon = spread(xy_Lon,3,km+1)
     xyz_Lat = spread(xy_Lat,3,km+1)
     xyz_Rad = spread(spread(z_Rad,1,jm),1,im)

     wz_Rad = spread(z_Rad,1,nm+1)

     z_Rad_Weight = z_Rad_Weight * z_Rad**2       ! r^2 dr ����ʬ�Ť�

     call MessageNotify('M','wt_initial', &
          'wt_zonal_module (2009/07/31) is initialized')

   end subroutine wt_initial

  !--------------- �����Ѵ� -----------------

    function xyz_wt(wt)
      !
      ! ���ڥ��ȥ�ǡ������� 3 �����ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(nm+1,0:lm), intent(in)          :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���
      real(8), dimension(0:im-1,1:jm,0:km)               :: xyz_wt
      !(out) 3 �������ٰ���ư�³ʻ����ǡ���

      xyz_wt = xya_wa(az_at(wt))

    end function xyz_wt

    function wt_xyz(xyz)
      !
      ! 3 �����ʻ����ǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���
      real(8), dimension(nm+1,0:lm)                    :: wt_xyz
      !(out) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      wt_xyz = at_az(wa_xya(xyz))

    end function wt_xyz

    function xyz_wz(wz)
      !
      ! ��ʿ���ڥ��ȥ롦ư�³ʻ����ǡ������� 3 �����ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(nm+1,0:km), intent(in)          :: wz
      !(in) 2 ��������Ĵ��ȡ�����ڥ��ȥ롦ư�³ʻ����ǡ���
      real(8), dimension(0:im-1,1:jm,0:km)               :: xyz_wz
      !(out) 3 �������ٰ���ư�³ʻ����ǡ���

      xyz_wz = xya_wa(wz)

    end function xyz_wz

    function wz_xyz(xyz)
      !
      ! 3 �����ʻҥǡ��������ʿ���ڥ��ȥ롦ư�³ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���
      real(8), dimension(nm+1,0:km)                      :: wz_xyz
      !(out) 2 ��������Ĵ��ȡ�����ڥ��ȥ롦ư�³ʻ����ǡ���

      wz_xyz = wa_xya(xyz)

    end function wz_xyz

    function wz_wt(wt)
      !
      ! ���ڥ��ȥ�ǡ��������ʿ���ڥ��ȥ롦ư�³ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���
      real(8), dimension(nm+1,0:km)             :: wz_wt
      !(out) 2 ��������Ĵ��ȡ�����ڥ��ȥ롦ư�³ʻ����ǡ���

      wz_wt = az_at(wt)

    end function wz_wt

    function wt_wz(wz)
      !
      ! ��ʿ���ڥ��ȥ롦ư�³ʻ����ǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(nm+1,0:km), intent(in) :: wz
      !(in) 2 ��������Ĵ��ȡ�����ڥ��ȥ롦ư�³ʻ����ǡ���
      real(8), dimension(nm+1,0:lm)             :: wt_wz
      !(out) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      wt_wz = at_az(wz)

    end function wt_wz

  !--------------- ��ʬ�׻� -----------------
    function wt_DRad_wt(wt)
      !
      ! ���ϥ��ڥ��ȥ�ǡ�����ư����ʬ ��/��r ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ�����ư����ʬ�Ȥ�, �б�����ʻ����ǡ�����ư����ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(nm+1,0:lm)             :: wt_DRad_wt
      !(in) ư����ʬ���줿2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      wt_DRad_wt = at_Dr_at(wt)

    end function wt_DRad_wt

    function wt_DivRad_wt(wt)
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
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(nm+1,0:lm)             :: wt_DivRad_wt
      !(out) ȯ����ư����ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wt_DivRad_wt = wt_Drad_wt(wt) + wt_wz(2/wz_rad*wz_wt(wt))


    end function wt_DivRad_wt

    function wt_RotRad_wt(wt)
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
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(nm+1,0:lm)             :: wt_RotRad_wt
      !(out) ��ž��ư����ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wt_RotRad_wt = wt_Drad_wt(wt) + wt_wz(1/wz_Rad*wz_wt(wt))

    end function wt_RotRad_wt

    function wt_Lapla_wt(wt)
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷����
      !
      !     ��^2 =   1/r^2 cos^2�ա���^2/�ߦ�^2 
      !            + 1/r^2 cos�ա���/�ߦ�(cos�բ�/�ߦ�) 
      !            + 1/r^2 ��/��r (r^2 ��/��r) 
      !
      ! ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(nm+1,0:lm)             :: wt_Lapla_wt
      !(out) ��ץ饷�������Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wt_Lapla_wt = wt_DivRad_wt(wt_Drad_wt(wt)) &
                   + wt_wz(wz_wt(wa_Lapla_wa(wt))/wz_Rad**2)

    end function wt_Lapla_wt

    function xyz_GradLon_wt(wt)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/rcos�ա���/�ߦ�
      ! ����Ѥ�����.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,1:jm,0:km)   :: xyz_GradLon_wt
      !(out) ���۷�������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      xyz_GradLon_wt = xya_GradLon_wa(wz_wt(wt))/xyz_Rad

    end function xyz_GradLon_wt

    function xyz_GradLat_wt(wt) 
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/r ��/�ߦ� ����Ѥ�����.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,1:jm,0:km)    :: xyz_GradLat_wt
      !(out) ���۷�������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      xyz_GradLat_wt = xya_GradLat_wa(wz_wt(wt))/xyz_Rad

    end function xyz_GradLat_wt

    function wt_DivLon_xyz(xyz)
      ! 
      ! �ʻ����ǡ�����ȯ����������ʬ 1/rcos�ա���/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������֤�.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���
      real(8), dimension(nm+1,0:lm)       :: wt_DivLon_xyz
      !(out) ȯ����������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wt_DivLon_xyz = wt_wz(wa_DivLon_xya(xyz/xyz_Rad))

    end function wt_DivLon_xyz

    function wt_DivLat_xyz(xyz)
      !
      ! �ʻҥǡ�����ȯ����������ʬ 1/rcos�ա���(f cos��)/�ߦ� ��
      ! ���Ѥ��������ڥ��ȥ�ǡ������֤�.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���
      real(8), dimension(nm+1,0:lm)       :: wt_DivLat_xyz
      !(out) ȯ����������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wt_DivLat_xyz = wt_wz(wa_divlat_xya(xyz/xyz_Rad))

    end function wt_DivLat_xyz

    function wt_Div_xyz_xyz_xyz(xyz_Vlon,xyz_Vlat,xyz_Vrad)
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vlon
      !(in) �٥��ȥ��η�����ʬ
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension(nm+1,0:lm)     :: wt_Div_xyz_xyz_xyz
      !(out) �٥��ȥ���ȯ��

      wt_Div_xyz_xyz_xyz =   wt_DivLon_xyz(xyz_Vlon) &
                           + wt_DivLat_xyz(xyz_Vlat) &
                           + wt_DivRad_wt(wt_xyz(xyz_Vrad))

    end function wt_Div_xyz_xyz_xyz

    function xyz_Div_xyz_xyz_xyz(xyz_Vlon,xyz_Vlat,xyz_Vrad)
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension(0:im-1,1:jm,0:km)             :: xyz_Div_xyz_xyz_xyz
      !(out) �٥��ȥ���ȯ��

      xyz_Div_xyz_xyz_xyz &
           = xyz_Rad/cos(xyz_Lat) &
                * xyz_wt(wt_Div_xyz_xyz_xyz(xyz_VLon*cos(xyz_Lat)/xyz_Rad,  &
                                            xyz_VLat*cos(xyz_Lat)/xyz_Rad,  &
                                            xyz_VRad*cos(xyz_Lat)/xyz_Rad ))&
             + xyz_VLat*tan(xyz_Lat)/xyz_Rad &
             + xyz_VRad/xyz_Rad

    end function xyz_Div_xyz_xyz_xyz

    function xyz_RotLon_wt_wt(wt_Vrad,wt_Vlat) 
      !
      ! �٥��ȥ���ư����ʬ, ������ʬ�Ǥ����� 1, 2 ���� Vrad, Vlat ����
      ! ��ž�η�����ʬ 
      !
      !    1/r ��Vrad/�ߦ�-1/r ��(r Vlat)/��r ��׻�����.
      !
      ! ��׻�����
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension(nm+1,0:lm), intent(in) :: wt_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyz_RotLon_wt_wt
      !(out) �٥��ȥ��β�ž�η�����ʬ

        xyz_RotLon_wt_wt =   xyz_GradLat_wt(wt_Vrad) &
                           - xyz_wt(wt_RotRad_wt(wt_Vlat))

    end function xyz_RotLon_wt_wt

    function xyz_RotLat_wt_wt(wt_Vlon,wt_Vrad) 
      !
      ! �٥��ȥ��η�����ʬ, ư����ʬ�Ǥ����� 1, 2 ���� Vlon, Vrad ����
      ! ��ž�ΰ�����ʬ 
      !
      !    1/r ��(r Vlon)/��r - 1/rcos�ա���Vrad/�ߦ�
      !
      ! ��׻�����.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension(nm+1,0:lm), intent(in) :: wt_Vrad
      !(in) �٥��ȥ���ư����ʬ

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyz_RotLat_wt_wt
      !(out) �٥��ȥ��β�ž�ΰ�����ʬ

        xyz_RotLat_wt_wt =   xyz_wt(wt_RotRad_wt(wt_Vlon)) &
                           - xyz_GradLon_wt(wt_Vrad) 

    end function xyz_RotLat_wt_wt

    function wt_RotRad_xyz_xyz(xyz_Vlat,xyz_Vlon) 
      !
      ! �٥��ȥ�ΰ�����ʬ, ������ʬ�Ǥ����� 1, 2 ���� Vlat, Vlon ���Ф���
      ! �٥��ȥ��β�ž��ư����ʬ 
      !
      !    1/rcos�ա���Vlat/�ߦ� - 1/rcos�ա���(Vlon cos��)/�ߦ�
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vlat
      !(in) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vlon
      !(in) �٥��ȥ��η�����ʬ

      real(8), dimension(nm+1,0:lm)     :: wt_RotRad_xyz_xyz
      !(out) �٥��ȥ��β�ž��ư����ʬ

        wt_RotRad_xyz_xyz =   wt_DivLon_xyz(xyz_Vlat) &
                            - wt_DivLat_xyz(xyz_Vlon)

    end function wt_RotRad_xyz_xyz

  !--------------- ��ʬ�׻� -----------------
    !----(���ϥǡ��� xyz)---
    function yz_IntLon_xyz(xyz)  ! ����(�Ӿ�)��ʬ
      !
      ! 3 �����ʻ����ǡ����η�������(�Ӿ�)��ʬ.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��� ��f(��,��,r)d�� ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(1:jm,0:km)  :: yz_IntLon_xyz
      !(out) ��������(�Ӿ�)��ʬ���줿 2 �����Ҹ��̳ʻ����ǡ���

      integer :: i

      yz_IntLon_xyz = 0.0d0
      do i=0,im-1
         yz_IntLon_xyz(:,:) = yz_IntLon_xyz(:,:) &
                       + xyz(i,:,:) * x_Lon_Weight(i)
      enddo
    end function yz_IntLon_xyz

    function xz_IntLat_xyz(xyz)
      !
      ! 3 �����ʻ����ǡ����ΰ�����������ʬ.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��Ƣ�f(��,��,r) cos�� d�� ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,0:km)  :: xz_IntLat_xyz        
      !(out) ������ʬ���줿 2 ��������ư�³ʻ����ǡ���.
      ! ���ٱ߳ʻ����ǡ���

      integer :: j

      xz_IntLat_xyz = 0.0d0
      do j=1,jm
         xz_IntLat_xyz(:,:) = xz_IntLat_xyz(:,:) &
                       + xyz(:,j,:) * y_Lat_Weight(j)
      enddo
    end function xz_IntLat_xyz

    function xy_IntRad_xyz(xyz)  ! ư����ʬ
      !
      ! 3 �����ʻ����ǡ�����ư����������ʬ.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��Ƣ�f(��,��,r) r^2dr ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,1:jm)  :: xy_IntRad_xyz
      !(out) ư����ʬ���줿 2 �������ٰ���(��ʿ, ����)�ʻ����ǡ���

      integer :: k

      xy_IntRad_xyz = 0.0d0
      do k=0,km
         xy_IntRad_xyz(:,:) = xy_IntRad_xyz(:,:) &
                       + xyz(:,:,k) * z_Rad_Weight(k) 
      enddo
    end function xy_IntRad_xyz

    function x_IntLatRad_xyz(xyz)
      !
      ! 3 �����ʻ����ǡ����ΰ���ư��(�Ҹ���)��ʬ
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,��,r) r^2cos�� d��dr 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1)     :: x_IntLatRad_xyz
      !(out) ����ư��(�Ҹ���)��ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: j, k

      x_IntLatRad_xyz = 0.0D0
      do k=0,km
         do j=1,jm
            x_IntLatRad_xyz = x_IntLatRad_xyz &
                 + xyz(:,j,k) * y_Lat_Weight(j) * z_Rad_Weight(k)
         enddo
      enddo
    end function x_IntLatRad_xyz

    function y_IntLonRad_xyz(xyz)
      !
      ! 3 �����ʻ����ǡ����η���ư��(���ٱ�)��ʬ.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��Ƣ�f(��,��,r) r^2d��dr ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(1:jm)       :: y_IntLonRad_xyz
      !(out) ����ư��(���ٱ�)��ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: i, k

      y_IntLonRad_xyz = 0
      do k=0,km
         do i=0,im-1
            y_IntLonRad_xyz = y_IntLonRad_xyz &
                 + xyz(i,:,k) * x_Lon_Weight(i) * z_Rad_Weight(k)
         enddo
      enddo
    end function y_IntLonRad_xyz

    function z_IntLonLat_xyz(xyz)  ! ���ٷ���(��ʿ)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ��ٷ���(��ʿ, ����)��ʬ
      ! 
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,��,r) cos�� d��d�� 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:km)     :: z_IntLonLat_xyz
      !(out) ���ٷ���(��ʿ, ����)��ʬ���줿 1 ����ư�³ʻ����ǡ���

      integer :: i, j

      z_IntLonLat_xyz = 0
      do j=1,jm
         do i=0,im-1
            z_IntLonLat_xyz = z_IntLonLat_xyz &
                 + xyz(i,j,:) * x_Lon_Weight(i) * y_Lat_Weight(j)
         enddo
      enddo
    end function z_IntLonLat_xyz

    function IntLonLatRad_xyz(xyz) ! ���ٷ���ư��(����)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ��ٷ���ư��(����)��ʬ
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !     ��f(��,��,r) r^2cos�� d��d��dr 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz 
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8)                     :: IntLonLatRad_xyz 
      !(out) ������ʬ��

      integer :: i, j, k

      IntLonLatRad_xyz = 0
      do k=0,km
         do j=1,jm
            do i=0,im-1
               IntLonLatRad_xyz = IntLonLatRad_xyz &
                    + xyz(i,j,k) * x_Lon_Weight(i) &
                         * y_Lat_Weight(j) * z_Rad_Weight(k)
            enddo
         enddo
      enddo
    end function IntLonLatRad_xyz

    !----(���ϥǡ��� yz)---
    function z_IntLat_yz(yz)  ! ������ʬ
      !
      ! 2 ����(YZ)�ʻ����ǡ����ΰ�����������ʬ.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��Ƣ�f(��,r) cos�� d�� ��׻�����.
      !
      real(8), dimension(jm,0:km), intent(in) :: yz
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(0:km)  :: z_IntLat_yz
      !(out) ������ʬ���줿 1 ����ư�³ʻ����ǡ���

      integer :: j

      z_IntLat_yz = 0.0d0
      do j=1,jm
         z_IntLat_yz(:) = z_IntLat_yz(:) + yz(j,:) * y_Lat_Weight(j)
      enddo
    end function z_IntLat_yz

    function y_IntRad_yz(yz)  ! ư����ʬ
      !
      ! 2 ����(YZ)�ʻ����ǡ�����ư����������ʬ.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��Ƣ�f(��,r) r^2dr ��׻�����.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yz
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(1:jm)  :: y_IntRad_yz
      !(out) ư����ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: k

      y_IntRad_yz = 0.0d0
      do k=0,km
         y_IntRad_yz(:) = y_IntRad_yz(:) &
                       + yz(:,k) * z_Rad_Weight(k) 
      enddo
    end function y_IntRad_yz

    function IntLatRad_yz(yz)
      !
      ! 2 ����(YZ)�ʻ����ǡ����ΰ���ư����ʬ(�Ҹ���)�����ʿ��
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r) r^2cos�� d��dr ��׻�����.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yz
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8)                   :: IntLatRad_yz
      !(out) ��ʬ��
      integer :: j, k

      IntLatRad_yz = 0
      do k=0,km
         do j=1,jm
            IntLatRad_yz = IntLatRad_yz &
                 + yz(j,k) * y_Lat_Weight(j) * z_Rad_Weight(k)
         enddo
      enddo
    end function IntLatRad_yz

    !----(���ϥǡ��� xz)---
    function z_IntLon_xz(xz)
      !
      ! 2 ����(XZ)�ʻ����ǡ����η���������ʬ.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r)d�� ��׻�����.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xz
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8), dimension(0:km)  :: z_IntLon_xz
      !(out) ������ʬ���줿 1 ����ư�³ʻ����ǡ���

      integer :: i

      z_IntLon_xz = 0.0d0
      do i=0,im-1
         z_IntLon_xz(:) = z_IntLon_xz(:) + xz(i,:) * x_Lon_Weight(i)
      enddo

    end function z_IntLon_xz

    function x_IntRad_xz(xz)
      !
      ! 2 ����(XZ)�ʻ����ǡ�����ư����������ʬ.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r) r^2dr ��׻�����.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xz
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8), dimension(0:im-1)  :: x_IntRad_xz
      !(out) ư����ʬ���줿 1 �������ٳʻ����ǡ���

      integer :: k

      x_IntRad_xz = 0.0d0
      do k=0,km
         x_IntRad_xz(:) = x_IntRad_xz(:) &
                       + xz(:,k) * z_Rad_Weight(k) 
      enddo

    end function x_IntRad_xz

    function IntLonRad_xz(xz)  ! ����ư��(���ٱ�)��ʬ
      !
      ! 2 ����(XZ)�ʻ����ǡ����η���ư����ʬ
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��Ƣ�f(��,r) r^2d��dr ��׻�����.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xz
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8)                                 :: IntLonRad_xz
      !(out) ��ʬ��

      integer :: i, k

      IntLonRad_xz = 0
      do k=0,km
         do i=0,im-1
            IntLonRad_xz = IntLonRad_xz &
                 + xz(i,k) * x_Lon_Weight(i) * z_Rad_Weight(k)
         enddo
      enddo
    end function IntLonRad_xz

    !----(���ϥǡ��� z)---
    function IntRad_z(z)  ! ư����ʬ
      !
      ! 1 ����(Z)�ʻ����ǡ�����ư����������ʬ.
      !
      ! 1 �����ǡ��� f(r) ���Ф��� ��f(r) r^2dr ��׻�����.
      !
      real(8), dimension(0:km), intent(in) :: z
      !(in) 1 ����ư�³ʻ����ǡ���

      real(8)                              :: IntRad_z
      !(out) ��ʬ��

      integer :: k

      IntRad_z = 0.0d0
      do k=0,km
         IntRad_z = IntRad_z + z(k) * z_Rad_Weight(k) 
      enddo
    end function IntRad_z

  !--------------- ʿ�ѷ׻� -----------------
    !----(���ϥǡ��� xyz)---
    function yz_AvrLon_xyz(xyz)  ! ����(�Ӿ�)��ʬ
      !
      ! 3 �����ʻ����ǡ����η�������(�Ӿ�)ʿ��.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��� ��f(��,��,r)d��/2�� ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(1:jm,0:km)  :: yz_AvrLon_xyz
      !(out) ��������(�Ӿ�)ʿ�Ѥ��줿 2 �����Ҹ��̳ʻ����ǡ���

      yz_AvrLon_xyz = yz_IntLon_xyz(xyz)/sum(x_Lon_Weight)

    end function yz_AvrLon_xyz

    function xz_AvrLat_xyz(xyz)  ! ������ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ���������ʿ��.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��� ��f(��,��,r)cos�� d��/2 ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,0:km)  :: xz_AvrLat_xyz
      !(out) ����ʿ�Ѥ��줿 2 ��������ư�³ʻ����ǡ���

      xz_AvrLat_xyz = xz_IntLat_xyz(xyz)/sum(y_Lat_Weight)

    end function xz_AvrLat_xyz

    function xy_AvrRad_xyz(xyz)
      !
      ! 3 �����ʻ����ǡ�����ư��������ʿ��.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф��� 
      !
      !    ��f(��,��,r) r^2dr/((r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1,1:jm)  :: xy_AvrRad_xyz          
      !(out) ư��ʿ�Ѥ��줿 2 �������ٰ���(��ʿ, ����)�ʻ����ǡ���
      ! ��ʿ�ʻ����ǡ���

      xy_AvrRad_xyz = xy_IntRad_xyz(xyz)/sum(z_Rad_Weight)

    end function xy_AvrRad_xyz

    function x_AvrLatRad_xyz(xyz)  ! ����ư��(�Ҹ���)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ���ư��(�Ҹ���)ʿ��
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,,r) r^2cos�� d��dr /(2(r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:im-1)     :: x_AvrLatRad_xyz
      !(out) ����ư��(�Ҹ���)ʿ�Ѥ��줿 1 �������ٳʻ����ǡ���

      x_AvrLatRad_xyz = x_IntLatRad_xyz(xyz) &
                   /( sum(y_Lat_Weight)*sum(z_Rad_Weight) )

    end function x_AvrLatRad_xyz

    function y_AvrLonRad_xyz(xyz)  ! ����ư��(���ٱ�)��ʬ
      !
      ! 3 �����ʻ����ǡ����η���ư��(���ٱ�)ʿ��.
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !     ��f(��,��,r) r^2d��dr /(2��(r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(1:jm)       :: y_AvrLonRad_xyz
      !(out) ����ư��(���ٱ�)ʿ�Ѥ��줿 1 �������ٳʻ����ǡ���

      y_AvrLonRad_xyz = y_IntLonRad_xyz(xyz) &
                 /(sum(x_Lon_Weight)*sum(z_Rad_Weight))

    end function y_AvrLonRad_xyz

    function z_AvrLonLat_xyz(xyz)  ! ���ٷ���(��ʿ)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ��ٷ���(��ʿ, ����)��ʬ
      ! 
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,��,r) cos�� d��d�� /4�� 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8), dimension(0:km)     :: z_AvrLonLat_xyz
      !(out) ���ٷ���(��ʿ, ����)ʿ�Ѥ��줿 1 ����ư�³ʻ����ǡ���

      z_AvrLonLat_xyz = z_IntLonLat_xyz(xyz) &
                 /(sum(x_Lon_Weight)*sum(y_Lat_Weight))

    end function z_AvrLonLat_xyz

    function AvrLonLatRad_xyz(xyz) ! ���ٷ���ư��(����)��ʬ
      !
      ! 3 �����ʻ����ǡ����ΰ��ٷ���ư��(����)��ʬ
      !
      ! 3 �����ǡ��� f(��,��,r) ���Ф���
      !
      !    ��f(��,��,r) r^2cos�� d��d��dr /(4��(r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 �������ٰ���ư�³ʻ����ǡ���

      real(8)                     :: AvrLonLatRad_xyz
      !(out) ����ʿ����

      AvrLonLatRad_xyz = IntLonLatRad_xyz(xyz) &
            /(sum(x_Lon_Weight)*sum(y_Lat_Weight) * sum(z_Rad_Weight))

    end function AvrLonLatRad_xyz

    !----(���ϥǡ��� yz)---
    function z_AvrLat_yz(yz)
      !
      ! 2 ����(YZ)�ʻ����ǡ����ΰ���������ʿ��.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r) cos�� d��/2 ��׻�����.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yz
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(0:km)  :: z_AvrLat_yz
      !(out) ����ʿ�Ѥ��줿 1 ����ư�³ʻ����ǡ���

      z_AvrLat_yz = z_IntLat_yz(yz)/sum(y_Lat_Weight)

    end function z_AvrLat_yz

    function y_AvrRad_yz(yz)
      !
      ! 2 ����(YZ)�ʻ����ǡ�����ư��������ʿ��.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r) r^2dr /((r[o]^3-r[i]^3)/3) 
      ! ��׻�����.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yz
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(1:jm)  :: y_AvrRad_yz
      !(out) ư��ʿ�Ѥ��줿 1 �������ٳʻ����ǡ���

      y_AvrRad_yz = y_IntRad_yz(yz)/sum(z_Rad_Weight)

    end function y_AvrRad_yz

    function AvrLatRad_yz(yz)  ! ����ư��(�Ҹ���)��ʬ
      !
      ! 2 ����(YZ)�ʻ����ǡ����ΰ���ư��(�Ҹ���)ʿ��
      !
      ! 2 �����ǡ��� f(��,r) ���Ф���
      !
      !    ��f(��,r) r^2cos�� d��dr /(2(r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yz
      !(in) 2 ��������ư��(�Ҹ���)�ʻ����ǡ���

      real(8)                   :: AvrLatRad_yz
      !(out) ʿ����

      AvrLatRad_yz = IntLatRad_yz(yz)/(sum(y_Lat_Weight)*sum(z_Rad_Weight))

    end function AvrLatRad_yz

    !----(���ϥǡ��� xz)---
    function z_AvrLon_xz(xz)  ! ����(�Ӿ�)��ʬ
      !
      ! 2 ����(XZ)�ʻ����ǡ����η�������ʿ��.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� ��f(��,r)d��/2�� ��׻�����.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xz
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8), dimension(0:km)  :: z_AvrLon_xz 
      !(out) ����ʿ�Ѥ��줿 1 ����ư�³ʻ����ǡ���

      z_AvrLon_xz = z_IntLon_xz(xz)/sum(x_Lon_Weight)

    end function z_AvrLon_xz

    function x_AvrRad_xz(xz)  ! ư����ʬ
      !
      ! 2 ����(XZ)�ʻ����ǡ�����ư��������ʿ��.
      !
      ! 2 �����ǡ��� f(��,r) ���Ф���
      !
      !   ��f(��,r) r^2dr /((r[o]^3-r[i]^3)/3) 
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xz
      !(in) 2 ��������ư�³ʻ����ǡ���

      real(8), dimension(0:im-1)  :: x_AvrRad_xz
      !(out) ư��ʿ�Ѥ��줿 1 �������ٳʻ����ǡ���

      x_AvrRad_xz = x_IntRad_xz(xz)/sum(z_Rad_Weight)

    end function x_AvrRad_xz

    function AvrLonRad_xz(xz)  ! ����ư��(���ٱ�)��ʬ
      !
      ! 2 ����(XZ)�ʻ����ǡ����η���ư��ʿ��
      !
      ! 2 �����ǡ��� f(��,r) ���Ф��� 
      ! 
      !    ��f(��,r) r^2d��dr /(2��(r[o]^3-r[i]^3)/3)
      !
      ! ��׻�����.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xz    
      ! (in) 2 �����ʻ����ǡ���
      real(8)                                 :: AvrLonRad_xz      
      ! ��ʬ��

      AvrLonRad_xz = IntLonRad_xz(xz)/(sum(x_Lon_Weight)*sum(z_Rad_Weight))

    end function AvrLonRad_xz

    !----(���ϥǡ��� z)---
    function AvrRad_z(z)
      !
      ! 1 ����(Z)�ʻ����ǡ�����ư��������ʿ��.
      !
      ! 1 �����ǡ��� f(r) ���Ф��� ��f(r) r^2dr /((r[o]^3-r[i]^3)/3) ��
      ! �׻�����.
      !
      real(8), dimension(0:km), intent(in) :: z
      !(in) 1 ����ư�³ʻ����ǡ���
      real(8)                              :: AvrRad_z
      !(out) ʿ����

      AvrRad_z = IntRad_z(z)/sum(z_Rad_Weight)

    end function AvrRad_z

  !--------------- �ݥ�����/�ȥ������ǥ�����ʬ -----------------

    function wt_KxRGrad_wt(wt)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˷�����ʬ k��r���� = ��/�ߦˤ���Ѥ���.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(nm+1,0:lm)             :: wt_KxRGrad_wt
      !(out) ������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wt_KxRGrad_wt =  wa_Dlon_wa(wt)

    end function wt_KxRGrad_wt

    function xyz_KGrad_wt(wt)    ! k���� = cos��/r ��/�ߦ� + sin�բ�/��r
      !
      ! ���ϥ��ڥ��ȥ�ǡ������б�����ʻҥǡ����˼�������ʬ 
      !
      !    k���� = cos��/r ��/�ߦ� + sin�բ�/��r 
      !
      ! ����Ѥ������ʻҥǡ������֤����. 
      ! �����ǥ٥��ȥ� k �ϵ���濴�����̶˸�����ñ�̥٥��ȥ�Ǥ���.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyz_KGrad_wt
      !(out) ��������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      xyz_KGrad_wt =  cos(xyz_Lat)*xyz_GradLat_wt(wt) &
                    + sin(xyz_Lat)*xyz_wt(wt_Drad_wt(wt))

    end function xyz_KGrad_wt

    function wt_L2_wt(wt)
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
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(nm+1,0:lm)             :: wt_L2_wt
      !(out) L^2 �黻�Ҥ���Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wt_L2_wt = -wa_Lapla_wa(wt)

    end function wt_L2_wt

    function wt_L2Inv_wt(wt)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� L^2 �黻�Ҥεձ黻(-�տ�ʿ��ץ饷����)��
      ! ���Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� L^2 �黻�Ҥ���Ѥ�����ؿ� wt_L2_wt �εշ׻���
      ! �Ԥ��ؿ��Ǥ���.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(nm+1,0:lm)             :: wt_L2Inv_wt
      !(out) L^2 �黻�Ҥεձ黻����Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wt_L2Inv_wt = -wa_LaplaInv_wa(wt)

    end function wt_L2Inv_wt

    function wt_QOperator_wt(wt)
      !
      ! ���ϥ��ڥ��ȥ�ǡ������б�����ʻ����ǡ����˱黻�� 
      !
      !    Q=(k����-1/2(L2 k����+ k����L2)) 
      !
      ! ����Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ����֤����.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(nm+1,0:lm)             :: wt_QOperator_wt
      !(out) Q �黻�Ҥ���Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      wt_QOperator_wt = &
             wt_xyz(xyz_KGrad_wt(wt) - xyz_KGrad_wt(wt_L2_wt(wt))/2) &
           - wt_L2_wt(wt_xyz(xyz_KGrad_wt(wt)))/2

    end function wt_QOperator_wt

    function wt_RadRot_xyz_xyz(xyz_VLON,xyz_VLAT)  ! r��(����v)
      !
      ! �٥��ȥ�α��٤�ư�¥٥��ȥ������ r��(����v) ��׻�����.
      !
      ! �� 1, 2 ����(v[��], v[��])�����줾��٥��ȥ�η�����ʬ, ������ʬ��ɽ��.
      !
      !    r��(����v) = 1/cos�ա���v[��]/�ߦ� - 1/cos�ա���(v[��] cos��)/�ߦ�
      !
      ! �Υ��ڥ��ȥ� �ǡ������֤����.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_VLON
      !(in) �٥��ȥ�η�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_VLAT
      !(in) �٥��ȥ�ΰ�����ʬ

      real(8), dimension(nm+1,0:lm)     :: wt_RadRot_xyz_xyz
      !(out) �٥��ȥ�α��٤�ư�¥٥��ȥ������

      wt_RadRot_xyz_xyz = wt_wz(wa_DivLon_xya(xyz_VLAT) &
                                - wa_DivLat_xya(xyz_VLON))
      
    end function wt_RadRot_xyz_xyz

    function wt_RadRotRot_xyz_xyz_xyz(xyz_VLON,xyz_VLAT,xyz_VRAD) 
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_VLON
      !(in) �٥��ȥ�η�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_VLAT
      !(in) �٥��ȥ�ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_VRAD
      !(in) �٥��ȥ��ư����ʬ

      real(8), dimension(nm+1,0:lm)     :: wt_RadRotRot_xyz_xyz_xyz
      !(out) �٥��ȥ� v �� r��(���ߢ���v) 

      wt_RadRotRot_xyz_xyz_xyz = &
               wt_RotRad_wt(wt_wz( &
                   (wa_DivLon_xya(xyz_VLON)+ wa_DivLat_xya(xyz_VLAT)))) &
             + wt_L2_wt(wt_xyz(xyz_VRAD/xyz_RAD))

    end function wt_RadRotRot_xyz_xyz_xyz

    subroutine wt_Potential2Vector(&
         xyz_VLON,xyz_VLAT,xyz_VRAD,wt_TORPOT,wt_POLPOT)
      !
      ! �ȥ�����ݥ�����ݥƥ󥷥�릷,����ɽ�������ȯ���٥��ȥ��
      !
      !     v = ��x(��r) + ��x��x(��r) 
      !
      ! �γ���ʬ��׻�����
      !
      real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VLON
      !(out) �٥��ȥ��η�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VLAT
      !(out) �٥��ȥ��ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VRAD
      !(out) �٥��ȥ���ư����ʬ

      real(8), dimension(nm+1,0:lm), intent(in) :: wt_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension(nm+1,0:lm), intent(in) :: wt_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      xyz_VLON =   xyz_RAD * xyz_GradLat_wt(wt_TORPOT) &
                 + xya_GradLon_wa(wz_wt(wt_RotRad_wt(wt_POLPOT)))
      xyz_VLAT = - xyz_RAD * xyz_GradLon_wt(wt_TORPOT) &
                 + xya_GradLat_wa(wz_wt(wt_RotRad_wt(wt_POLPOT)))
      xyz_VRAD = xyz_wt(wt_L2_wt(wt_POLPOT))/xyz_RAD

    end subroutine wt_Potential2Vector

    subroutine wt_Potential2Rotation(&
       xyz_RotVLON,xyz_RotVLAT,xyz_RotVRAD,wt_TORPOT,wt_POLPOT)
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(OUT) :: xyz_RotVLON
      !(out) ��ž�η�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(OUT) :: xyz_RotVLAT
      !(out) ��ž�ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km), intent(OUT) :: xyz_RotVRAD
      !(out) ��ž��ư����ʬ

      ! ���ϥ٥��ȥ���ɽ���ݥƥ󥷥��
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension(nm+1,0:lm), intent(in) :: wt_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      call wt_Potential2Vector( &
           xyz_RotVLON,xyz_RotVLAT,xyz_RotVRAD, &
           -wt_Lapla_wt(wt_POLPOT), wt_TORPOT)

    end subroutine wt_Potential2Rotation

 !------------------- ��������׻� ----------------------
    subroutine wt_VGradV(xyz_VGRADV_LON,xyz_VGRADV_LAT,xyz_VGRADV_RAD, &
                          xyz_VLON,xyz_VLAT,xyz_VRAD )
      !
      ! �٥��ȥ��� v����v ��׻�����.
      !
      ! �٥��ȥ�� v=(v[��],v[��],v[r]) ���Ф���v����v�γ���ʬ��
      ! ���Τ褦�˷׻������.
      !
      !   (v����v)[��] = ����(v[��]v) + v[��]v[r]/r - v[��]v[��]tan(��)/r
      !   (v����v)[��] = ����(v[��]v) + v[��]v[r]/r - v[��]^2tan(��)/r
      !   (v����v)[r] = ����(v[r]v) + (v[��]^2+v[��]^2)/r
      !
      ! ��ȯ��®�پ���Ф��Ƥϥݥƥ󥷥�뤫�� wt_Potential2Rotation ��
      ! �Ѥ��Ʋ�ž��׻���, ������ v����v = ��(v[2^/2) - vx��xv ��
      ! �Ѥ��������褤����.
      !
      real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_LON
      !(out) (v����v) ������ʬ

      real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_LAT
      !(out) (v����v) ������ʬ

      real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_RAD
      !(out) (v����v) ư����ʬ

      real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VLON
      !(in) �٥��ȥ�� v �η�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VLAT
      !(in) �٥��ȥ�� v �ΰ�����ʬ

      real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VRAD
      !(in) �٥��ȥ�� v ��ư����ʬ

      xyz_VGRADV_LON = &
              xyz_Div_xyz_xyz_xyz( &
                  xyz_VLON * xyz_VLON, xyz_VLON*xyz_VLAT, xyz_VLON*xyz_VRAD ) &
            + xyz_VLON*xyz_VRAD/xyz_RAD              &
            - xyz_VLON*xyz_VLAT*tan(xyz_LAT)/xyz_RAD 

      xyz_VGRADV_LAT = &
              xyz_Div_xyz_xyz_xyz( &
                  xyz_VLAT*xyz_VLON, xyz_VLAT*xyz_VLAT, xyz_VLAT*xyz_VRAD ) &
            + xyz_VLAT*xyz_VRAD/xyz_RAD        &
            + xyz_VLON**2*tan(xyz_LAT)/xyz_RAD 

      xyz_VGRADV_RAD = &
              xyz_Div_xyz_xyz_xyz( &
                  xyz_VRAD*xyz_VLON, xyz_VRAD*xyz_VLAT, xyz_VRAD*xyz_VRAD ) &
            - (xyz_VLON**2 + xyz_VLAT**2)/xyz_RAD 

    end subroutine wt_VGradV

  !--------------- ��ַ׻� -----------------
    function Interpolate_wt(wt_data,alon,alat,arad)
      !
      ! ���� alon, ���� alat ư�� arad �ˤ�����ؿ��ͤ�
      ! ���ε���Ĵ���Ѵ����� wa_data ������ַ׻�����
      !
      real(8), intent(IN) :: wt_data(nm+1,0:km)       ! ���ڥ��ȥ�ǡ���
      real(8), intent(IN) :: alon                     ! ��֤������(����)
      real(8), intent(IN) :: alat                     ! ��֤������(����)
      real(8), intent(IN) :: arad                     ! ��֤������(ư��)
      real(8) :: Interpolate_wt                       ! ��֤�����
      
      Interpolate_wt = &
           Interpolate_w(a_Interpolate_at(wt_data,arad),alon,alat)

    end function Interpolate_wt

  !--------------- �ݥ�����/�ȥ������ǥ��ѥ��ڥ��ȥ���� ----------------

    function nmz_ToroidalEnergySpectrum_wt(wt_TORPOT)
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
      !    wt_VMiss �ˤ�ä�����Ǥ��� (����ͤ� -999.0)
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,-nm:nm,0:km) :: nmz_ToroidalEnergySpectrum_wt
      !(out) ���ͥ륮�����ڥ��ȥ�ȥ�������ʬ

      real(8), dimension(nm+1,0:km) ::wz_DATA   ! ����ΰ�
      integer :: n, m

      nmz_ToroidalEnergySpectrum_wt = wt_VMiss

      wz_DATA = wz_wt(wt_TORPOT)
      do n=0,nm
         do m=1,n
            nmz_ToroidalEnergySpectrum_wt(n,m,:) = 0.0D0
            nmz_ToroidalEnergySpectrum_wt(n,-m,:) = 0.0D0
         enddo
         nmz_ToroidalEnergySpectrum_wt(n,0,:) &
              = 0.5 * n*(n+1)* (4*pi) * z_Rad**2 &
                * wz_DATA(l_nm(n,0),:)**2
      enddo

    end function nmz_ToroidalEnergySpectrum_wt

    function nz_ToroidalEnergySpectrum_wt(wt_TORPOT)
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
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,0:km) :: nz_ToroidalEnergySpectrum_wt 
      !(out) ���ͥ륮�����ڥ��ȥ�ȥ�������ʬ

      real(8), dimension(nm+1,0:km) ::wz_DATA   ! ����ΰ�
      integer :: n, m

      wz_DATA = wz_wt(wt_TORPOT)
      do n=0,nm
         nz_ToroidalEnergySpectrum_wt(n,:) &
              = 0.5 * n*(n+1)* (4*pi) * z_Rad**2 * wz_Data(l_nm(n,0),:)**2
      enddo

    end function nz_ToroidalEnergySpectrum_wt

    function nmz_PoloidalEnergySpectrum_wt(wt_POLPOT)
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
      !    ��»�ͤ��ͤϥ⥸�塼���ѿ� wt_VMiss �ˤ�ä�����Ǥ���
      !    (����ͤ� -999.0)
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,-nm:nm,0:km) :: nmz_PoloidalEnergySpectrum_wt 
      !(out) ���ͥ륮�����ڥ��ȥ�ݥ�������ʬ


      real(8), dimension(nm+1,0:km) ::wz_DATA1   ! ����ΰ�
      real(8), dimension(nm+1,0:km) ::wz_DATA2   ! ����ΰ�
      integer :: n, m

      nmz_PoloidalEnergySpectrum_wt = wt_VMiss

      wz_Data1 = wz_wt(wt_POLPOT)
      wz_Data2 = wz_Rad*wz_wt(wt_DRad_wt(wt_POLPOT)) &    ! d(r��)/dr
               + wz_wt(wt_POLPOT)                         ! = rd��/dr+��


      do n=0,nm
         do m=1,n
            nmz_PoloidalEnergySpectrum_wt(n,m,:) = 0.0D0
            nmz_PoloidalEnergySpectrum_wt(n,-m,:) = 0.0D0
         enddo
         nmz_PoloidalEnergySpectrum_wt(n,0,:) = &
                 + 0.5* n*(n+1)* (4*pi) &
                 *( wz_Data2(l_nm(n,0),:)**2  &
                   + n*(n+1)*wz_Data1(l_nm(n,0),:)**2 )
      enddo

    end function nmz_PoloidalEnergySpectrum_wt

    function nz_PoloidalEnergySpectrum_wt(wt_POLPOT)
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
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,0:km) :: nz_PoloidalEnergySpectrum_wt
      !(out) ���ͥ륮�����ڥ��ȥ�ݥ�������ʬ

      real(8), dimension(nm+1,0:km) ::wz_DATA1   ! ����ΰ�
      real(8), dimension(nm+1,0:km) ::wz_DATA2   ! ����ΰ�
      integer :: n, m

      wz_Data1 = wz_wt(wt_POLPOT)
      wz_Data2 = wz_Rad*wz_wt(wt_DRad_wt(wt_POLPOT)) &    ! d(r��)/dr
               + wz_wt(wt_POLPOT)                         ! = rd��/dr+��

      do n=0,nm
         nz_PoloidalEnergySpectrum_wt(n,:) = &
              + 0.5* n*(n+1)* (4*pi) &
              *( wz_Data2(l_nm(n,0),:)**2  &
                + n*(n+1)*wz_Data1(l_nm(n,0),:)**2 )
      enddo

    end function nz_PoloidalEnergySpectrum_wt


  !--------------- ���������� -----------------

    subroutine wt_BoundariesTau(wt,values,cond)
      !
      ! ���ڥ��ȥ�ǡ����˥ǥ��ꥯ�졦�Υ��ޥ󶭳�����Ŭ�Ѥ���
      ! Chebyshev ���֤Ǥζ������Ŭ��(����ˡ)
      !
      ! �����ӥ����ն��֤ˤ����ƶ��������������٤��⼡�η�����
      ! ������ˡ��ȤäƤ���(����ˡ).
      !
      real(8), dimension(nm+1,0:lm),intent(inout)      :: wt
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension(nm+1,2), intent(in), optional :: values
              !(in) �����Ǥ� ��/���� ʬ�ۤ��ʿ���ڥ��ȥ��Ѵ�������Τ�Ϳ����. 
              !     ��ά������/���� 0 �Ȥʤ�. 

      character(len=2), intent(in), optional             :: cond
              !(in) �������. ��ά���� 'DD'
              !        DD : ξü�ǥ��ꥯ����
              !        DN : ��ü�ǥ��ꥯ��, ��ü�Υ��ޥ���
              !        ND : ��ü�Υ��ޥ�, ��ü�ǥ��ꥯ����
              !        NN : ξü�Υ��ޥ���

      if (.not. present(cond)) then
         if (present(values)) then
            call at_BoundariesTau_DD(wt,values)
         else
            call at_BoundariesTau_DD(wt)
         endif
         return
      endif

      select case(cond)
      case ('NN')
         if (present(values)) then
            call at_BoundariesTau_NN(wt,values)
         else
            call at_BoundariesTau_NN(wt)
         endif
      case ('DN')
         if (present(values)) then
            call at_BoundariesTau_DN(wt,values)
         else
            call at_BoundariesTau_DN(wt)
         endif
      case ('ND')
         if (present(values)) then
            call at_BoundariesTau_ND(wt,values)
         else
            call at_BoundariesTau_ND(wt)
         endif
      case ('DD')
         if (present(values)) then
            call at_BoundariesTau_DD(wt,values)
         else
            call at_BoundariesTau_DD(wt)
         endif
      case default
         call MessageNotify('E','wt_BoundariesTau','B.C. not supported')
      end select

    end subroutine wt_BoundariesTau

    subroutine wt_BoundariesGrid(wt,values,cond)
      !
      ! ���ڥ��ȥ�ǡ����˥ǥ��ꥯ�졦�Υ��ޥ󶭳�����Ŭ�Ѥ���
      ! �¶��֤Ǥζ������Ŭ��
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! wt_Initial �ˤ����ꤹ������ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��
      ! ���������Ƥ���ɬ�פ�����.
      !
      real(8), dimension(nm+1,0:lm),intent(inout)      :: wt
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension(nm+1,2), intent(in), optional :: values
              !(in) �����Ǥ� ��/���� ʬ�ۤ��ʿ���ڥ��ȥ��Ѵ�������Τ�Ϳ����. 
              !    ��ά������/���� 0 �Ȥʤ�. 

      character(len=2), intent(in), optional             :: cond
              !(in) �������. ��ά���� 'DD'
              !        DD : ξü�ǥ��ꥯ����
              !        DN : ��ü�ǥ��ꥯ��, ��ü�Υ��ޥ���
              !        ND : ��ü�Υ��ޥ�, ��ü�ǥ��ꥯ����
              !        NN : ξü�Υ��ޥ���

      if (.not. present(cond)) then
         if (present(values)) then
            call at_boundariesGrid_DD(wt,values)
         else
            call at_boundariesGrid_DD(wt)
         endif
         return
      endif

      select case(cond)
      case ('NN')
         if (present(values)) then
            call at_BoundariesGrid_NN(wt,values)
         else
            call at_BoundariesGrid_NN(wt)
         endif
      case ('DN')
         if (present(values)) then
            call at_BoundariesGrid_DN(wt,values)
         else
            call at_BoundariesGrid_DN(wt)
         endif
      case ('ND')
         if (present(values)) then
            call at_BoundariesGrid_ND(wt,values)
         else
            call at_BoundariesGrid_ND(wt)
         endif
      case ('DD')
         if (present(values)) then
            call at_BoundariesGrid_DD(wt,values)
         else
            call at_BoundariesGrid_DD(wt)
         endif
      case default
         call MessageNotify('E','wt_BoundariesGrid','B.C. not supported')
      end select

    end subroutine wt_BoundariesGrid

    subroutine wt_TorBoundariesTau(wt_TORPOT,values,cond,new) 
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
      real(8), dimension(nm+1,0:lm),intent(inout)      :: wt_TORPOT
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension(nm+1,2), intent(in), optional :: values
              !(in) ξü�����ǤΥȥ�����ݥƥ󥷥��
              !     Ǵ����λ��Τ�ͭ��

      character(len=2), intent(in), optional  :: cond
              !(in) ������凉���å�. ��ά���� 'RR'
              !     RR    : ξüǴ����
              !     RF    : ��üǴ��, ��ü���Ϥʤ����
              !     FR    : ��ü���Ϥʤ�, ��üǴ����
              !     FF    : ξü���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:), allocatable  :: alu
      integer, dimension(:), allocatable    :: kp
      real(8), dimension(0:lm,0:lm)         :: tt_data
      real(8), dimension(0:lm,0:km)         :: tg_data
      logical                               :: rigid1, rigid2   ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid1=.TRUE. ; rigid2=.TRUE.
      else
         select case (cond)
         case ('RR')
            rigid1 = .TRUE.  ; rigid2 = .TRUE.
         case ('RF')
            rigid1 = .TRUE.  ; rigid2 = .FALSE.
         case ('FR')
            rigid1 = .FALSE. ; rigid2 = .TRUE.
         case ('FF')
            rigid1 = .FALSE. ; rigid2 = .FALSE.
         case default
            call MessageNotify('E','wt_TorBoundariesTau','B.C. not supported')
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
         allocate(alu(0:lm,0:lm),kp(0:lm))

         tt_data = 0.0D0
         do l=0,lm
            tt_data(l,l)=1
         enddo
         alu = tt_data

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid1 ) then
            tg_data = az_at(tt_data)
         else
            tg_data = az_at(at_dr_at(at_az( &
                 az_at(tt_data)/spread(z_rad,1,lm+1))))
         endif
         alu(lm-1,:) = tg_data(:,0)       ! ���� k=0 �Ǥξ�Ｐ����

         if ( rigid2 ) then
            tg_data = az_at(tt_data)    
         else
            tg_data = az_at(at_dr_at(at_az( &
                 az_at(tt_data)/spread(z_rad,1,lm+1))))
         endif
         alu(lm,:)   = tg_data(:,km)      ! ���� k=km �Ǥξ�Ｐ����

         call ludecomp(alu,kp)

         if ( rigid1 .AND. present(values) ) then 
            call MessageNotify('M','wt_TorBoundariesTau',&
                 'Toroidal potential at k=0 was given by the optional variable.')
         else if ( rigid1 .AND. (.NOT.present(values)) ) then
            call MessageNotify('M','wt_TorBoundariesTau',&
                 'Toroidal potential at k=0 was set to zero.')
         else if ( (.NOT. rigid1) .AND. present(values) ) then
            call MessageNotify('W','wt_TorBoundariesTau',&
                 'Boundary value k=0 cannot be set under stress-free condition.')
         endif

         if ( rigid2 .AND. present(values) ) then 
            call MessageNotify('M','wt_TorBoundariesTau',&
                 'Toroidal potential at k=0 was given by the optional variable.')
         else if ( rigid2 .AND. (.NOT.present(values)) ) then
            call MessageNotify('M','wt_TorBoundariesTau',&
                 'Toroidal potential at k=0 was set to zero.')
         else if ( (.NOT. rigid2) .AND. present(values) ) then
            call MessageNotify('W','wt_TorBoundariesTau',&
                 'Boundary value k=0 cannot be set under stress-free condition.')
         endif

         call MessageNotify('M','wt_TorBoundariesTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      if ( rigid1 .AND. present(values) ) then
         wt_torpot(:,lm-1) = values(:,1)
      else
         wt_torpot(:,lm-1) = 0.0D0
      endif
      if ( rigid2 .AND. present(values) ) then
         wt_torpot(:,lm)   = values(:,2)
      else
         wt_torpot(:,lm) = 0.0D0
      endif

      wt_torpot = lusolve(alu,kp,wt_TORPOT)

    end subroutine wt_TorBoundariesTau

    subroutine wt_TorBoundariesGrid(wt_TORPOT,values,cond,new) 
      !
      ! ®�٥ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! �¶��֤Ǥζ������Ŭ��
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! wt_Initial �ˤ����ꤹ������ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��
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
      real(8), dimension(nm+1,0:lm),intent(inout)      :: wt_TORPOT
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension(nm+1,2), intent(in), optional :: values
              !(in) ξü�����ǤΥȥ�����ݥƥ󥷥��
              !     Ǵ����λ��Τ�ͭ��

      character(len=2), intent(in), optional  :: cond
              !(in) ������凉���å�. ��ά���� 'RR'
              !     RR    : ξüǴ����
              !     RF    : ��üǴ��, ��ü���Ϥʤ����
              !     FR    : ��ü���Ϥʤ�, ��üǴ����
              !     FF    : ξü���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(nm+1,0:km):: wz_TORPOT
      real(8), dimension(:,:), allocatable  :: alu
      integer, dimension(:), allocatable    :: kp
      real(8), dimension(0:lm,0:lm)         :: tt_data
      real(8), dimension(0:lm,0:km)         :: tz_data
      logical                               :: rigid1, rigid2   ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid1=.TRUE. ; rigid2=.TRUE.
      else
         select case (cond)
         case ('RR')
            rigid1 = .TRUE.  ; rigid2 = .TRUE.
         case ('RF')
            rigid1 = .TRUE.  ; rigid2 = .FALSE.
         case ('FR')
            rigid1 = .FALSE. ; rigid2 = .TRUE.
         case ('FF')
            rigid1 = .FALSE. ; rigid2 = .FALSE.
         case default
            call MessageNotify('E','wt_TorBoundariesGrid','B.C. not supported')
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
            call MessageNotify('E','TorBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu(0:km,0:lm),kp(0:lm))

         tt_data = 0.0D0
         do l=0,lm
            tt_data(l,l)=1
         enddo
         tz_data = az_at(tt_data)
         alu = transpose(tz_data)       ! �����ΰ�Ǥ��ͤϤ��Τޤ�

         if ( rigid1 ) then
            tz_data = az_at(tt_data)
         else
            tz_data = az_at(at_dr_at(at_az( &
                 az_at(tt_data)/spread(z_rad,1,lm+1))))
         endif
         alu(0,:) = tz_data(:,0)        ! ���� k=0 �Ǥξ�Ｐ����

         if ( rigid2 ) then
            tz_data = az_at(tt_data)
         else
            tz_data = az_at(at_dr_at(at_az( &
                 az_at(tt_data)/spread(z_rad,1,lm+1))))
         endif
         alu(km,:)   = tz_data(:,km)    ! ���� k=km �Ǥξ�Ｐ����

         call ludecomp(alu,kp)

         if ( rigid1 .AND. present(values) ) then 
            call MessageNotify('M','wt_TorBoundariesGrid',&
                 'Toroidal potential at k=0 was given by the optional variable.')
         else if ( rigid1 .AND. (.NOT.present(values)) ) then
            call MessageNotify('M','wt_TorBoundariesGrid',&
                 'Toroidal potential at k=0 was set to zero.')
         else if ( (.NOT. rigid1) .AND. present(values) ) then
            call MessageNotify('W','wt_TorBoundariesGrid',&
                 'Boundary value at k=0 cannot be set under stress-free condition.')
         endif

         if ( rigid2 .AND. present(values) ) then 
            call MessageNotify('M','wt_TorBoundariesGrid',&
                 'Toroidal potential at k=km was given by the optional variable.')
         else if ( rigid2 .AND. (.NOT.present(values)) ) then
            call MessageNotify('M','wt_TorBoundariesGrid',&
                 'Toroidal potential at k=km was set to zero.')
         else if ( (.NOT. rigid2) .AND. present(values) ) then
            call MessageNotify('W','wt_TorBoundariesGrid',&
                 'Boundary value at k=km cannot be set under stress-free condition.')
         endif

         call MessageNotify('M','wt_TorBoundariesGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wz_TorPot       = wz_wt(wt_TorPot)

      if ( rigid1 .AND. present(values) ) then
         wz_TorPot(:,0)  = values(:,1)
      else
         wz_TorPot(:,0)  = 0.0D0
      endif

      if ( rigid2 .AND. present(values) ) then
         wz_TorPot(:,km) = values(:,2)
      else
         wz_TorPot(:,km) = 0.0D0
      endif

      wt_torpot = lusolve(alu,kp,wz_TorPot)

    end subroutine wt_TorBoundariesGrid

    function wz_LaplaPol2Pol_wz(wz,cond,new)
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵��^2������׻�����.
      !
      ! �����ӥ����ճʻ������֤Ƕ�������Ŭ�Ѥ��Ƥ���. 
      ! ���δؿ����Ѥ��뤿��ˤ� wt_Initial �ˤ����ꤹ��
      ! �����ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��������
      ! ���Ƥ���ɬ�פ�����. 
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵�� f = ��^2���������뼰��
      !
      !   ��^2�� = f
      !     �� = const. at boundaries.
      !     �ߦ�/��r = 0 at boundaries           (Ǵ����) 
      !     or ��^2��/��r^2 = 0 at boundaries    (���Ϥʤ����)
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension(nm+1,0:km),intent(in)  :: wz
              !(in) ���Ϣ�^2��ʬ��

      real(8), dimension(nm+1,0:km)             :: wz_LaplaPol2Pol_wz
              !(out) ���ϥݥ�����ݥƥ󥷥��ʬ��

      character(len=2), intent(in), optional  :: cond
              !(in) ������凉���å�. ��ά���� 'RR'
              !     RR    : ξüǴ����
              !     RF    : ��üǴ��, ��ü���Ϥʤ����
              !     FR    : ��ü���Ϥʤ�, ��üǴ����
              !     FF    : ξü���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(nm+1,0:km)  :: wz_work
      real(8), dimension(0:km,0:km)           :: gg
      real(8), dimension(0:km,0:km)           :: gg_work
      logical                                 :: rigid1, rigid2   ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k,n
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid1=.TRUE. ; rigid2=.TRUE.
      else
         select case (cond)
         case ('RR')
            rigid1 = .TRUE.  ; rigid2 = .TRUE.
         case ('RF')
            rigid1 = .TRUE.  ; rigid2 = .FALSE.
         case ('FR')
            rigid1 = .FALSE. ; rigid2 = .TRUE.
         case ('FF')
            rigid1 = .FALSE. ; rigid2 = .FALSE.
         case default
            call MessageNotify('E','wt_laplapol2pol_wt','B.C. not supported')
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
            call MessageNotify('E','wz_LaplaPol2Pol_wz', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu(nm+1,0:km,0:km),kp(nm+1,0:km))

         do k=0,km
            wz_work = 0.0D0 ; wz_work(:,k) = 1.0D0

            ! �ƿ�ʿ�ȿ��˴ؤ�����Ω�μ�
            alu(:,:,k) = wz_wt(wt_lapla_wt(wt_wz(wz_work)))
         enddo

         ! ��ư��Ū���. ή���϶����ǰ���
         gg = 0.0D0
         do k=0,km
            gg(k,k)=1.0D0
         enddo
         do n=1,nm+1
            alu(n,0,:)   = gg(:,0)
            alu(n,km,:)  = gg(:,km)
         enddo

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid1 ) then
            gg_work=az_at(at_dr_at(at_az(gg)))
         else
            gg_work=az_at(at_dr_at(at_dr_at(at_az(gg))))
         endif
         do n=1,nm+1
            alu(n,1,:) = gg_work(:,0)
         enddo

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid2 ) then
            gg_work=az_at(at_dr_at(at_az(gg)))
         else
            gg_work=az_at(at_dr_at(at_dr_at(at_az(gg))))
         endif
         do n=1,nm+1
            alu(n,km-1,:) = gg_work(:,km)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','wz_LaplaPol2Pol_wz',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wz_work         = wz
      wz_work(:,1)    = 0.0D0               ! �ϳ�Ū���
      wz_work(:,km-1) = 0.0D0               ! �ϳ�Ū���
      wz_work(:,0)    = 0.0D0               ! ��ư��Ū���
      wz_work(:,km)   = 0.0D0               ! ��ư��Ū��� 

      wz_laplapol2pol_wz = lusolve(alu,kp,wz_work)

    end function wz_LaplaPol2Pol_wz

    function wt_LaplaPol2PolGrid_wt(wt,cond,new)
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵��^2������׻�����.
      ! �����ӥ����ճʻ������֤Ƕ�������Ŭ�Ѥ��Ƥ���. 
      !
      ! ���δؿ����Ѥ��뤿��ˤ� wt_Initial �ˤ����ꤹ��
      ! �����ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��������
      ! ���Ƥ���ɬ�פ�����. 
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵�� f = ��^2���������뼰��
      !
      !    ��^2�� = f
      !      �� = const. at boundaries.
      !      �ߦ�/��r = 0 at boundaries          (Ǵ����) 
      !      or ��^2��/��r^2 = 0 at boundaries   (���Ϥʤ����)
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      ! �ǽ�Ū�˥����ӥ����շ����β��ߤ������ˤ�, wz_LaplaPol2Pol_wz ��
      ! ��٤ƥ����ӥ����� -- �ʻ����Ѵ��� 1 ��ʬ���ʤ��ƺѤ�.
      !
      real(8), dimension(nm+1,0:lm),intent(in)  :: wt
              !(in) ���Ϣ�^2��ʬ��

      real(8), dimension(nm+1,0:lm)             :: wt_LaplaPol2PolGrid_wt
              !(out) ���ϥݥ�����ݥƥ󥷥��ʬ��

      character(len=2), intent(in), optional  :: cond
              !(in) ������凉���å�. ��ά���� 'RR'
              !     RR    : ξüǴ����
              !     RF    : ��üǴ��, ��ü���Ϥʤ����
              !     FR    : ��ü���Ϥʤ�, ��üǴ����
              !     FF    : ξü���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(nm+1,0:km)  :: wz_work
      real(8), dimension(nm+1,0:lm)  :: wt_work
      real(8), dimension(0:lm,0:lm)           :: tt_I
      real(8), dimension(0:lm,0:km)           :: tz_work
      logical                                 :: rigid1, rigid2   ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l,n
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid1=.TRUE. ; rigid2=.TRUE.
      else
         select case (cond)
         case ('RR')
            rigid1 = .TRUE.  ; rigid2 = .TRUE.
         case ('RF')
            rigid1 = .TRUE.  ; rigid2 = .FALSE.
         case ('FR')
            rigid1 = .FALSE. ; rigid2 = .TRUE.
         case ('FF')
            rigid1 = .FALSE. ; rigid2 = .FALSE.
         case default
            call MessageNotify('E','wt_LaplaPol2PolGrid_wt','B.C. not supported')
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
            call MessageNotify('E','wt_LaplaPol2PolGrid_wt', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu(nm+1,0:km,0:lm),kp(nm+1,0:lm))

         do l=0,lm
            wt_work = 0.0D0 ; wt_work(:,l) = 1.0D0

            ! �ƿ�ʿ�ȿ��˴ؤ�����Ω�μ�
            alu(:,:,l) = wz_wt(wt_Lapla_wt(wt_work))
         enddo

         ! ��ư��Ū���. ή���϶����ǰ���
         tt_I = 0.0D0
         do l=0,lm
            tt_I(l,l)=1.0D0
         enddo

         ! ���ŵ���Ƴ��
         tz_work = az_at(tt_I)

         do n=1,nm+1
            alu(n,0,:)  = tz_work(:,0)
            alu(n,km,:) = tz_work(:,km)
         enddo

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid1 ) then
            tz_work=az_at(at_Dr_at(tt_I))
         else
            tz_work=az_at(at_Dr_at(at_Dr_at(tt_I)))
         endif
         do n=1,nm+1
            alu(n,1,:) = tz_work(:,0)
         enddo

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid2 ) then
            tz_work=az_at(at_Dr_at(tt_I))
         else
            tz_work=az_at(at_Dr_at(at_Dr_at(tt_I)))
         endif
         do n=1,nm+1
            alu(n,km-1,:) = tz_work(:,km)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','wt_LaplaPol2PolGrid_wt',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wz_work         = wz_wt(wt)
      wz_work(:,1)    = 0.0D0               ! �ϳ�Ū���
      wz_work(:,km-1) = 0.0D0               ! �ϳ�Ū���
      wz_work(:,0)    = 0.0D0               ! ��ư��Ū���
      wz_work(:,km)   = 0.0D0               ! ��ư��Ū��� 

      wt_LaplaPol2PolGrid_wt = lusolve(alu,kp,wz_work)

    end function wt_LaplaPol2PolGrid_wt

    subroutine wt_TormagBoundariesTau(wt_TOR,new)
      
      ! ����ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! Chebyshev ���֤Ǥζ������Ŭ��
      !
      ! �����ӥ����ն��֤ˤ����ƶ��������������٤��⼡�η�����������ˡ��
      ! �ȤäƤ���(����ˡ). ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ�
      ! �б����Ƥ���. ���ξ��, ����ȥ�����ݥƥ󥷥��ζ�������
      !
      ! ��¦
      !    wt_psi = 0   at the outer boundary
      ! ��¦
      !    wt_psi = 0       at the inner boundary
      ! 
      ! �Ǥ��뤫�� wt_Boundaries ���б���ǽ����, ����Τ������Ӻ������Ƥ���.
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension(nm+1,0:lm),intent(inout)   :: wt_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: tt_I
      real(8), dimension(:,:), allocatable    :: tz_PSI

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l, n
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
         if ( allocated(tt_I) ) deallocate(tt_I)
         if ( allocated(tz_PSI) ) deallocate(tz_PSI)
         allocate(alu(nm+1,0:lm,0:lm),kp(nm+1,0:lm))
         allocate(tt_I(0:lm,0:lm),tz_PSI(0:lm,0:km))

         tt_I = 0.0D0
         do l=0,lm
            tt_I(l,l)=1.0D0
         enddo
         do n=1,nm+1
            alu(n,:,:) = tt_I
         enddo

         ! ���ŵ���Ƴ��
         tz_PSI = az_at(tt_I)

         do n=1,nm+1
            alu(n,lm-1,:) = tz_PSI(:,0)
            alu(n,lm,:)   = tz_PSI(:,km)
         enddo
         call ludecomp(alu,kp)

         deallocate(tt_I,tz_PSI)

         call MessageNotify('M','TormagBoundariesTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wt_TOR(:,lm-1) = 0.0D0
      wt_TOR(:,lm)   = 0.0D0
      wt_TOR = lusolve(alu,kp,wt_TOR)

    end subroutine wt_TormagBoundariesTau

    subroutine wt_TormagBoundariesGrid(wt_TOR,new)
      !
      ! ����ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! ��ľ�¶��֤Ǥζ������Ŭ��.
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! wt_Initial �ˤ����ꤹ������ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��
      ! ���������Ƥ���ɬ�פ�����. 
      !
      ! ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ��б����Ƥ���. 
      ! ���ξ��, ����ȥ�����ݥƥ󥷥��ζ�������
      !
      ! ��¦
      !    wt_psi = 0   at the outer boundary
      ! ��¦
      !    wt_psi = 0       at the inner boundary
      ! 
      ! �Ǥ���Τ� wt_Boundaries ���б���ǽ����, ����Τ������Ӻ������Ƥ���
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension(nm+1,0:lm),intent(inout)   :: wt_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: tt_I
      real(8), dimension(:,:), allocatable    :: tz_PSI
      real(8), dimension(nm+1,0:km)  :: wz_TOR

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l, n
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lm /= km ) then
            call MessageNotify('E','TorMagBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(tt_I) ) deallocate(tt_I)
         if ( allocated(tz_PSI) ) deallocate(tz_PSI)
         allocate(alu(nm+1,0:km,0:lm),kp(nm+1,0:lm))
         allocate(tt_I(0:lm,0:lm),tz_PSI(0:lm,0:km))

         tt_I = 0.0D0
         do l=0,lm
            tt_I(l,l)=1.0D0
         enddo
         do n=1,nm+1
            alu(n,:,:) = transpose(az_at(tt_I))   ! �����ΰ���ͤ��Τޤ�.
         enddo

         ! ���ŵ���Ƴ��
         tz_PSI = az_at(tt_I)

         do n=1,nm+1
            alu(n,0,:) = tz_PSI(:,0)
            alu(n,km,:)   = tz_PSI(:,km)
         enddo
         call ludecomp(alu,kp)

         deallocate(tt_I,tz_PSI)

         call MessageNotify('M','TormagBoundariesGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif
      
      wz_TOR       = wz_wt(wt_TOR)
      wz_TOR(:,0)  = 0.0D0
      wz_TOR(:,km) = 0.0D0
      wt_TOR = lusolve(alu,kp,wz_TOR)

    end subroutine wt_TormagBoundariesGrid

    subroutine wt_PolmagBoundariesTau(wt_POL,new)
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
      !  * ��¦���� : dh/dr - nh/r = 0
      !
      ! �Ǥ���. ������ n �� h �ο�ʿ���ȿ��Ǥ���. 
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension(nm+1,0:lm),intent(inout)   :: wt_POL
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: tt_I
      real(8), dimension(:,:), allocatable    :: tz_PSI
      real(8), dimension(:,:), allocatable    :: tz_DPSIDR

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
         if ( allocated(tt_I) ) deallocate(tt_I)
         if ( allocated(tz_PSI) ) deallocate(tz_PSI)
         if ( allocated(tz_DPSIDR) ) deallocate(tz_DPSIDR)

         allocate(alu(nm+1,0:lm,0:lm),kp(nm+1,0:lm))
         allocate(tt_I(0:lm,0:lm),tz_PSI(0:lm,0:km),tz_DPSIDR(0:lm,0:km))

         tt_I = 0.0D0
         do l=0,lm
            tt_I(l,l)=1.0D0
         enddo
         do n=1,nm+1
            alu(n,:,:) = tt_I
         enddo

         ! ���ŵ���Ƴ��
         tz_PSI = az_at(tt_I)
         tz_DPSIDR = az_at(at_dr_at(tt_I))

         do n=1,nm+1
            nn=nm_l(n)
            alu(n,lm-1,:) = tz_DPSIDR(:,0) + (nn(1)+1) * tz_PSI(:,0)/z_RAD(0)
            alu(n,lm,:)   = tz_DPSIDR(:,km) - nn(1) * tz_PSI(:,km)/z_RAD(km)
         enddo
         call ludecomp(alu,kp)

         deallocate(tt_I,tz_PSI,tz_DPSIDR)

         call MessageNotify('M','PolmagBoundariesTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wt_POL(:,lm-1) = 0.0D0
      wt_POL(:,lm)   = 0.0D0
      wt_POL = lusolve(alu,kp,wt_POL)

    end subroutine wt_PolmagBoundariesTau

    subroutine wt_PolmagBoundariesGrid(wt_POL,new)
      !
      ! ����ݥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! ��ľ�¶��֤Ǥζ������Ŭ��. 
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! wt_Initial �ˤ����ꤹ������ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��
      ! ���������Ƥ���ɬ�פ�����. 
      !
      ! ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ��б����Ƥ���. 
      ! ���ξ��, ����ݥ�����ݥƥ󥷥��γƿ�ʿ���ڥ��ȥ���ʬ h ��
      ! �������ƶ�����郎Ϳ�����,
      !
      !  * ��¦���� : dh/dr + (n+1)h/r = 0
      !  * ��¦���� : dh/dr - nh/r = 0
      !
      ! �Ǥ���. ������ n �� h �ο�ʿ���ȿ��Ǥ���. 
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension(nm+1,0:lm),intent(inout)   :: wt_POL
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: tt_I
      real(8), dimension(:,:), allocatable    :: tz_PSI
      real(8), dimension(:,:), allocatable    :: tz_DPSIDR
      real(8), dimension(nm+1,0:km)  :: wz_POL

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
            call MessageNotify('E','PolMagBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(tt_I) ) deallocate(tt_I)
         if ( allocated(tz_PSI) ) deallocate(tz_PSI)
         if ( allocated(tz_DPSIDR) ) deallocate(tz_DPSIDR)

         allocate(alu(nm+1,0:lm,0:lm),kp(nm+1,0:lm))
         allocate(tt_I(0:lm,0:lm),tz_PSI(0:lm,0:km),tz_DPSIDR(0:lm,0:km))

         tt_I = 0.0D0
         do l=0,lm
            tt_I(l,l)=1.0D0
         enddo
         do n=1,nm+1
            alu(n,:,:) = transpose(az_at(tt_I))  ! �����ΰ���ͤ���¸
         enddo

         ! ���ŵ���Ƴ��
         tz_PSI = az_at(tt_I)
         tz_DPSIDR = az_at(at_dr_at(tt_I))

         do n=1,nm+1
            nn=nm_l(n)
            alu(n,0,:)  = tz_DPSIDR(:,0) + (nn(1)+1) * tz_PSI(:,0)/z_RAD(0)
            alu(n,km,:) = tz_DPSIDR(:,km) - nn(1) * tz_PSI(:,km)/z_RAD(km)
         enddo
         call ludecomp(alu,kp)

         deallocate(tt_I,tz_PSI,tz_DPSIDR)

         call MessageNotify('M','PolmagBoundariesGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wz_POL       = wz_wt(wt_POL)
      wz_POL(:,0)  = 0.0D0
      wz_POL(:,km) = 0.0D0
      wt_POL = lusolve(alu,kp,wz_POL)

    end subroutine wt_PolmagBoundariesGrid

end module wt_zonal_module
