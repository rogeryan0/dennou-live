!--
!----------------------------------------------------------------------
!     Copyright (c) 2008--2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wa_zonal_module
!
!   spml/wa_zonal_module �⥸�塼��ϵ��̾�Ǥη��������˰��ͤ��Ӿ�Ū 
!   1 ����ή�α�ư��른���ɥ�¿�༰���Ѥ������ڥ��ȥ�ˡ�ˤ�ä�
!   ���ͷ׻����뤿��� Fortran90 �ؿ����󶡤���. 
!
!   ���̾�� 1 �إ�ǥ��� w_zonal_module �⥸�塼���¿�إ�ǥ��Ѥ�
!   ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
!   �Ф����Ѵ����Ԥ���.
!
!   ������ l_module/la_module ���Ѥ��Ƥ���. �ǲ����Ǥϥ른���ɥ�
!   ¿�༰�Ѵ��Υ��󥸥�Ȥ��� ISPACK �� Fortran77 ���֥롼�����
!   �Ѥ��Ƥ���.
!
!   wa_zonal_modulde ���󶡤����ؿ������֥롼����� 2 ����Ūή�α�ư�򰷤� 
!   wa_module �⥸�塼����Ѥ����Ƥ����Τ�̾������ӥ��󥿡��ե�������
!   ���̤ˤʤ�褦���߷פ��Ƥ���. �������ä�, wa_module ���Ѥ��ƹ������줿
!   2 ������ǥ���Ӿ������˰��ͤ� 1 �������оΥ�ǥ�ؤȲ�¤����ˤ�
!   ���μ�礬ɬ�פȤʤ�. 
!
!   * use ʸ�Ǥ� wa_module �ΰ��Ѥ� wa_zonal_module ���ѹ�����. 
!   * ������礭������������ʻ����� im -> 1 ��, 
!     ��ʿ�ȿ��� (nm+1)**2 -> nm+1 ���ѹ�����. 
!   * DO ʸ�ǿ�ʿ�ȿ��˴ؤ��ƥ롼�פ�󤷤Ƥ���Ȥ����
!     (nm+1)**2 -> nm+1 ���ѹ�����. 
!   * gtool ���Ϥμ����ѿ��ѹ�����. 
!
!
!����  2008/12/29  �ݹ�����  wa_module ���Ȥ˼��о�¿���Ѥ˲�¤
!      2009/01/08  �ݹ�����  wa_Initial ��å����������դ��ɲ�
!
!++
module wa_zonal_module
  !
  != wa_zonal_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_zonal_module.f90,v 1.6 2009-02-28 21:33:48 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  !  spml/wa_zonal_module �⥸�塼��ϵ��̾�Ǥη��������˰��ͤ��Ӿ�Ū 
  !  1 ����ή�α�ư��른���ɥ�¿�༰���Ѥ������ڥ��ȥ�ˡ�ˤ�ä�
  !  ���ͷ׻����뤿��� Fortran90 �ؿ����󶡤���. 
  !
  !  ���̾�� 1 �إ�ǥ��� w_zonal_module �⥸�塼���¿�إ�ǥ��Ѥ�
  !  ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
  !  �Ф����Ѵ����Ԥ���.
  !
  !  ������ l_module/la_module ���Ѥ��Ƥ���. �ǲ����Ǥϥ른���ɥ�
  !  ¿�༰�Ѵ��Υ��󥸥�Ȥ��� ISPACK �� Fortran77 ���֥롼�����
  !  �Ѥ��Ƥ���.
  !
  !  wa_zonal_modulde ���󶡤����ؿ������֥롼����� 2 ����Ūή�α�ư�򰷤� 
  !  wa_module �⥸�塼����Ѥ����Ƥ����Τ�̾������ӥ��󥿡��ե�������
  !  ���̤ˤʤ�褦���߷פ��Ƥ���. �������ä�, wa_module ���Ѥ��ƹ������줿
  !  2 ������ǥ���Ӿ������˰��ͤ� 1 �������оΥ�ǥ�ؤȲ�¤����ˤ�
  !  ���μ�礬ɬ�פȤʤ�. 
  !
  !  1. use ʸ�Ǥ� wa_module �ΰ��Ѥ� wa_zonal_module ���ѹ�����. 
  !  2. ������礭������������ʻ����� im -> 1 ��, 
  !     ��ʿ�ȿ��� (nm+1)**2 -> nm+1 ���ѹ�����. 
  !  3. DO ʸ�ǿ�ʿ�ȿ��˴ؤ��ƥ롼�פ�󤷤Ƥ���Ȥ����
  !     (nm+1)**2 -> nm+1 ���ѹ�����. 
  !  4. gtool ���Ϥμ����ѿ��ѹ�����. 
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  ! w_zonal_module �ǤΤ�Τ��ɲä����ؿ����ѿ���̾���ȷ��ˤĤ��ƤΤ�
  ! ��������. w_zonal_module �Ǥδؿ����ѿ���̾���ȷ��ˤĤ��Ƥ� 
  ! w_zonal_module �γ�����򻲾Ȥ��줿��.
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (wa_, nma_, na_, xya_, xa_, ya_, w_, xy_, x_, y_, a_) ��, 
  !   �֤��ͤη��򼨤��Ƥ���.
  !   wa_  :: ��ʿ���ڥ��ȥ�ǡ������¤�� 2 ��������(���ڥ��ȥ�ǡ�����
  !           �¤Ӥ� LTPACK/ISPACK �˽��ä����)
  !   nma_ :: ���ڥ��ȥ�ǡ������¤�� 3 �������� (���ڥ��ȥ�ǡ������¤Ӥ�
  !           ���ȿ� n, �Ӿ��ȿ� m �ǻ��ꤵ��� 2 ��������)
  !   na_  :: ���ڥ��ȥ�ǡ������¤�� 2 �������� (���ڥ��ȥ�ǡ������¤Ӥ�
  !           ���ȿ� n �ǻ��ꤵ��� 1 ��������)
  !   xya_ :: 2 �����ʻ����ǡ������¤�� 3 ��������
  !   xa_  :: �������� 1 �����ʻ����ǡ������¤�� 2 ��������
  !   ya_  :: �������� 1 �����ʻ����ǡ������¤�� 2 ��������
  !
  ! * �ؿ�̾�δ֤�ʸ����(DLon, GradLat, GradLat, DivLon, DivLat, Lapla, 
  !   LaplaInv, Jacobian)��, ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_wa_wa, _wa, _xya, _xa, _ya, _w_w, _w, _xy, _x, _y) ��, 
  !   �����ѿ��η����ڥ��ȥ�ǡ�������ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _wa    :: ���ڥ��ȥ�ǡ������¤�� 2 ��������
  !   _wa_wa :: 2 �ĤΥ��ڥ��ȥ�ǡ������¤�� 2 ��������
  !   _xya   :: 2 �����ʻ����ǡ������¤�� 3 ��������
  !   _xa    :: �������� 1 �����ʻ����ǡ������¤�� 2 ��������
  !   _ya    :: �������� 1 �����ʻ����ǡ������¤�� 2 ��������
  !
  !=== �ƥǡ����μ��������
  !
  ! * xya : 2 �����ʻ����ǡ������¤�� 3 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:im-1,1:jm,:). 
  !   * im, jm �Ϥ��줾�����, ���ٺ�ɸ�γʻ������Ǥ���, ���֥롼����
  !     wa_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !   * ������ 3 �������礭���κ����ͤ� wa_Initial �����ꤷ�Ƥ���.
  !
  ! * wa : ���ڥ��ȥ�ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(nm+1,:). 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, ���֥롼���� wa_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���. 
  !   * ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ϴؿ� l_nm, nm_l �ˤ�ä�
  !     Ĵ�٤뤳�Ȥ��Ǥ���. 
  !   * ������ 3 �������礭����
  !     �����ͤ� wa_Initial �����ꤷ�Ƥ���.
  !
  ! * xa, ya : ����, �������� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����Ϥ��줾�� real(8), dimension(0:im-1,:) 
  !     ����� real(8), dimension(1:jm,:).
  !
  ! * nma : ���ڥ��ȥ�ǡ������¤�� 3 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,-nm:nm,:). 
  !   * �� 1 ��������ʿ���ȿ�, �� 2 �������Ӿ��ȿ���ɽ��. 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, 
  !     ���֥롼���� wa_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * na : ���ڥ��ȥ�ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,:). 
  !   * �� 1 ��������ʿ���ȿ���ɽ��.nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, 
  !     ���֥롼���� wa_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * wa_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ������¤�� 2 ���������Ʊ��.
  !
  ! * nma_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ������¤�� 3 ���������Ʊ��.
  !
  ! * na_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ������¤�� 2 ���������Ʊ��.
  !
  ! * xya_ �ǻϤޤ�ؿ����֤��ͤ� 2 �����ʻ����ǡ������¤�� 3 ���������
  !   Ʊ��.
  !
  ! * xa_, ya_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ������¤�� 2 ����
  !   �����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  !
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! wa_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��, 
  !            :: �����Ʊ���˷׻�����ǡ����θĿ��κ���������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! x_Lon, y_Lat     ::  �ʻ�����ɸ(����, ���ٺ�ɸ)���Ǽ���� 1 ��������
  ! x_Lon_Weight, y_Lat_Weight ::  �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! xy_Lon, xy_Lat   :: �ʻ����ǡ����η��١����ٺ�ɸ(X,Y)
  !                     (�ʻ����ǡ����� 2 ��������)
  !
  !==== �����Ѵ�
  !
  ! xya_wa, xy_w :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�(¿��, 1 ����)
  ! wa_xya, w_xy :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�(¿��, 1 ����)
  ! l_nm, nm_l :: ���ڥ��ȥ�ǡ����γ�Ǽ���֤����ȿ����Ӿ��ȿ����Ѵ� 
  !
  !==== ��ʬ
  !
  ! wa_Lapla_wa, w_Lapla_w        :: ���ڥ��ȥ�ǡ����˥�ץ饷�����
  !                                  ���Ѥ�����(¿��, 1 ����)
  ! rn                            :: ���ڥ��ȥ�ǡ����Υ�ץ饷�����
  !                                  �׻����뤿��η���
  ! irm                           :: ������ʬ�黻������
  ! wa_LaplaInv_wa, w_LaplaInv_w  :: ���ڥ��ȥ�ǡ����˥�ץ饷����ε��Ѵ���
  !                                  ���Ѥ�����(¿��, 1 ����)
  ! wa_DLon_wa, w_DLon_w          :: ���ڥ��ȥ�ǡ�����
  !                                  ������ʬ��/�ߦˤ���Ѥ�����(¿��, 1 ����)
  ! xya_GradLon_wa, xy_GradLon_w  :: ���ڥ��ȥ�ǡ�����
  !                                  ���۷�������ʬ 1/cos�ա���/�ߦ� ��
  !                                  ���Ѥ�����(¿��, 1 ����)
  ! xya_GradLat_wa, xy_GradLat_w  :: ���ڥ��ȥ�ǡ�����
  !                                  ���۷�������ʬ ��/�ߦ� ����Ѥ�����
  !                                  (¿��, 1 ����)
  ! wa_DivLon_xya, w_DivLon_xy    :: �ʻҥǡ�����
  !                                  ȯ����������ʬ 1/cos�ա���/�ߦˤ�
  !                                  ���Ѥ�����(¿��, 1 ����)
  ! wa_DivLat_xya, w_DivLat_xy    :: �ʻҥǡ�����
  !                                  ȯ����������ʬ 1/cos�ա���(g cos��)/�ߦդ�
  !                                  ���Ѥ����� (¿��, 1 ����)
  ! wa_Div_xya_xya, w_Div_xy_xy   :: �٥��ȥ���ʬ�Ǥ��� 2 �Ĥγʻҥǡ�����
  !                                  ȯ������Ѥ�����(¿��, 1 ����)
  ! wa_Jacobian_wa_wa,w_Jacobian_w_w :: 2 �ĤΥ��ڥ��ȥ�ǡ�������䥳�ӥ����
  !                                     �׻�����(¿��, 1 ����)
  !
  !
  !==== ��ʬ(��,��=sin�� ��ɸ)
  !
  ! xya_GradLambda_wa,xy_GradLambda_w  :: ���ڥ��ȥ�ǡ�����
  !                                       ���۷�������ʬ��/�ߦˤ�
  !                                       ���Ѥ�����(¿��, 1 ����)
  !
  ! xya_GradMu_wa,xy_GradMu_w          :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ
  !                                       (1-��^2)��/�ߦ̤���Ѥ�����
  !                                       (¿��, 1 ����)
  !
  ! wa_DivLambda_xya,w_DivLambda_xy    :: �ʻҥǡ�����ȯ����������ʬ
  !                                       1 /(1-��^2)����/�ߦˤ���Ѥ�����
  !                                       (¿��, 1 ����)
  !
  ! wa_DivMu_xya,w_DivMu_xy            :: �ʻҥǡ�����ȯ����������ʬ
  !                                       ��/�ߦ̤���Ѥ�����(¿��, 1 ����)
  !
  !==== ��ַ׻�
  !
  ! a_Interpolate_wa, Interpolate_w :: ���ڥ��ȥ�ǡ�������Ǥ�դ�����
  !                                    �ؿ��ͤ�׻�����.
  !
  !==== ��ʬ��ʿ��
  !
  ! a_IntLonLat_xya, a_AvrLonLat_xya :: 2 �����ʻ����ǡ�����
  !                                     ���ΰ���ʬ�����ʿ��(¿����)
  ! IntLonLat_xy, AvrLonLat_xy       :: 2 �����ʻ����ǡ�����
  !                                     ���ΰ���ʬ�����ʿ��(1 ����)
  ! ya_IntLon_xya, ya_AvrLon_xya     :: 2 �����ʻ����ǡ�����
  !                                     ����������ʬ�����ʿ��(¿����)
  ! y_IntLon_xy, y_AvrLon_xy         :: 2 �����ʻ����ǡ�����
  !                                     ����������ʬ�����ʿ��(1 ����)
  ! a_IntLon_xa, a_AvrLon_xa         :: 1 ����(X)�ʻ����ǡ�����
  !                                     ����������ʬ�����ʿ��(¿����)
  ! IntLon_x, AvrLon_x               :: 1 ����(X)�ʻ����ǡ�����
  !                                     ����������ʬ�����ʿ��(1 ����)
  ! xa_IntLat_xya, x_AvrLat_xy       :: 2 �����ʻ����ǡ�����
  !                                     ����������ʬ�����ʿ��(¿����)
  ! x_IntLat_xy, x_AvrLat_xy         :: 2 �����ʻ����ǡ�����
  !                                     ����������ʬ�����ʿ��(1 ����)
  ! a_IntLat_ya, a_AvrLat_ya         :: 1 ����(Y)�ʻ����ǡ�����
  !                                     ����������ʬ�����ʿ��(¿����)
  ! IntLat_y, AvrLat_y               :: 1 ����(Y)�ʻ����ǡ�����
  !                                     ����������ʬ�����ʿ��(1 ����)
  !
  !==== ���ڥ��ȥ����
  !
  ! nma_EnergyFromStreamfunc_wa,nm_EnergyFromStreamfunc_w   :: 
  ! ή���ؿ����饨�ͥ륮�����ڥ��ȥ��׻�����
  ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)(¿��, 1 ����)
  ! 
  ! na_EnergyFromStreamfunc_wa,n_EnergyFromStreamfunc_w     ::
  ! ή���ؿ����饨�ͥ륮�����ڥ��ȥ��׻�����
  ! (��ʿ���ȿ� n ����) (¿��, 1 ����)
  ! 
  ! nma_EnstrophyFromStreamfunc_wa,nm_EnstrophyFromStreamfunc_w   ::
  ! ή���ؿ����饨�󥹥ȥ�ե������ڥ��ȥ��׻�����
  ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)(¿��, 1 ����)
  ! 
  ! na_EnstrophyFromStreamfunc_wa,n_EnstrophyFromStreamfunc_w :: 
  ! ή���ؿ����饨�󥹥ȥ�ե������ڥ��ȥ��׻�����(��ʿ���ȿ� n ����)
  ! (¿��, 1 ����)
  ! 
  ! w_spectrum_VMiss              ::  ��»��
  !
  use dc_message, only : MessageNotify
  use w_zonal_module
  use la_module, wa_ya => la_ya, ya_wa => ya_la, &
       ya_GradLat_wa => ya_GradLat_la, wa_DivLat_ya => la_DivLat_ya, &
       wa_Lapla_wa => la_Lapla_la, wa_LaplaInv_wa => la_LaplaInv_la, &
       a_Interpolate_alat_wa => a_Interpolate_la
  
  implicit none

  integer               :: im=64            ! �ʻ���������(����)
  integer               :: jm=32            ! �ʻ���������(����)
  integer               :: km=16            ! �ؤο��κ����ͤ�����
  integer               :: nm=21            ! �����ȿ�������
  integer               :: np=1             ! OPENMP ���祹��åɿ�

  real(8) :: wa_spectrum_VMiss = -999.000   ! ��»�ͽ����

  private

  public wa_Initial                           ! �����

  public x_Lon, y_Lat                         ! �ʻҺ�ɸ
  public x_Lon_Weight, y_Lat_Weight           ! �ʻҺ�ɸ�Ť�
  public xy_Lon, xy_Lat                       ! �ʻҺ�ɸ(im,jm)
  public xy_w, w_xy, l_nm, nm_l               ! �Ѵ��ؿ�
  public xya_wa, wa_xya                       ! �Ѵ��ؿ�
  
  public rn, irm                              ! ��ץ饷����η���, ������ʬ������
  public w_Lapla_w, w_LaplaInv_w              ! ��ץ饷����ȵձ黻
  public w_DLon_w                             ! ������ʬ
  public xy_GradLon_w, xy_GradLat_w           ! ���۷���ʬ
  public w_DivLon_xy, w_DivLat_xy             ! ȯ������ʬ
  public w_Div_xy_xy                          ! ȯ������ʬ
  public w_Jacobian_w_w                       ! �䥳�ӥ���
  public xy_GradLambda_w, xy_GradMu_w         ! ���۷���ʬ(��,�̺�ɸ)
  public w_DivLambda_xy, w_DivMu_xy           ! ȯ������ʬ(��,�̺�ɸ)

  public wa_Lapla_wa, wa_LaplaInv_wa          ! ��ץ饷����ȵձ黻
  public wa_DLon_wa                           ! ������ʬ
  public xya_GradLon_wa, xya_GradLat_wa       ! ���۷���ʬ
  public wa_DivLon_xya, wa_DivLat_xya         ! ȯ������ʬ
  public wa_Div_xya_xya                       ! ȯ������ʬ
  public wa_Jacobian_wa_wa                    ! �䥳�ӥ���
  public xya_GradLambda_wa, xya_GradMu_wa     ! ���۷���ʬ(��,�̺�ɸ)
  public wa_DivLambda_xya, wa_DivMu_xya       ! ȯ������ʬ(��,�̺�ɸ)

  public a_Interpolate_wa, Interpolate_w      ! ��ִؿ�

  public IntLonLat_xy                         ! ���ٷ�����ʬ
  public y_IntLon_xy, IntLon_x                ! ������ʬ    
  public x_IntLat_xy, IntLat_y                ! ������ʬ    
  public AvrLonLat_xy                         ! ���ٷ���ʿ��
  public y_AvrLon_xy, AvrLon_x                ! ����ʿ��    
  public x_AvrLat_xy, AvrLat_y                ! ����ʿ��    

  public a_IntLonLat_xya                      ! ���ٷ�����ʬ
  public ya_IntLon_xya, a_IntLon_xa           ! ������ʬ    
  public xa_IntLat_xya, a_IntLat_ya           ! ������ʬ    
  public a_AvrLonLat_xya                      ! ���ٷ���ʿ��
  public ya_AvrLon_xya, a_AvrLon_xa           ! ����ʿ��    
  public xa_AvrLat_xya, a_AvrLat_ya           ! ����ʿ��    

  public nma_EnergyFromStreamfunc_wa          ! ���ͥ륮�����ڥ��ȥ�
                                              ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public na_EnergyFromStreamfunc_wa           ! ���ͥ륮�����ڥ��ȥ�
                                              ! (��ʿ���ȿ� n ����)
  public nma_EnstrophyFromStreamfunc_wa       ! ���󥹥ȥ�ե������ڥ��ȥ�
                                              ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public na_EnstrophyFromStreamfunc_wa        ! ���󥹥ȥ�ե������ڥ��ȥ�
                                              !  (��ʿ���ȿ� n ����)
  public wa_spectrum_VMiss                    ! ��»��

  public nm_EnergyFromStreamfunc_w          ! ���ͥ륮�����ڥ��ȥ�           
                                            ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public n_EnergyFromStreamfunc_w           ! ���ͥ륮�����ڥ��ȥ�
                                            ! (��ʿ���ȿ� n ����) 
  public nm_EnstrophyFromStreamfunc_w       ! ���󥹥ȥ�ե������ڥ��ȥ�     
                                            ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public n_EnstrophyFromStreamfunc_w        ! ���󥹥ȥ�ե������ڥ��ȥ�  
                                            !  (��ʿ���ȿ� n ����)
  public w_spectrum_VMiss                   ! ��»��

  save im, jm, km, nm                       ! �ʻ�����, �����ȿ��򵭲�

contains
  !--------------- ����� -----------------
    subroutine wa_Initial(n_in,i_in,j_in,k_in,np_in)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, ����ǡ�����(�ؿ�)����� 
      ! OPENMP ���ѻ��κ��祹��åɿ������ꤹ��.
      !
      ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ�ǽ�������
      ! ���ʤ���Фʤ�ʤ�. 
      !
      ! ����ǡ����ؿ��� wa_module �Ȥθߴ����Τ���ˤ����Ƥ���. 
      ! �ºݤˤϤ�������ʾ���ؿ��Υǡ�������������갷����. 
      !
      ! np_in �� wa_module �Ȥθߴ����Τ���ˤ����Ƥ�������Ǥ���. 
      ! OPENMP �׻��ϼ�������Ƥ��ʤ�. 
      !
      integer,intent(in) :: i_in                ! �ʻ�����(����)
      integer,intent(in) :: j_in                ! �ʻ�����(����)
      integer,intent(in) :: n_in                ! �����ȿ�
      integer,intent(in) :: k_in                ! ����ǡ�����(�ؿ�)
      integer,intent(in), optional :: np_in     ! OPENMP �Ǥκ��祹��åɿ�

      call w_Initial(n_in,i_in,j_in)
      call la_Initial(n_in,j_in,l_init=.false.)

      nm = n_in ; im = i_in ; jm = j_in ; km = k_in

      call MessageNotify('M','wa_initial',&
           'wa_zonal_module (2009/01/08) is initialized')

    end subroutine wa_Initial

  !--------------- �����Ѵ� -----------------

    function xya_wa(wa_data,ipow,iflag)
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����(¿����).
      !
      real(8), intent(in)   :: wa_data(:,:)
      !(in) ���ڥ��ȥ�ǡ���(nm+1,:)
      !
      real(8)               :: xya_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) �ʻ����ǡ���(0:im-1,1:jm,:)

      integer, intent(in), optional  :: ipow      
      !(in) ���Ѥ����� 1/cos�� �μ���. ��ά���� 0. 

      integer, intent(in), optional  :: iflag
      !(in) �Ѵ��μ���
      !    0 : �̾�����Ѵ�
      !    1 : ������ʬ cos�ա���/�ߦ� ����Ѥ��������Ѵ�
      !   -1 : ������ʬ����Ѥ��������Ѵ�
      !    2 : sin�դ���Ѥ��������Ѵ�(�����)
      !    ��ά���� 0.
      !
      integer, parameter  :: ipow_default  = 0
      integer, parameter  :: iflag_default = 0

      integer ipval, ifval, i

      if (present(ipow)) then
         ipval = ipow
      else
         ipval = ipow_default
      endif

      if (present(iflag)) then
         ifval = iflag
      else
         ifval = iflag_default
      endif

      if ( ifval == -1 ) then                        ! ������ʬ�� 0 
         xya_wa = 0.0D0        
      else if ( ifval == 1 ) then                    ! ������ʬ
         do i=0,im-1
            xya_wa(i,:,:) = ya_GradLat_wa(wa_data) &
                 * spread(cos(y_Lat),2,size(wa_data,2))
         enddo
      else if ( ifval == 2 ) then                     ! sin�� �򤫤������Ѵ�
         do i=0,im-1
            xya_wa(i,:,:) = ya_wa(wa_data)
         enddo
         xya_wa = xya_wa * spread(sin(xy_Lat),3,size(wa_data,2))
      else
         do i=0,im-1
            xya_wa(i,:,:) = ya_wa(wa_data)
         enddo
      endif

      if ( ipval /= 0 ) then
         xya_wa = xya_wa/spread(cos(xy_Lat)**ipval,3,size(wa_data,2))
      end if

    end function xya_wa

    function wa_xya(xya_data,ipow,iflag) ! �ʻ��� -> ����Ĵ�´ؿ����ڥ��ȥ�
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����(¿����).
      !
      real(8), intent(in)   :: xya_data(0:,:,:)
      !(in) �ʻ����ǡ���(0:im-1,1:jm,:)

      real(8)               :: wa_xya(nm+1,size(xya_data,3))
      !(out) ���ڥ��ȥ�ǡ���(nm+1,:)

      integer, intent(in), optional  :: ipow
      !(in) �Ѵ�����Ʊ���˺��Ѥ����� 1/cos�� �μ���. ��ά���� 0.

      integer, intent(in), optional  :: iflag
      ! �Ѵ��μ���
      !    0 : �̾�����Ѵ�
      !   -1 : ������ʬ����Ѥ��������Ѵ�
      !    1 : ������ʬ����Ѥ��������Ѵ�
      !    2 : sin�դ���Ѥ��������Ѵ�
      !  ��ά���� 0.

      integer, parameter  :: ipow_default  = 0    ! �����å��ǥե������
      integer, parameter  :: iflag_default = 0    ! �����å��ǥե������

      integer ipval, ifval

      real(8)             :: xya_work(0:im-1,1:jm,size(xya_data,3))
      ! �ʻ����ǡ����������

      if (present(ipow)) then
         ipval = ipow
      else
         ipval = ipow_default
      endif

      if (present(iflag)) then
         ifval = iflag
      else
         ifval = iflag_default
      endif

      xya_work = xya_data/spread(cos(xy_Lat)**ipval,3,size(xya_data,3))

      if ( ifval == -1 ) then                         ! ������ʬ�� 0 
         wa_xya = 0.0D0        
      else if ( ifval == 1 ) then                     ! ������ʬ
         xya_work = xya_work * spread(cos(xy_Lat),3,size(xya_data,3))
         wa_xya = wa_DivLat_ya(xya_work(0,:,:))
      else if ( ifval == 2 ) then                     ! sin�� �򤫤������Ѵ�
         xya_work = xya_work * spread(sin(xy_Lat),3,size(xya_data,3))
         wa_xya = wa_ya(xya_work(0,:,:))
      else
         wa_xya = wa_ya(xya_work(0,:,:))
      endif
      
    end function wa_xya

  !--------------- ��ʬ�׻� -----------------
    function wa_DLon_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˷�����ʬ ��/�ߦ� ����Ѥ�����(¿����).
      !
      ! ���ڥ��ȥ�ǡ����η�����ʬ�Ȥ�, �б�����ʻ����ǡ�����
      ! ������ʬ��/�ߦˤ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      ! 
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      real(8)              :: wa_DLon_wa(nm+1,size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ����η�����ʬ

      wa_DLon_wa = 0.0D0

    end function wa_DLon_wa

    function xya_GradLon_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/cos�ա���/�ߦ� ��
      ! ���Ѥ������ʻ����ǡ������֤�(¿����).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      real(8)              :: xya_GradLon_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      xya_GradLon_wa = xya_wa(wa_data,ipow=1,iflag=-1)

    end function xya_GradLon_wa

    function xya_GradLat_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ�����
      ! �ʻ����ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      real(8)              :: xya_GradLat_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      xya_GradLat_wa = xya_wa(wa_data,ipow=1,iflag=1)

    end function xya_GradLat_wa

    function wa_DivLon_xya(xya_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: xya_data(0:,:,:)
      !(in) ���ϳʻ����ǡ���
      real(8)              :: wa_DivLon_xya(nm+1,size(xya_data,3))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      wa_DivLon_xya = wa_xya(xya_data,ipow=1,iflag=-1)

    end function wa_DivLon_xya

    function wa_DivLat_xya(xya_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���(f cos��)/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: xya_data(0:,:,:)
      !(in) ���ϳʻ����ǡ���
      real(8)              :: wa_DivLat_xya(nm+1,size(xya_data,3))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      wa_DivLat_xya = wa_xya(xya_data,ipow=1,iflag=1)

    end function wa_DivLat_xya

    function wa_Div_xya_xya(xya_u,xya_v)
      !
      ! 2 �Ĥ����ϳʻ����ǡ�����٥��ȥ���ʬ�Ȥ���ȯ����׻���, 
      ! ���ڥ��ȥ�ǡ����Ȥ����֤�(¿����).
      !
      real(8), intent(in)  :: xya_u(0:,:,:)
      !(in) �٥��ȥ������ʬ�γʻ����ǡ���
      real(8), intent(in)  :: xya_v(0:,:,:)
      !(in) �٥��ȥ������ʬ�γʻ����ǡ���
      real(8)              :: wa_Div_xya_xya(nm+1,size(xya_u,3))
      !(out) 2 �Ĥ����ϳʻ����ǡ�����٥��ȥ���ʬ�Ȥ���ȯ���Υ��ڥ��ȥ�ǡ���

      wa_Div_xya_xya = wa_DivLon_xya(xya_u) + wa_DivLat_xya(xya_v)

    end function wa_Div_xya_xya

    function wa_Jacobian_wa_wa(wa_a,wa_b)
      ! 2 �ĤΥ��ڥ��ȥ�ǡ����˥䥳�ӥ���
      !
      !   J(f,g) = ��f/�ߦˡ���g/�ߦ� - ��g/�ߦˡ���f/�ߦ�
      !          = ��f/�ߦˡ�1/cos�ա���g/�ߦ�
      !             - ��g/�ߦˡ�1/cos�ա���f/�ߦ�
      !
      ! ����Ѥ�����(¿����).
      !
      real(8), intent(in) :: wa_a(:,:)
      !(in) 1���ܤ����ϥ��ڥ��ȥ�ǡ���
      real(8), intent(in) :: wa_b(:,:)
      !(in) 2���ܤ����ϥ��ڥ��ȥ�ǡ���
      real(8)             :: wa_Jacobian_wa_wa(nm+1,size(wa_a,2))
      !(out) 2 �ĤΥ��ڥ��ȥ�ǡ����Υ䥳�ӥ���
      integer :: k

      do k=1,size(wa_a,2)
         wa_Jacobian_wa_wa(:,k) = w_Jacobian_w_w(wa_a(:,k),wa_b(:,k))
      end do
    end function wa_Jacobian_wa_wa


  !--------------- ��ʬ�׻� (��,�̺�ɸ����) -----------------
    function xya_GradLambda_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ���(¿����).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      real(8)              :: xya_GradLambda_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      xya_GradLambda_wa = xya_wa(wa_data,ipow=0,iflag=-1)

    end function xya_GradLambda_wa

    function xya_GradMu_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ (1-��^2)��/�ߦ�  (��=sin��)
      ! ����Ѥ����Ƴʻ����ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      real(8)              :: xya_GradMu_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      xya_GradMu_wa = xya_wa(wa_data,ipow=0,iflag=1)

    end function xya_GradMu_wa

    function wa_DivLambda_xya(xya_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/(1-��^2)����/�ߦ� (��=sin��) 
      ! ����Ѥ����ƥ��ڥ��ȥ�ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: xya_data(0:,:,:)
      !(in) ���ϳʻ����ǡ���
      real(8)              :: wa_DivLambda_xya(nm+1,size(xya_data,3))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      wa_DivLambda_xya = wa_xya(xya_data,ipow=2,iflag=-1)

    end function wa_DivLambda_xya

    function wa_DivMu_xya(xya_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ ��/�ߦ� (��=sin��)����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: xya_data(0:,:,:)
      !(in) ���ϳʻ����ǡ���
      real(8)              :: wa_DivMu_xya(nm+1,size(xya_data,3))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      wa_DivMu_xya = wa_xya(xya_data,ipow=2,iflag=1)

    end function wa_DivMu_xya

  !--------------- ��ַ׻� -----------------
    function a_Interpolate_wa(wa_data,alon,alat)
      !
      ! ���� alon, ���� alat �ˤ�����ؿ��ͤ�
      ! ���ε���Ĵ���Ѵ����� wa_data ������ַ׻�����
      !
      real(8), intent(IN) :: wa_data(:,:)             ! ���ڥ��ȥ�ǡ���
      real(8), intent(IN) :: alon                     ! ��֤������(����)
      real(8), intent(IN) :: alat                     ! ��֤������(����)
      real(8) :: a_Interpolate_wa(size(wa_data,2))    ! ��֤�����

      integer :: k

      do k=1,size(wa_data,2)
         a_Interpolate_wa(k) = Interpolate_w(wa_data(:,k),alon,alat)
      enddo

    end function a_Interpolate_wa
      
  !--------------- ��ʬ�׻� -----------------
    function a_IntLonLat_xya(xya_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ��������ΰ���ʬ(¿����). 
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), intent(in)   :: xya_data(0:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,1:jm,*)
      real(8) :: a_IntLonLat_xya(size(xya_data,3))
      !(out) ��ʬ���줿�ǡ������¤�(*)

      a_IntLonLat_xya = a_IntLon_xa(xa_IntLat_xya(xya_data))

    end function a_IntLonLat_xya

    function xa_IntLat_xya(xya_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����ΰ���(Y)������ʬ(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,1:jm,*)
      real(8)             :: xa_IntLat_xya(0:im-1,size(xya_data,3))
      !(out) ��ʬ���줿 1 ��������(X)�ʻ����ǡ������¤�
      Integer :: j

      xa_IntLat_xya = 0.0D0
      do j=1,jm
         xa_IntLat_xya = xa_IntLat_xya + xya_data(:,j,:) * y_Lat_Weight(j)
      enddo

    end function xa_IntLat_xya

    function ya_IntLon_xya(xya_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����η���(X)������ʬ(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,1:jm,*)

      real(8)             :: ya_IntLon_xya(1:jm,size(xya_data,3))
      !(out) ��ʬ���줿 1 ��������(Y)�ʻ����ǡ������¤�

      integer :: i

      ya_IntLon_xya = 0.0D0
      do i=0,im-1
         ya_IntLon_xya = ya_IntLon_xya + xya_data(i,:,:) * x_Lon_Weight(i)
      enddo

    end function ya_IntLon_xya

    function a_IntLon_xa(xa_data)          ! ������ʬ
      !
      ! 1 ��������(X)�ʻ����ǡ����� X ������ʬ(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xa_data(0:,:)
      !(in)  1 ��������(X)�ʻ����ǡ������¤�(0:im-1,*)
      real(8)             :: a_IntLon_xa(size(xa_data,2))
      !(out) ��ʬ�ͤ��¤�(*)
      integer :: i

      a_IntLon_xa = 0.0D0
      do i=0,im-1
         a_IntLon_xa = a_IntLon_xa + xa_data(i,:) * x_Lon_Weight(i)
      enddo

    end function a_IntLon_xa

  !--------------- ʿ�ѷ׻� -----------------
    function a_AvrLonLat_xya(xya_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ��������ΰ�ʿ��(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in)   :: xya_data(0:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,1:jm,*)

      real(8) :: a_AvrLonLat_xya(size(xya_data,3))
      !(out) ʿ���ͤ��¤�(*)

      a_AvrLonLat_xya = a_AvrLon_xa(xa_AvrLat_xya(xya_data))

    end function a_AvrLonLat_xya

    function xa_AvrLat_xya(xya_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����ΰ���(Y)����ʿ��(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,1:jm,*)

      real(8)             :: xa_AvrLat_xya(0:im-1,size(xya_data,3))
      !(out) ʿ�Ѥ��줿 1 ��������(X)�ʻ����ǡ������¤�(im,*)

      xa_AvrLat_xya = xa_IntLat_xya(xya_data)/sum(y_Lat_Weight)

    end function xa_AvrLat_xya

    function ya_AvrLon_xya(xya_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����η���(X)����ʿ��(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,1:jm,*)

      real(8)             :: ya_AvrLon_xya(1:jm,size(xya_data,3))
      !(out) ʿ�Ѥ��줿 1 ��������(Y)�ʻ������¤�(1:jm,*)

      ya_AvrLon_xya = ya_IntLon_xya(xya_data)/sum(x_Lon_Weight)

    end function ya_AvrLon_xya

    function a_AvrLon_xa(xa_data)          ! ����ʿ��
      !
      ! 1 ����(X)�ʻ����ǡ����η���(X)����ʿ��(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xa_data(0:,:)
      !(in)  1 ��������(X)�ʻ����ǡ������¤�(0:im-1,*)

      real(8)             :: a_AvrLon_xa(size(xa_data,2))
      !(out) ʿ���ͤ��¤�(*)

      a_AvrLon_xa = a_IntLon_xa(xa_data)/sum(x_Lon_Weight)

    end function a_AvrLon_xa

  !--------------- ���ͥ륮�����ڥ��ȥ�׻� -----------------
    function nma_EnergyFromStreamfunc_wa(wa_Strfunc)
      ! 
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ������饨�ͥ륮���ε���Ĵ��ȡ����ʬ
      ! (���ڥ��ȥ�)��׻�����(¿����).
      !
      !  * ���ȿ� n, �Ӿ��ȿ� m ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ����
      !    ���ͥ륮�����ڥ��ȥ�� (1/2)n(n+1)��(n,m)^2 �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�4�Ф򤫤�����Τ����̾�Ǥ�
      !    �����ͥ륮����������.
      !
      !  * �ǡ�����¸�ߤ��ʤ����ȿ� n, �Ӿ��ȿ� m ������ˤϷ�»�ͤ���Ǽ�����.
      !    ��»�ͤ��ͤϥ⥸�塼���ѿ� wa_spectrum_VMiss �ˤ�ä�����Ǥ���
      !    (����ͤ� -999.0)
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm,-nm:nm,size(wa_Strfunc,2)) &
           :: nma_EnergyFromStreamfunc_wa
      !(out) ���ͥ륮�����ڥ��ȥ�(��ʿ���ȿ� n, �Ӿ��ȿ� m ����)

      integer :: n, m

      nma_EnergyFromStreamfunc_wa = wa_spectrum_VMiss

      do n=0,nm
         do m=1,n
            nma_EnergyFromStreamfunc_wa(n,m,:)  = 0.0D0
            nma_EnergyFromStreamfunc_wa(n,-m,:) = 0.0D0
         enddo
         nma_EnergyFromStreamfunc_wa(n,0,:) &
              = 0.5 * n*(n+1) * wa_Strfunc(l_nm(n,0),:)**2
      enddo

    end function nma_EnergyFromStreamfunc_wa

    function na_EnergyFromStreamfunc_wa(wa_Strfunc)
      !
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ�����������ȿ��Υ��ͥ륮����ʬ(���ڥ��ȥ�)��
      ! �׻�����(¿����).
      !
      !  * ���ȿ� n ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ����
      !    ���ͥ륮�����ڥ��ȥ�Ϧ�[m=-nm]^nm(1/2)n(n+1)��(n,m)^2 
      !    �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤� 4�Ф򤫤�����Τ�
      !    ���̾�Ǥ������ͥ륮����������.
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm,size(wa_Strfunc,2)) &
           :: na_EnergyFromStreamfunc_wa
      !(out) ���ͥ륮�����ڥ��ȥ� (��ʿ���ȿ� n ����) 

      integer :: n

      do n=0,nm
         na_EnergyFromStreamfunc_wa(n,:)  &
              = 0.5 * n*(n+1) * wa_StrFunc(l_nm(n,0),:)**2
      enddo

    end function na_EnergyFromStreamfunc_wa

  !--------------- ���󥹥ȥ�ե������ڥ��ȥ�׻� -----------------

    function nma_EnstrophyFromStreamfunc_wa(wa_Strfunc)  ! 
      !
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ������饨�󥹥ȥ�ե����ε���Ĵ��ȡ����ʬ
      ! (���ڥ��ȥ�)��׻�����(¿����). 
      !
      ! * ���ȿ� n, �Ӿ��ȿ� m ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ����
      !    ���󥹥ȥ�ե������ڥ��ȥ�� (1/2)n^2(n+1)^2��(n,m)^2 �ȷ׻������.
      !
      ! * ���ƤΥ��󥹥ȥ�ե������ڥ��ȥ���ʬ���¤�4��/R^2�򤫤�����Τ�
      !   ���̾�Ǥ������󥹥ȥ�ե�����������. ������ R �ϵ��̤�Ⱦ�¤Ǥ���.
      !
      ! * �ǡ�����¸�ߤ��ʤ����ȿ� n, �Ӿ��ȿ� m ������ˤϷ�»�ͤ���Ǽ�����.
      !   ��»�ͤ��ͤϥ⥸�塼���ѿ� wa_spectrum_VMiss �ˤ�ä�����Ǥ���
      !   (����ͤ� -999.0)
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm,-nm:nm,size(wa_Strfunc,2)) &
           :: nma_EnstrophyFromStreamfunc_wa
      ! ���󥹥ȥ�ե������ڥ��ȥ� (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)

      integer :: n, m

      nma_EnstrophyFromStreamfunc_wa = wa_spectrum_VMiss

      do n=0,nm
         do m=1,n
            nma_EnstrophyFromStreamfunc_wa(n,m,:) = 0.0
            nma_EnstrophyFromStreamfunc_wa(n,-m,:) = 0.0
         enddo
         nma_EnstrophyFromStreamfunc_wa(n,0,:) &
                 = 0.5 * n**2 * (n+1)**2 * wa_Strfunc(l_nm(n,0),:)**2
      enddo

    end function nma_EnstrophyFromStreamfunc_wa

    function na_EnstrophyFromStreamfunc_wa(wa_Strfunc)  ! 
      !
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ�����������ȿ��Υ��ͥ륮����ʬ(���ڥ��ȥ�)��
      ! �׻�����(¿����)
      !
      ! * ���ȿ� n ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ���饨�󥹥ȥ�ե���
      !   ���ڥ��ȥ�Ϧ�[m=-nm]^nm(1/2)n^2(n+1)^2��(n,m)^2 �ȷ׻������.
      !    
      ! * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤� 4��/R^2 �򤫤�����Τ�
      !   ���̾�Ǥ������󥹥ȥե�����������.
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm,size(wa_Strfunc,2)) &
           :: na_EnstrophyFromStreamfunc_wa      ! ���󥹥ȥ�ե������ڥ��ȥ�
      !(out) ���󥹥ȥ�ե������ڥ��ȥ�(��ʿ���ȿ� n ����)

      integer :: n

      do n=0,nm
         na_EnstrophyFromStreamfunc_wa(n,:)  &
              = 0.5 * n**2 * (n+1)**2 * wa_StrFunc(l_nm(n,0),:)**2
      enddo

    end function na_EnstrophyFromStreamfunc_wa

end module wa_zonal_module
