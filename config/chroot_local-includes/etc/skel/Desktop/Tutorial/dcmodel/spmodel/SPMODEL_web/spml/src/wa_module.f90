!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wa_module
!
!   spml/wa_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
!   ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿���
!   Fortran90 �ؿ����󶡤���. 
!
!   ���̾�� 1 �إ�ǥ��� w_module �⥸�塼���¿�إ�ǥ��Ѥ�
!   ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
!   �Ф����Ѵ����Ԥ���.
!
!   wa_module �ϼºݤˤϴ����Ѵ�, ��ʬ�׻�, ��ʬ��ʿ�ѷ׻�, ���ڥ��ȥ����
!   �򤽤줾��ô�äƤ��벼���⥸�塼�� wa_base_module, wa_deriv_module, 
!   wa_integral_module, wa_spectrum_module ����� 1 ���ѤΥ⥸�塼��
!   w_module ����ʤäƤ���.
!
!   ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���.
!   ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!   �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!
!����  2002/02/02  �ݹ�����  ¿����
!      2002/02/07  �ݹ�����  �ؿ�,�ѿ�̾�����ѹ�
!      2002/03/25  �ݹ�����  �⥸�塼��̾�ѹ�
!      2002/05/25  �ݹ�����  �ʻ�����ɸ����̤��٤�̿̾ˡ�ѹ�
!      2002/10/07  �ݹ�����  ��, �̺�ɸ������ʬ�ɲ�
!      2005/04/23  �ݹ�����  ���ڥ��ȥ���ϥ⥸�塼���ɲ�
!      2005/07/04  �ݹ�����  OPENMP ���Ѵ��롼������б�
!      2006/03/08  �ݹ�����  �����Ȥ� RDoc �Ѥ˽���
!      2006/03/19  �ݹ�����  �ѿ�����³����������򥳥��Ȥ��ɲ�
!      2007/05/01  ��������ʿ rn, irm �Ѥ� public ������ɲ�
!      2007/11/02  �ݹ�����  ��ַ׻��롼�����ɲ�
!      2007/11/21  �ݹ�����  ��������֥롼�����å���������
!      2008/05/31  �ݹ�����  ��������֥롼�����ѹ�
!      2008/06/28  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2008/12/29  �ݹ�����  ɽ�ꥳ���Ƚ���
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!
!++
module wa_module
  !
  != wa_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_module.f90,v 1.22 2009-07-30 12:50:23 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/wa_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿���
  ! Fortran90 �ؿ����󶡤���. 
  !
  ! ���̾�� 1 �إ�ǥ��� w_module �⥸�塼���¿�إ�ǥ��Ѥ�
  ! ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
  ! �Ф����Ѵ����Ԥ���.
  !
  ! wa_module �ϼºݤˤϴ����Ѵ�, ��ʬ�׻�, ��ʬ��ʿ�ѷ׻�, ���ڥ��ȥ����
  ! �򤽤줾��ô�äƤ��벼���⥸�塼�� wa_base_module, wa_deriv_module, 
  ! wa_integral_module, wa_spectrum_module ����� 1 ���ѤΥ⥸�塼��
  ! w_module ����ʤäƤ���.
  !
  ! ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���.
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  ! �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  ! w_module �ǤΤ�Τ��ɲä����ؿ����ѿ���̾���ȷ��ˤĤ��ƤΤ���������. 
  ! w_module �Ǥδؿ����ѿ���̾���ȷ��ˤĤ��Ƥ� w_module �γ�����򻲾�
  ! ���줿��.
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (wa_, nma_, na_, xya_, xa_, ya_, w_, xy_, x_, y_, a_) ��, 
  !   �֤��ͤη��򼨤��Ƥ���.
  !   wa_  :: ��ʿ���ڥ��ȥ�ǡ������¤�� 2 ��������(���ڥ��ȥ�ǡ�����
  !           �¤Ӥ� SNPACK/ISPACK �˽��ä����)
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
  !   * �ѿ��μ���ȼ����� real(8), dimension((nm+1)*(nm+1),:). 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, ���֥롼���� wa_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���. 
  !   * ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ϴؿ� 
  !     l_nm, nm_l �ˤ�ä�Ĵ�٤뤳�Ȥ��Ǥ���. 
  !   * ������ 3 �������礭���κ����ͤ� wa_Initial �����ꤷ�Ƥ���.
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
  !   *  �� 1 ��������ʿ���ȿ���ɽ��.nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, 
  !      ���֥롼���� wa_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
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
  !                                  ���۷�������ʬ ��/�ߦ� ��
  !                                  ���Ѥ�����(¿��, 1 ����)
  ! wa_DivLon_xya, w_DivLon_xy    :: �ʻҥǡ�����
  !                                  ȯ����������ʬ 1/cos�ա���/�ߦˤ�
  !                                  ���Ѥ�����(¿��, 1 ����)
  ! wa_DivLat_xya, w_DivLat_xy    :: �ʻҥǡ�����
  !                                  ȯ����������ʬ 1/cos�ա���(g cos��)/�ߦդ�
  !                                  ���Ѥ����� (¿��, 1 ����)
  ! wa_Div_xya_xya, w_Div_xy_xy   :: �٥��ȥ���ʬ�Ǥ��� 2 �Ĥγʻҥǡ�����
  !                                  ȯ������Ѥ�����(¿��, 1 ����)
  ! wa_Jacobian_wa_wa,w_Jacobian_w_w :: 2�ĤΥ��ڥ��ȥ�ǡ�������䥳�ӥ����
  !                                     �׻�����(¿��, 1 ����)
  !
  !==== ��ʬ(��,��=sin�� ��ɸ)
  !
  ! xya_GradLambda_wa, xy_GradLambda_w  :: ���ڥ��ȥ�ǡ�����
  !                                        ���۷�������ʬ��/�ߦˤ���Ѥ�����
  !                                        (¿��, 1 ����)
  ! xya_GradMu_wa, xy_GradMu_w          :: ���ڥ��ȥ�ǡ�����
  !                                        ���۷�������ʬ(1-��^2)��/�ߦ�
  !                                        ����Ѥ�����
  !                                        (¿��, 1 ����)
  ! wa_DivLambda_xya,w_DivLambda_xy     :: �ʻҥǡ�����ȯ����������ʬ
  !                                        1 /(1-��^2)����/�ߦˤ���Ѥ�����
  !                                        (¿��, 1 ����)
  !
  ! wa_DivMu_xya,w_DivMu_xy             :: �ʻҥǡ�����ȯ����������ʬ
  !                                        ��/�ߦ̤���Ѥ�����(¿��, 1 ����)
  !
  !==== ��ַ׻�
  !
  ! a_Interpolate_wa, Interpolate_w :: ���ڥ��ȥ�ǡ�������
  !                                    Ǥ�դ����δؿ��ͤ�׻�����.
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
  !
  use dc_message, only : MessageNotify
  use w_module
  use wa_base_module
  use wa_deriv_module
  use wa_interpolate_module
  use wa_integral_module
  use wa_spectrum_module

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

contains
  subroutine wa_Initial(n_in,i_in,j_in,k_in,np_in)
    !
    ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, ����ǡ�����(�ؿ�)����� 
    ! OPENMP ���ѻ��κ��祹��åɿ������ꤹ��.
    !
    ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ�ǽ�������
    ! ���ʤ���Фʤ�ʤ�. 
    !
    ! np_in �� 1 ����礭���ͤ���ꤹ��� ISPACK �ε���Ĵ��ȡ���Ѵ� 
    ! OPENMP ����׻��롼�����Ѥ�����. ����׻���¹Ԥ���ˤ�, 
    ! �¹Ի��˴Ķ��ѿ� OMP_NUM_THREADS �� np_in �ʲ��ο��������ꤹ������
    ! �����ƥ�˱�����������ɬ�פȤʤ�. 
    !
    ! np_in �� 1 ����礭���ͤ���ꤷ�ʤ��������׻��롼����ϸƤФ�ʤ�.
    !
    integer,intent(in) :: i_in                ! �ʻ�����(����)
    integer,intent(in) :: j_in                ! �ʻ�����(����)
    integer,intent(in) :: n_in                ! �����ȿ�
    integer,intent(in) :: k_in                ! ����ǡ�����(�ؿ�)
    integer,intent(in), optional :: np_in     ! OPENMP �Ǥκ��祹��åɿ�

    if ( present(np_in) ) then
       call w_Initial(n_in,i_in,j_in,np_in)
    else
       call w_Initial(n_in,i_in,j_in)
    endif
    call wa_base_Initial(k_in)

    call MessageNotify('M','wa_initial','wa_module is initialized')
  end subroutine wa_Initial

end module wa_module
