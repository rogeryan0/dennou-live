!--
!----------------------------------------------------------------------
! Copyright (c) 2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  w_module_sjpack
!
!   spml/w_module_sjpack �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
!   ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿���
!   Fortran90 �ؿ����󶡤���. 
!
!   w_module_sjpack �ϼºݤˤϴ����Ѵ�, ��ʬ�׻�, ��ʬ��ʿ�ѷ׻�, ���ڥ��ȥ����
!   �򤽤줾��ô�äƤ��벼���⥸�塼�� w_base_module_sjpack, w_deriv_module_sjpack, 
!   w_integral_module_sjpack, w_spectrum_module_sjpack ����ʤäƤ���.
!
!   ������ ISPACK �� SJPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���.
!   ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!   �Ĥ��Ƥ� ISPACK/SJPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!   �ؿ�, ���֥롼�����̾���ȵ�ǽ�� w_module �Τ�Τ�Ʊ���Ǥ���. 
!   �������ä� use ʸ�� w_module ���� w_module_sjpack ��
!   �ѹ���������� SJPACK �ε�ǽ���Ȥ���褦�ˤʤ�. 
! 
!   ������ l_nm, nm_l �λȤ����ˤ���դ��줿��. w_module �� l_nm, nm_l ��
!   w_Initial �ǽ�������ʤ��Ȥ��Ѥ��뤳�Ȥ��Ǥ���(��̤������ȿ��˰ͤ�ʤ�)��,
!   w_module_sjpack �Τ�ΤϽ���������Τ��ˤ����Ȥ����Ȥ��Ǥ��ʤ�. 
!
!����  2009/09/04  �ݹ�����  w_module ��� SJPACK �Ѥ˲�¤
!      2009/09/20  �ݹ�����  ���Ѿ����դ��ɲ�
!
!++
module w_module_sjpack
  !
  != w_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_module_sjpack.f90,v 1.3 2010-01-26 12:34:07 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/w_module_sjpack �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿���
  ! Fortran90 �ؿ�����n����. 
  !
  ! w_module �ϼºݤˤϴ����Ѵ�, ��ʬ�׻�, ��ʬ��ʿ�ѷ׻�, ���ڥ��ȥ����
  ! �򤽤줾��ô�äƤ��벼���⥸�塼�� w_base_module_sjpack, w_deriv_module_sjpack, 
  ! w_integral_module_sjpack, w_spectrum_module_sjpack ����ʤäƤ���.
  !
  ! ������ ISPACK �� SJPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���.
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  ! �Ĥ��Ƥ� ISPACK/SJPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  ! �ؿ�, ���֥롼�����̾���ȵ�ǽ�� w_module ��Ʊ���Ǥ���. 
  ! �������ä� use ʸ�� w_module ���� w_module_sjpack ��
  ! �ѹ���������� SJPACK �ε�ǽ���Ȥ���褦�ˤʤ�. 
  ! 
  ! ������ l_nm, nm_l �λȤ����ˤ���դ��줿��. w_module �� l_nm, nm_l ��
  ! w_Initial �ǽ�������ʤ��Ȥ��Ѥ��뤳�Ȥ��Ǥ���(��̤������ȿ��˰ͤ�ʤ�)��,
  ! w_module_sjpack �Τ�ΤϽ���������Τ��ˤ����Ȥ����Ȥ��Ǥ��ʤ�. 
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (w_, nm_, n_, xy_, x_, y_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   w_  :: ���ڥ��ȥ�ǡ���
  !   xy_ :: 2 �����ʻ����ǡ���
  !   nm_ :: ���ڥ��ȥ�ǡ������¤�� 3 ��������(���ڥ��ȥ�ǡ������¤Ӥ�
  !          ���ȿ� n, �Ӿ��ȿ� m �ǻ��ꤵ��� 2 ��������)
  !   n_  :: ���ڥ��ȥ�ǡ������¤�� 2 �������� (���ڥ��ȥ�ǡ������¤Ӥ�
  !          ���ȿ� n �ǻ��ꤵ��� 1 ��������)
  !   x_  :: �������� 1 �����ʻ����ǡ���
  !   y_  :: �������� 1 �����ʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(DLon, GradLat, GradLat, DivLon, DivLat, Lapla, 
  !   LaplaInv, Jacobian)��, ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_w_w, _w, _xy, _x, _y) ��, �����ѿ��η����ڥ��ȥ�ǡ���
  !   ����ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _w   :: ���ڥ��ȥ�ǡ���
  !   _w_w :: 2 �ĤΥ��ڥ��ȥ�ǡ���
  !   _xy  :: 2 �����ʻ����ǡ���
  !   _x   :: �������� 1 �����ʻ����ǡ���
  !   _y   :: �������� 1 �����ʻ����ǡ���
  !
  !=== �ƥǡ����μ��������
  !
  ! * xy : 2 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:im-1,1:jm). 
  !   * im, jm �Ϥ��줾�����, ���ٺ�ɸ�γʻ������Ǥ���, ���֥롼����
  !     w_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * w : ���ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension((nm+1)*(nm+1)). 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, ���֥롼���� w_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���. 
  !   * ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ϴؿ� l_nm, nm_l �ˤ�ä�
  !     Ĵ�٤뤳�Ȥ��Ǥ���.
  !
  ! * nm : ���ڥ��ȥ�ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,-nm:nm). 
  !     �� 1 ��������ʿ���ȿ�,  �� 2 �������Ӿ��ȿ���ɽ��. 
  !   * nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, ���֥롼���� w_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���.
  !
  ! * n : ���ڥ��ȥ�ǡ������¤�� 1 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm). 
  !   * �� 1 ��������ʿ���ȿ���ɽ��. nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, 
  !     ���֥롼���� w_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * x, y : ����, �������� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����Ϥ��줾�� real(8), dimension(0:im-1) 
  !     ����� real(8), dimension(1:jm).
  !
  ! * w_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * xy_ �ǻϤޤ�ؿ����֤��ͤ� 2 �����ʻ����ǡ�����Ʊ��.
  !
  ! * x_, y_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  !
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! w_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
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
  ! xy_w :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! w_xy :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  ! l_nm, nm_l :: ���ڥ��ȥ�ǡ����γ�Ǽ���֤����ȿ����Ӿ��ȿ����Ѵ� 
  !
  !==== ��ʬ
  !
  ! w_Lapla_w       :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! rn              :: ���ڥ��ȥ�ǡ����Υ�ץ饷�����׻����뤿��η���. 
  ! irm             :: ������ʬ�黻������(̤���)
  ! w_LaplaInv_w    :: ���ڥ��ȥ�ǡ����˥�ץ饷����ε��Ѵ�����Ѥ�����
  ! w_DLon_w        :: ���ڥ��ȥ�ǡ����˷�����ʬ��/�ߦˤ���Ѥ�����
  ! xy_GradLon_w    :: ���ڥ��ȥ�ǡ�����
  !                    ���۷�������ʬ 1/cos�ա���/�ߦˤ���Ѥ�����
  ! xy_GradLat_w    :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ��/�ߦդ���Ѥ�����
  ! w_DivLon_xy     :: �ʻҥǡ�����ȯ����������ʬ 1/cos�ա���/�ߦˤ���Ѥ�����
  ! w_DivLat_xy     :: �ʻҥǡ�����
  !                    ȯ����������ʬ 1/cos�ա���(g cos��)/�ߦդ���Ѥ�����
  ! w_Div_xy_xy     :: �٥��ȥ���ʬ�Ǥ��� 2 �Ĥγʻҥǡ�����ȯ������Ѥ�����
  ! w_Jacobian_w_w  :: 2 �ĤΥ��ڥ��ȥ�ǡ�������䥳�ӥ����׻�����
  !
  !
  !==== ��ʬ(��,��=sin�� ��ɸ)
  !
  ! xy_GradLambda_w :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ��/�ߦˤ���Ѥ�����
  ! xy_GradMu_w     :: ���ڥ��ȥ�ǡ�����
  !                    ���۷�������ʬ (1-��^2)��/�ߦ̤���Ѥ�����
  ! w_DivLambda_xy  :: �ʻҥǡ�����
  !                    ȯ����������ʬ 1/(1-��^2)����/�ߦˤ���Ѥ�����
  ! w_DivMu_xy      :: �ʻҥǡ�����ȯ����������ʬ��/�ߦ̤���Ѥ�����
  !
  !==== ���
  !
  ! Interpolate_w :: ���ڥ��ȥ�ǡ�������Ǥ�դ����Ǥ��ͤ����. 
  !
  !==== ��ʬ��ʿ��
  !
  ! IntLonLat_xy, AvrLonLat_xy :: 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��
  ! y_IntLon_xy, y_AvrLon_xy   :: 2 �����ʻ����ǡ����η���������ʬ�����ʿ��
  ! IntLon_x, AvrLon_x         :: 1 ����(X)�ʻ����ǡ����η���������ʬ�����ʿ��
  ! x_IntLat_xy, x_AvrLat_xy   :: 2 �����ʻ����ǡ����ΰ���������ʬ�����ʿ��
  ! IntLat_y, AvrLat_y         :: 1 ����(Y)�ʻ����ǡ����ΰ���������ʬ�����ʿ��
  !
  !==== ���ڥ��ȥ����
  !
  ! nm_EnergyFromStreamfunc_w  :: ή���ؿ����饨�ͥ륮�����ڥ��ȥ��׻�����
  !                               (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  ! n_EnergyFromStreamfunc_w   :: ή���ؿ����饨�ͥ륮�����ڥ��ȥ��׻�����
  !                               (��ʿ���ȿ� n ����) 
  ! nm_EnstrophyFromStreamfunc_w  :: ή���ؿ����饨�󥹥ȥ�ե������ڥ��ȥ��
  !                                  �׻����� (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  ! n_EnstrophyFromStreamfunc_w   :: ή���ؿ����饨�󥹥ȥ�ե������ڥ��ȥ��
  !                                  �׻����� (��ʿ���ȿ� n ����)
  ! w_spectrum_VMiss              ::  ��»��
  !
  !
  use w_base_module_sjpack
  use w_deriv_module_sjpack
  use w_integral_module_sjpack
  use w_spectrum_module_sjpack
  use w_interpolate_module_sjpack

  private

  public w_Initial                            ! �����

  public x_Lon, y_Lat                         ! �ʻҺ�ɸ
  public x_Lon_weight, y_Lat_Weight           ! �ʻҺ�ɸ�Ť�
  public xy_Lon, xy_Lat                       ! �ʻҺ�ɸ(im,jm)
  public xy_w, w_xy, l_nm, nm_l               ! �Ѵ��ؿ�

  public w_Lapla_w, w_LaplaInv_w              ! ��ץ饷����ȵձ黻
  public rn                                   ! ��ץ饷����黻������
  public w_DLon_w                             ! ������ʬ
  public xy_GradLon_w, xy_GradLat_w           ! ���۷���ʬ
  public w_DivLon_xy, w_DivLat_xy             ! ȯ������ʬ
  public w_Div_xy_xy                          ! ȯ������ʬ
  public w_Jacobian_w_w                       ! �䥳�ӥ���
  public xy_GradLambda_w, xy_GradMu_w         ! ���۷���ʬ(��,�̺�ɸ)
  public w_DivLambda_xy, w_DivMu_xy           ! ȯ������ʬ(��,�̺�ɸ)

  public Interpolate_w                        ! ��ִؿ�

  public IntLonLat_xy                         ! ���ٷ�����ʬ
  public y_IntLon_xy, IntLon_x                ! ������ʬ    
  public x_IntLat_xy, IntLat_y                ! ������ʬ    
  public AvrLonLat_xy                         ! ���ٷ���ʿ��
  public y_AvrLon_xy, AvrLon_x                ! ����ʿ��    
  public x_AvrLat_xy, AvrLat_y                ! ����ʿ��    

  public nm_EnergyFromStreamfunc_w            ! ���ͥ륮�����ڥ��ȥ�           
                                              ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public n_EnergyFromStreamfunc_w             ! ���ͥ륮�����ڥ��ȥ�
                                              ! (��ʿ���ȿ� n ����) 
  public nm_EnstrophyFromStreamfunc_w         ! ���󥹥ȥ�ե������ڥ��ȥ�     
                                              ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public n_EnstrophyFromStreamfunc_w          ! ���󥹥ȥ�ե������ڥ��ȥ�  
                                              !  (��ʿ���ȿ� n ����)
  public w_spectrum_VMiss                     ! ��»��

contains

  !--------------- ����� -----------------
    subroutine w_initial(n_in,i_in,j_in,np_in)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ������ OPENMP ���ѻ���
      ! ���祹��åɿ������ꤹ��.
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
      integer,intent(in) :: i_in              !(in) �ʻ�����(����)
      integer,intent(in) :: j_in              !(in) �ʻ�����(����)
      integer,intent(in) :: n_in              !(in) �����ȿ�������
      integer,intent(in), optional :: np_in   !(in) OPENMP �Ǥκ��祹��åɿ�

      if ( present (np_in) )then
         call w_base_initial(n_in,i_in,j_in,np_in)
      else
         call w_base_initial(n_in,i_in,j_in)
      endif

      call w_deriv_initial

    end subroutine w_initial

end module w_module_sjpack
