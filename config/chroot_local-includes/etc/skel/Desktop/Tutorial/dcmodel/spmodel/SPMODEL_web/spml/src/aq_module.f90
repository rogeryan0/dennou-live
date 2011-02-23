!--
!----------------------------------------------------------------------
! Copyright(c) 2008-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  aq_module
!
!      spml/aq_module �⥸�塼��� 1 ����ͭ���ΰ�β��Ǥ�ή�α�ư��
!      Matsushima and Marcus (1994) ���󾧤��줿¿�༰���Ѥ���
!      ���ڥ��ȥ���ͷ׻������ Fortran90 �ؿ����󶡤���. 
!      ���Υ롼�����Υ�����˥����ӥ�����--������--������ʻ�����Ŭ��
!      ���Ƥ���, ��� 2 �����˺�ɸ,  ������ɸ, ���ɸ�θ�����
!      �ð�������򤷤ʤ��饹�ڥ��ȥ�׻���Ԥ�������Ѥ��뤳�Ȥ�
!      ǰƬ�ˤ����Ƥ���. 
!
!      2 �����ǡ����� 1 �����˴ؤ���Ʊ���˥��ڥ��ȥ�׻���¹Ԥ��뤿���
!      �ؿ����󶡤��Ƥ���, 2, 3 �����ΰ�Ǥη׻��Υ١������󶡤���. 
!
!      Matsushima and Marcus (1994) ��¿�༰�˴ؤ���������
!      doc/spectral_radial.tex �򻲾ȤΤ���. 
!
!����  2008/03/26  �ݹ�����  ��������
!      2008/05/02  �ݹ�����  �������ɲ�
!      2008/07/07  ��������ʿ �����������, �����Ȥ� RDoc �Ѥ�������
!      2008/07/20  �ݹ�����  aq_r2_aq, q_r2_q, aq_r2Inv_aq, q_r2Inv_q �ɲ�
!                            ag_aq �������·׻�����
!      2008/10/28  �ݹ�����  DGamma ����, gammaln ��ľ�ܻ���
!      2009/01/09  �ݹ�����  aq_Initial ��å����������դ��ɲ�
!      2009/01/23  ��������ʿ rdoc �Ѥ˥ɥ�����Ȥ�����
!      2009/07/31  �ݹ�����  ������׻�������� threadprivate ����(OpenMP)
!      2009/12/06  �ݹ�����  threadprivate �����ȥ�����
!      2010/03/10  ��������ʿ  threadprivate ���(����ѥ����¸)
!
!++
module aq_module
  !
  != aq_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: aq_module.f90,v 1.13 2010-03-02 10:09:01 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/aq_module �⥸�塼��� 1 ����ͭ���ΰ�β��Ǥ�ή�α�ư��
  ! Matsushima and Marcus (1994) ���󾧤��줿¿�༰���Ѥ������ڥ��ȥ�ˡ�ˤ�ä�
  ! ���ͷ׻����뤿��� Fortran90 �ؿ����󶡤���.
  !
  ! ���Υ롼�����Υ�����˥����ӥ�����--������--������ʻ�����Ŭ�Ѥ���
  ! ����, ��� 2 �����˺�ɸ, ������ɸ, ���ɸ�θ������ð�������򤷤�
  ! ���饹�ڥ��ȥ�׻���Ԥ�������Ѥ��뤳�Ȥ�ǰƬ�ˤ����Ƥ���.
  !
  ! 2 �����ǡ����� 1 �����˴ؤ���Ʊ���˥��ڥ��ȥ�׻���¹Ԥ��뤿���
  ! �ؿ����󶡤��Ƥ���, 2, 3 �����ΰ�Ǥη׻��Υ١������󶡤���.
  !
  ! Matsushima and Marcus (1994) ��¿�༰�˴ؤ���������
  ! ư�º�ɸ�Υ��ڥ��ȥ�ˡ(spectral_radial.pdf[link:spectral_radial.pdf]) 
  ! �򻲾ȤΤ���.
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (u_, g_, aq_, ag_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   ʣ���¤�� 2 �����ǡ������� 1 �������礭���� aq_Initial ��
  !   ���ꤹ��Ťߤλؿ������� nd ���礭����Ʊ���Ǥʤ���Фʤ�ʤ�.
  !   q_  :: ���ڥ��ȥ�ǡ���
  !   g_  :: 1 �����ʻ����ǡ���
  !   aq_ :: 1 �������ڥ��ȥ�ǡ�����ʣ���¤�� 2 �����ǡ���
  !   ag_ :: 1 �����ʻ����ǡ�����ʣ���¤�� 2 �����ǡ���.
  !
  ! * �ؿ�̾�δ֤�ʸ����(Dr)��, ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_e, _aq, _g, _ag) ��, �����ѿ��η������ڥ��ȥ�ǡ���
  !   ����ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _q  :: ���ڥ��ȥ�ǡ���
  !   _g  :: 1 �����ʻ����ǡ���
  !   _aq :: 1 �������ڥ��ȥ�ǡ�����ʣ���¤�� 2 �����ǡ���
  !   _ag :: 1 �����ʻ����ǡ�����ʣ���¤�� 2 �����ǡ���
  !
  !=== �ƥǡ����μ��������
  !
  ! * g : 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(im). 
  !   * im �� R ��ɸ�γʻ������Ǥ���, ���֥롼���� aq_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���.
  !
  ! * q : ���ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:km). 
  !   * km �� R �����κ����ȿ��Ǥ���, ���֥롼���� aq_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���. ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ˤĤ��Ƥ�...
  !
  ! * ag : 1 ����(R)�ʻ����ǡ������¤�� 2 �����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(size(nd),im). 
  !     �� 2 ������ R ������ɽ��.
  !
  ! * aq : 1 �������ڥ��ȥ�ǡ������¤�� 2 �����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(size(nd),0:km). 
  !     �� 2 ���������ڥ��ȥ��ɽ��.
  !
  ! * g_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ�����Ʊ��.
  !
  ! * q_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * ag_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ������¤�� 
  !   2 �����ǡ�����Ʊ��.
  !
  ! * aq_ �ǻϤޤ�ؿ����֤��ͤ� 1 �������ڥ��ȥ�ǡ������¤��
  !   2 �����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������ΤΤ��ȤǤ���.
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! aq_Initial  :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! g_R        :: �ʻ�����ɸ(R)���Ǽ���� 1 ��������
  ! g_R_Weight :: �Ťߺ�ɸ���Ǽ���� 1 ��������
  !
  !==== �����Ѵ�
  !
  ! g_q, ag_aq :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! q_g, aq_ag :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  !
  !==== ��ʬ
  !
  ! q_rDr_q, aq_rDr_aq :: ���ڥ��ȥ�ǡ����� r(d/dR) ��ʬ����Ѥ�����
  !
  !==== ��
  !
  ! q_r2_q,    aq_r2_aq    :: ���ڥ��ȥ�ǡ����� r^2 �򤫤���
  ! q_r2Inv_q, aq_r2Inv_aq :: ���ڥ��ȥ�ǡ����� r^-2 �򤫤���
  !
  !==== ��ʬ��ʿ��
  !
  ! a_Int_ag, a_Avr_ag :: 1 �����ʻ����ǡ������¤�� 2 �����������ʬ�����ʿ��
  ! Int_g, Avr_g       :: 1 �����ʻ����ǡ�������ʬ�����ʿ��
  !
  !==== ����������
  !
  ! aq_Boundary_D, aq_Boundary_N         :: ��¦�ǥ��ꥯ����, �Υ��ޥ���
  ! aq_BoundaryTau_D, aq_BoundaryTau_N   :: ��¦�ǥ��ꥯ����, 
  !                                         �Υ��ޥ���(����ˡ)
  ! ag_BoundaryGrid_D, ag_BoundaryGrid_N :: ��¦�ǥ��ꥯ����, 
  !                                         �Υ��ޥ���(����ˡ)
  !
  use dc_message
  use lumatrix
  implicit none

  private

  public  aq_initial                           ! �����  
  public  g_R, g_R_Weight                      ! ��ɸ�ѿ�
                                        
  public  ag_aq, aq_ag, g_q, q_g               ! �����Ѵ�
                                        
  public  aq_rDr_aq, q_rDr_q                   ! ��ʬ    
  public  aq_r2_aq, q_r2_q                     ! �� 
  public  aq_r2Inv_aq, q_r2Inv_q               ! �� 

  public  a_Int_ag, a_Avr_ag                   ! ��ʬ��ʿ��
  public  Int_g, Avr_g                         ! ��ʬ��ʿ��

  public  aq_Boundary_D, aq_Boundary_N         ! �������
  public  aq_BoundaryTau_D, aq_BoundaryTau_N   ! �������
  public  ag_BoundaryGrid_D, ag_BoundaryGrid_N ! �������

  interface aq_Boundary_D
     !
     ! ��¦�����ǥ��ꥯ�췿��������Ŭ��(����ˡ). 
     !
     ! * ��������Ŭ�Ѥ�������μ����ˤ�ä������ǥ��֥롼�����
     !   �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !   �����롼�����Ƥ�ɬ�פϤʤ�.
     !
     ! �����ȷ�̤η�
     !
     ! * 1 �������ڥ��ȥ�ǡ������¤�� 2 ��������ξ��
     !
     !   real(8), dimension(size(md),0:km),intent(inout)   :: aq_data
     !   !(inout) ��������Ŭ�Ѥ��륹�ڥ��ȥ�ǡ���
     !  
     !   real(8), dimension(size(md)), intent(in), optional :: value
     !   !(in) Ŭ�Ѥ��붭����
     !
     ! * 1 ���������ӥ����եǡ����ξ��
     !
     !   real(8), dimension(0:km),intent(inout)       :: q_data
     !   !(inout) ��������Ŭ�Ѥ��륹�ڥ��ȥ�ǡ���
     !  
     !   real(8), intent(in), optional                :: value
     !   !(in) Ŭ�Ѥ��붭����
     !
     module procedure aq_BoundaryTau_D_1d, aq_BoundaryTau_D_2d
  end interface

  interface aq_Boundary_N
     !
     ! ��¦�����Υ��ޥ󷿶�������Ŭ��(����ˡ). 
     !
     ! * ��������Ŭ�Ѥ�������μ����ˤ�ä������ǥ��֥롼�����
     !   �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !   �����롼�����Ƥ�ɬ�פϤʤ�.
     !
     ! �����ȷ�̤η�
     !
     ! * 1 �������ڥ��ȥ�ǡ������¤�� 2 ��������ξ��
     !
     !   real(8), dimension(size(md),0:km),intent(inout)    :: aq_data
     !   !(inout) ��������Ŭ�Ѥ��륹�ڥ��ȥ�ǡ���
     !   
     !   real(8), dimension(size(md)), intent(in), optional :: value
     !   !(in) Ŭ�Ѥ��붭����
     !
     ! * 1 ���������ӥ����եǡ����ξ��
     !
     !   real(8), dimension(0:km),intent(inout)       :: q_data
     !   !(inout) ��������Ŭ�Ѥ��륹�ڥ��ȥ�ǡ���
     !   
     !   real(8), intent(in), optional                :: value
     !   !(in) Ŭ�Ѥ��붭����
     !
     module procedure aq_BoundaryTau_N_1d, aq_BoundaryTau_N_2d
  end interface

  interface aq_BoundaryTau_D
     !
     ! ��¦�����ǥ��ꥯ�췿��������Ŭ��(����ˡ). 
     !
     ! * ��������Ŭ�Ѥ�������μ����ˤ�ä������ǥ��֥롼�����
     !   �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !   �����롼�����Ƥ�ɬ�פϤʤ�.
     !
     ! �����ȷ�̤η�
     !
     ! * 1 �������ڥ��ȥ�ǡ������¤�� 2 ��������ξ��
     !
     !   real(8), dimension(size(md),0:km),intent(inout)    :: aq_data
     !   !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !  
     !   real(8), dimension(size(md)), intent(in), optional :: value
     !   !(in) Ŭ�Ѥ��붭����
     !
     ! * 1 �������ڥ��ȥ�ǡ����ξ��
     !
     !   real(8), dimension(0:km),intent(inout)       :: q_data
     !   !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !  
     !   real(8), intent(in), optional                :: value
     !   !(in) Ŭ�Ѥ��붭����
     !
     module procedure aq_BoundaryTau_D_1d, aq_BoundaryTau_D_2d
  end interface

  interface aq_BoundaryTau_N
     !
     ! ��¦�����Υ��ޥ󷿶�������Ŭ��(����ˡ). 
     !
     ! * ��������Ŭ�Ѥ�������μ����ˤ�ä������ǥ��֥롼�����
     !   �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !   �����롼�����Ƥ�ɬ�פϤʤ�.
     !
     ! �����ȷ�̤η�
     !
     ! * 1 �������ڥ��ȥ�ǡ������¤�� 2 ��������ξ��
     !
     !   real(8), dimension(size(md),0:km),intent(inout)    :: aq_data
     !   !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !   
     !   real(8), dimension(size(md)), intent(in), optional :: value
     !   !(in) Ŭ�Ѥ��붭����
     !
     ! * 1 �������ڥ��ȥ�ǡ����ξ��
     !
     !   real(8), dimension(0:km),intent(inout)       :: q_data
     !   !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !   
     !   real(8), intent(in), optional                :: value
     !   !(in) Ŭ�Ѥ��붭����
     !
     module procedure aq_BoundaryTau_N_1d, aq_BoundaryTau_N_2d
  end interface

  interface ag_BoundaryGrid_D
     !
     ! ��¦�����ǥ��ꥯ�췿��������Ŭ��(����ˡ). 
     !
     ! * ��������Ŭ�Ѥ�������μ����ˤ�ä������ǥ��֥롼�����
     !   �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !   �����롼�����Ƥ�ɬ�פϤʤ�.
     !
     ! �����ȷ�̤η�
     !
     ! * 1 �������ڥ��ȥ�ǡ������¤�� 2 ��������ξ��
     !
     !   real(8), dimension(size(md),0:km),intent(inout)    :: aq_data
     !   !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !   
     !   real(8), dimension(size(md)), intent(in), optional :: value
     !   !(in) Ŭ�Ѥ��붭����
     !
     ! * 1 �������ڥ��ȥ�ǡ����ξ��
     !
     !   real(8), dimension(0:km),intent(inout)       :: q_data
     !   !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !   
     !   real(8), intent(in), optional                :: value
     !   !(in) Ŭ�Ѥ��붭����
     !
     module procedure ag_BoundaryGrid_D_1d, ag_BoundaryGrid_D_2d
  end interface

  interface ag_BoundaryGrid_N
     !
     ! ��¦�����Υ��ޥ󷿶�������Ŭ��(����ˡ). 
     !
     ! * ��������Ŭ�Ѥ�������μ����ˤ�ä������ǥ��֥롼�����
     !   �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !   �����롼�����Ƥ�ɬ�פϤʤ�.
     !
     ! �����ȷ�̤η�
     !
     ! * 1 �������ڥ��ȥ�ǡ������¤�� 2 ��������ξ��
     !
     !   real(8), dimension(size(md),0:km),intent(inout)    :: aq_data
     !   !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !   
     !   real(8), dimension(size(md)), intent(in), optional :: value
     !   !(in) Ŭ�Ѥ��붭����
     !
     ! * 1 �������ڥ��ȥ�ǡ����ξ��
     !
     !   real(8), dimension(0:km),intent(inout)       :: q_data
     !   !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !   
     !   real(8), intent(in), optional                :: value
     !   !(in) Ŭ�Ѥ��붭����
     !
     module procedure ag_BoundaryGrid_N_1d, ag_BoundaryGrid_N_2d
  end interface

  integer :: im, km                       ! �ʻ�����, �����ȿ�
  real(8) :: ra                           ! �ΰ���礭��

  real(8) :: alpha                        ! Ÿ��¿�༰�ѥ�᥿��  0 < �� <= 1
  real(8) :: beta                         ! Ÿ��¿�༰�ѥ�᥿��  0 < ��
  real(8) :: gamma                        ! Ÿ��¿�༰�ѥ�᥿��  ��=2��+��
  integer, allocatable :: md(:)           ! Ÿ��¿�༰���ռ����Τʤ��

  integer :: jmax                         ! a ��ɸ(�ǡ����� 1 ����)���礭��

  real(8), allocatable :: g_R(:)          ! ������--������ʻ���
  real(8), allocatable :: g_R_Weight(:)   ! �������Ť�

  real(8), allocatable :: CF(:,:,:)       ! ���Ѵ��ѹ���
  real(8), allocatable :: CB(:,:,:)       ! ���Ѵ��ѹ���


  logical :: first_r2inv  =.true.
  logical :: first_Tau_D  =.true.
  logical :: first_Tau_N  =.true.
  logical :: first_Grid_N =.true.

  save im, km, ra, alpha, beta, gamma, CF, CB, g_R, g_R_Weight, md, jmax
  save first_r2inv, first_Tau_D, first_Tau_N, first_Grid_N

contains

! --- �����
  subroutine aq_Initial(i_in,k_in,r_in,alpha_in,beta_in,md_in)
    !
    ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��, �Ťߤ����ꤹ��.
    ! 
    ! ¾�δؿ����ѿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ��
    ! �������򤷤ʤ���Фʤ�ʤ�.
    !
    integer,intent(in) :: i_in              !(in) �ʻ�����
    integer,intent(in) :: k_in              !(in) �����ȿ�
    real(8),intent(in) :: r_in              !(in) ��¦�����κ�ɸ(Ⱦ��)
    real(8),intent(in) :: alpha_in          !(in) Ÿ��¿�༰�ѥ�᥿��
    real(8),intent(in) :: beta_in           !(in) Ÿ��¿�༰�ѥ�᥿�� 
    integer,intent(in) :: md_in(:)          !(in) Ÿ��¿�༰���ռ����Τʤ��

    integer i, j, n

   !--- �ѥ�᥿���Υ����å�������
    im=i_in ; km=k_in ; ra = r_in; alpha=alpha_in; beta=beta_in

    if ( km .ge. 2*im ) then
       call MessageNotify('E','aq_initial','KM shoud be less than 2*IM')
    endif

    if ( alpha .le. 0.0D0 ) then
       call MessageNotify('E','aq_initial','alpha must be larger than 0')
    endif
    if ( alpha .gt. 1.0D0 ) then
       call MessageNotify('E','aq_initial','alpha must be smaller equal to 1')
    endif
    if ( beta .le. 0.0D0 ) then
       call MessageNotify('E','aq_initial','beta must be larger than 0')
    endif

    gamma = 2.0D0 * alpha + beta

   !--- ��������η׻�
    jmax = size(md_in)
    if ( allocated(md) ) deallocate(md)
    allocate(md(jmax))

    md = md_in

   !--- �ʻ���, �Ťߤ�����
    if ( allocated(g_R) ) deallocate(g_R)
    if ( allocated(g_R_Weight) ) deallocate(g_R_Weight)
    allocate(g_R(im),g_R_Weight(im))

    call gauss_radau(2*im, g_R, g_R_Weight)
    g_R = ra * g_R
    g_R_Weight = ra**(gamma-1) * g_R_Weight

   !--- �Ѵ����������
    if ( allocated(CF) ) deallocate(CF)
    if ( allocated(CB) ) deallocate(CB)
    allocate(CF(jmax,0:km,im),CB(jmax,im,0:km))

    CF = 0.0D0
    do j=1,jmax
       do n=md(j),km,2
          do i=1,im
             CF(j,n,i) = Phi(g_R(i)/ra,n,md(j)) * g_R_Weight(i)/ra**(gamma-1)
          enddo
       enddo
    enddo

    CB = 0.0D0
    do j=1,jmax
       do i=1,im
          do n=md(j),km,2
             CB(j,i,n) = Phi(g_R(i)/ra,n,md(j)) 
          enddo
       enddo
    enddo

   !--- �ƥ롼�����Ѵ�����ν���������å�

    first_r2inv  = .true.
    first_Tau_D  = .true.
    first_Tau_N  = .true. 
    first_Grid_N = .true.

    call MessageNotify(&
      'M','aq_initial','aq_module (2009/07/31) is initialized')

  end subroutine aq_Initial

! ---- ���Ѵ� ---- 
  function ag_aq(aq_data)
    !
    ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����(2 ����������).
    !
    real(8), dimension(:,0:), intent(in)    :: aq_data
    !(in) ���ڥ��ȥ�ǡ���

    real(8), dimension(size(aq_data,1),im)  :: ag_aq
    !(out) �ʻ����ǡ���

    integer :: i, j

    if ( size(aq_data,1) /= jmax ) then
       call MessageNotify('E','ag_aq', &
            '1st dim. of the spectral data should be same as dim. of MD.')
    end if

    if ( size(aq_data,2)-1 < km ) then
       call MessageNotify('E','ag_aq', &
            'The spectral dimension of input data too small.')
    elseif ( size(aq_data,2)-1 > km ) then
       call MessageNotify('W','ag_aq', &
            'The spectral dimension of input data too large.')
    endif

    ag_aq = 0.0D0
    do i=1,im
!$omp parallel do
       do j=1,jmax
             ag_aq(j,i) = sum(CB(j,i,md(j):km:2)*aq_data(j,md(j):km:2))
       enddo
!$omp end parallel do
    enddo

  end function ag_aq

  function g_q(q_data)
    !
    ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����(1 ����������).
    !
    real(8), dimension(:), intent(in)  :: q_data
    !(in) ���ڥ��ȥ�ǡ���

    real(8), dimension(im)             :: g_q
    !(out) �ʻ����ǡ���

    real(8), dimension(1,size(q_data)) :: q_work
    ! ���������
    real(8), dimension(1,im)         :: g_work
    ! ���������

    q_work(1,:) = q_data  
    g_work = ag_aq(q_work)
    g_q = g_work(1,:)

  end function g_q

! ---- ���Ѵ� ---- 
  function aq_ag(ag_data)
    !
    ! �ʻҥǡ������饹�ڥ��ȥ�ǡ������Ѵ�����(2 ����������).
    !
    real(8), dimension(:,:), intent(in)      :: ag_data
    !(in) �ʻ����ǡ���

    real(8), dimension(size(ag_data,1),0:km) :: aq_ag
    !(out) ���ڥ��ȥ�ǡ���

    integer :: j, n

    if ( size(ag_data,1) /= jmax ) then
       call MessageNotify('E','aq_ag', &
            '1st dim. of the grid data should be same as dim. of MD.')
    end if

    if ( size(ag_data,2) < im ) then
       call MessageNotify('E','aq_ag', &
            'The Grid points of input data too small.')
    elseif ( size(ag_data,2) > im ) then
       call MessageNotify('W','aq_ag', &
            'The Grid points of input data too large.')
    endif

    aq_ag = 0.0D0
!$omp parallel do private(n)
    do j=1,jmax
       do n=md(j),km,2
          aq_ag(j,n) = sum(CF(j,n,:)*ag_data(j,:))
       enddo
    enddo
!$omp end parallel do          

  end function aq_ag

  function q_g(g_data)
    !
    ! �ʻҥǡ������饹�ڥ��ȥ�ǡ������Ѵ�����(1 ����������).
    !
    real(8), dimension(:), intent(in)     :: g_data
    !(in) �ʻ����ǡ���

    real(8), dimension(0:km)              :: q_g
    !(out) ���ڥ��ȥ�ǡ���

    real(8), dimension(1,size(g_data)) :: ag_work
    real(8), dimension(1,0:km)         :: aq_work

    ag_work(1,:) = g_data
    aq_work = aq_ag(ag_work)
    q_g = aq_work(1,:)
    
  end function q_g

! ---- ��ʬ�׻� ---- 
  function aq_rDr_aq(aq_data)
    !
    ! ���ϥ��ڥ��ȥ�ǡ������Ф�����ʬ r(d/dr) �Υ��ڥ��ȥ뷸��
    ! ��׻�����(2 ����������).
    !
    !   a_n = aq_data/sqrt(Inm),  b_n = aq_rDr_aq/sqrt(Inm)
    !
    !   b_n = (2n+gamma-1)/(2n+gamma+3)b_n+2
    !       + (2n+gamma-1)(n+gamma+1)/(2n+gamma+3)a_n+2 + n a_n
    !
    real(8), dimension(:,0:), intent(in)     :: aq_data
    !(in) ���ϥ��ڥ��ȥ�ǡ���

    real(8), dimension(size(aq_data,1),0:size(aq_data,2)-1) :: aq_rDr_aq
    !(out) ���ϥ��ڥ��ȥ�ǡ���

    integer :: j, n
    integer :: nstr, nend
    real(8) :: sqInm, sqInm2

    if ( size(aq_data,1) /= jmax ) then
       call MessageNotify('E','aq_rDr_aq', &
            '1st dim. of the spectral data should be same as dim. of MD.')
    end if

    if ( size(aq_data,2)-1 < km ) then
       call MessageNotify('E','aq_rDr_aq', &
            'The spectral dimension of input data too small.')
    elseif ( size(aq_data,2)-1 > km ) then
       call MessageNotify('W','aq_rDr_aq', &
            'The spectral dimension of input data too large.')
    endif

    aq_rDr_aq = 0.0D0
!$omp parallel do private(nstr,nend,sqInm,sqInm2,n)
    do j=1,jmax
       if ( mod(md(j),2) .eq. mod(km,2) ) then
          nstr=md(j) ; nend=km
       else
          nstr=md(j) ; nend=km-1
       endif
       
       sqInm = sqrt(Inm(nend,md(j)))
       aq_rDr_aq(j,nend) = nend * aq_data(j,nend)

       do n=nend-2,nstr,-2
          sqInm2 = sqInm
          sqInm = sqrt(Inm(n,md(j)))

          aq_rDr_aq(j,n) = sqInm/sqInm2 * (&
                 (2*n+gamma-1)/(2*n+gamma+3) * aq_rDr_aq(j,n+2) &
               + (2*n+gamma-1)*(n+gamma+1)/(2*n+gamma+3) * aq_data(j,n+2) )&
               + n * aq_data(j,n)
       enddo
    enddo
!$omp end parallel do

  end function aq_rDr_aq

  function q_rDr_q(q_data)
    !
    ! ���ϥ��ڥ��ȥ�ǡ����� r(d/dR) ��ʬ����Ѥ���(1 ����������).
    !
    ! ���ڥ��ȥ�ǡ����� r(d/dR) ��ʬ�Ȥ�, �б�����ʻ����ǡ����� R ��ʬ��
    ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
    !
    !
    real(8), dimension(:), intent(in)   :: q_data
    !(in) ���ϥ����ӥ����եǡ���

    real(8), dimension(0:km)            :: q_rDr_q
    !(out) �����ӥ����եǡ����� R ��ʬ

    real(8), dimension(1,size(q_data))  :: aq_work
    ! ���������

    aq_work(1,:) = q_data
    aq_work = aq_rDr_aq(aq_work)
    q_rDr_q = aq_work(1,:)

  end function q_rDr_q

! ---- r^2 �ѷ׻� ---- 
  function aq_r2_aq(aq_data)
    !
    ! ���ϥ��ڥ��ȥ�ǡ������Ф����� r^2 �Υ��ڥ��ȥ뷸��
    ! ��׻�����(2 ����������).
    !
    !   a_n^m = aq_data/sqrt(Inm),  b_n^m = aq_rDr_aq/sqrt(Inm)
    !
    !   b_n^m = (n-|m|)(n+|m|+beta-1)/((2n+gamma-5)(2n+gamma-3)) a_n-2^m
    !       + (2n(n+gamma-1) + 2|m|(|m|+beta-1)+(gamma-3)(beta+1)
    !            /((2n+gamma+1)(2n+gamma-3)) a_n^m
    !       + (n-|m|+gamma-beta)(n+|m|+gamma-1)
    !            /((2n+gamma+3)(2n+gamma+1)) a_n+2^m
    !
    real(8), dimension(:,0:), intent(in)     :: aq_data
    !(in) ���ϥ��ڥ��ȥ�ǡ���

    real(8), dimension(size(aq_data,1),0:size(aq_data,2)-1) :: aq_r2_aq
    !(out) ���ϥ��ڥ��ȥ�ǡ���

    integer :: j, n, m
    integer :: nstr, nend
    real(8) :: sqrInp2m, sqrInm, sqrInm2m

    if ( size(aq_data,1) /= jmax ) then
       call MessageNotify('E','aq_r2_aq', &
            '1st dim. of the spectral data should be same as dim. of MD.')
    end if

    if ( size(aq_data,2)-1 < km ) then
       call MessageNotify('E','aq_r2_aq', &
            'The spectral dimension of input data too small.')
    elseif ( size(aq_data,2)-1 > km ) then
       call MessageNotify('W','aq_r2_aq', &
            'The spectral dimension of input data too large.')
    endif

    aq_r2_aq = 0.0D0

!$omp parallel do private(nstr,nend,sqrInm,sqrInm2m,sqrInp2m,n,m)
    do j=1,jmax
       if ( mod(md(j),2) .eq. mod(km,2) ) then
          nstr=md(j) ; nend=km
       else
          nstr=md(j) ; nend=km-1
       endif

       m = abs(md(j))

       n = nstr
       sqrInm   = sqrt(Inm(n,m))
       sqrInp2m = sqrt(Inm(n+2,m))
       aq_r2_aq(j,n) =                                            & 
            (2*n*(n+gamma-1) + 2*m*(m+beta-1)+(gamma-3)*(beta+1)) &
               /((2*n+gamma+1)*(2*n+gamma-3)) * aq_data(j,n)      &  
          + (n-m+gamma-beta)*(n+m+gamma-1)                        &
               /((2*n+gamma+3)*(2*n+gamma+1))                     &
                * aq_data(j,n+2) * (sqrInm/sqrInp2m)

       do n=nstr+2,nend-2,2
          sqrInm2m = sqrInm
          sqrInm   = sqrInp2m
          sqrInp2m = sqrt(Inm(n+2,m))

          aq_r2_aq(j,n) = &
             (n-m)*(n+m+beta-1)/((2*n+gamma-5)*(2*n+gamma-3))      &
             * aq_data(j,n-2)* (sqrInm/sqrInm2m)                   &
           + (2*n*(n+gamma-1) + 2*m*(m+beta-1)+(gamma-3)*(beta+1)) &
                /((2*n+gamma+1)*(2*n+gamma-3)) * aq_data(j,n)      &
           + (n-m+gamma-beta)*(n+m+gamma-1)                        &
                /((2*n+gamma+3)*(2*n+gamma+1))                     &
                * aq_data(j,n+2) * (sqrInm/sqrInp2m)
       enddo

       n = nend
       sqrInm2m = sqrInm
       sqrInm   = sqrInp2m
       aq_r2_aq(j,n) = &
            (n-m)*(n+m+beta-1)/((2*n+gamma-5)*(2*n+gamma-3))       &
             * aq_data(j,n-2)* (sqrInm/sqrInm2m)                   &
           + (2*n*(n+gamma-1) + 2*m*(m+beta-1)+(gamma-3)*(beta+1)) &
                /((2*n+gamma+1)*(2*n+gamma-3)) * aq_data(j,n)      
    enddo
!$omp end parallel do

    aq_r2_aq = aq_r2_aq * ra**2

  end function aq_r2_aq

  function q_r2_q(q_data)
    !
    ! ���ϥ��ڥ��ȥ�ǡ������Ф����� r^2 �Υ��ڥ��ȥ뷸��
    ! ��׻�����(1 ����������).
    !
    real(8), dimension(:), intent(in)   :: q_data
    !(in) ���ϥ����ӥ����եǡ���

    real(8), dimension(0:km)            :: q_r2_q
    !(out) �����ӥ����եǡ����� R ��ʬ

    real(8), dimension(1,size(q_data))  :: aq_work
    ! ���������

    aq_work(1,:) = q_data
    aq_work = aq_r2_aq(aq_work)
    q_r2_q = aq_work(1,:)

  end function q_r2_q

! ---- r^-2 �ѷ׻� ---- 
  function aq_r2Inv_aq(aq_data)
    !
    ! ���ϥ��ڥ��ȥ�ǡ������Ф����� r^-2 �Υ��ڥ��ȥ뷸��
    ! ��׻�����(2 ����������).
    !
    !   a_n^m = aq_r2Inv_aq/sqrt(Inm),  b_n^m = aq_data/sqrt(Inm)
    !
    !   b_n^m = (n-|m|)(n+|m|+beta-1)/((2n+gamma-5)(2n+gamma-3)) a_n-2^m
    !       + (2n(n+gamma-1) + 2|m|(|m|+beta-1)+(gamma-3)(beta+1)
    !            /((2n+gamma+1)(2n+gamma-3)) a_n^m
    !       + (n-|m|+gamma-beta)(n+|m|+gamma-1)
    !            /((2n+gamma+3)(2n+gamma+1)) a_n+2^m
    !
    real(8), dimension(:,0:), intent(in)     :: aq_data
    !(in) ���ϥ��ڥ��ȥ�ǡ���

    real(8), dimension(size(aq_data,1),0:size(aq_data,2)-1) :: aq_r2Inv_aq
    !(out) ���ϥ��ڥ��ȥ�ǡ���

    real(8), dimension(:,:,:), allocatable    :: R2MTX
    integer, dimension(:,:), allocatable      :: kp

    integer :: j, n, m
    integer :: nstr, nend
    real(8) :: sqrInp2m, sqrInm, sqrInm2m

    save R2MTX, kp

    if ( size(aq_data,1) /= jmax ) then
       call MessageNotify('E','aq_r2Inv_aq', &
            '1st dim. of the spectral data should be same as dim. of MD.')
    end if

    if ( size(aq_data,2)-1 < km ) then
       call MessageNotify('E','aq_r2Inv_aq', &
            'The spectral dimension of input data too small.')
    elseif ( size(aq_data,2)-1 > km ) then
       call MessageNotify('W','aq_r2Inv_aq', &
            'The spectral dimension of input data too large.')
    endif

    if ( first_r2inv ) then
       first_r2inv = .false.
       if ( allocated(R2MTX) ) deallocate(R2MTX)
       if ( allocated(kp) ) deallocate(kp)
       allocate(R2MTX(jmax,0:km,0:km),kp(jmax,0:km))

       R2MTX(:,:,:) = 0.0D0
       do n=0,km
          R2MTX(:,n,n) = 1.0D0
       enddo

!$omp parallel do private(nstr,nend,sqrInm,sqrInm2m,sqrInp2m,n,m)
       do j=1,jmax
          if ( mod(md(j),2) .eq. mod(km,2) ) then
             nstr=md(j) ; nend=km
          else
             nstr=md(j) ; nend=km-1
          endif

          m = abs(md(j))

          n = nstr

          sqrInm   = sqrt(Inm(n,m))
          sqrInp2m = sqrt(Inm(n+2,m))

          R2MTX(j,n,n)   = &
               (2*n*(n+gamma-1) + 2*m*(m+beta-1)+(gamma-3)*(beta+1)) &
               /((2*n+gamma+1)*(2*n+gamma-3))
          
          R2MTX(j,n,n+2) = &
               (n-m+gamma-beta)*(n+m+gamma-1)                        &
               /((2*n+gamma+3)*(2*n+gamma+1))                        &
               * (sqrInm/sqrInp2m)

          do n=nstr+2,nend-2,2
             sqrInm2m = sqrInm
             sqrInm   = sqrInp2m
             sqrInp2m = sqrt(Inm(n+2,m))

             R2MTX(j,n,n-2) = &
                (n-m)*(n+m+beta-1)/((2*n+gamma-5)*(2*n+gamma-3))      &
                *(sqrInm/sqrInm2m)

             R2MTX(j,n,n)   = &
                (2*n*(n+gamma-1) + 2*m*(m+beta-1)+(gamma-3)*(beta+1)) &
                   /((2*n+gamma+1)*(2*n+gamma-3))

             R2MTX(j,n,n+2) = &
                (n-m+gamma-beta)*(n+m+gamma-1)                        &
                   /((2*n+gamma+3)*(2*n+gamma+1))                     &
                   * (sqrInm/sqrInp2m)
          enddo

          n = nend
          sqrInm2m = sqrInm
          sqrInm   = sqrInp2m
             R2MTX(j,n,n-2) = &
                (n-m)*(n+m+beta-1)/((2*n+gamma-5)*(2*n+gamma-3))      &
                *(sqrInm/sqrInm2m)
             R2MTX(j,n,n)   = &
                (2*n*(n+gamma-1) + 2*m*(m+beta-1)+(gamma-3)*(beta+1)) &
                   /((2*n+gamma+1)*(2*n+gamma-3))
       enddo
!$omp end parallel do

       R2MTX = R2MTX * ra**2

       call LuDecomp(R2MTX,kp)

    end if

    aq_r2Inv_aq = LuSolve(R2MTX,kp,aq_data)
    
  end function aq_r2Inv_aq

  function q_r2Inv_q(q_data)
    !
    ! ���ϥ��ڥ��ȥ�ǡ������Ф����� r^2 �Υ��ڥ��ȥ뷸��
    ! ��׻�����(1 ����������).
    !
    real(8), dimension(:), intent(in)   :: q_data
    !(in) ���ϥ����ӥ����եǡ���

    real(8), dimension(0:km)            :: q_r2Inv_q
    !(out) �����ӥ����եǡ����� R ��ʬ

    real(8), dimension(1,size(q_data))  :: aq_work
    ! ���������

    aq_work(1,:) = q_data
    aq_work = aq_r2Inv_aq(aq_work)
    q_r2Inv_q = aq_work(1,:)

  end function q_r2Inv_q

!--------------- ��ʬ�׻� -----------------
  function a_Int_ag(ag)
    !
    ! 1 �����ʻ����ǡ������¤�� 2 �����������ʬ
    !
    !      \int_0^a f(R) W(R) dR, W(R) = R^beta/(a^2-R^2)^(1-alpha)
    !
    ! ��׻�����. 
    !
    real(8), dimension(:,:), intent(in)      :: ag
    !(in)���ϳʻ����ǡ���

    real(8), dimension(size(ag,1))           :: a_Int_ag
    !(out) ��ʬ�����ǡ���
    integer :: i

    if ( size(ag,2) < im ) then
       call MessageNotify('E','a_Int_ag', &
            'The Grid points of input data too small.')
    elseif ( size(ag,2) > im ) then
       call MessageNotify('W','a_Int_ag', &
            'The Grid points of input data too large.')
    endif

    a_Int_ag = 0.0d0
    do i=1,im
       a_Int_ag(:) = a_Int_ag(:) + ag(:,i)*g_R_Weight(i)
    enddo
  end function a_Int_ag

  function Int_g(g)
    !
    ! 1 �����ʻ����ǡ�������ʬ
    !
    !      \int_0^a f(R) W(R) dR, W(R) = R^beta/(a^2-R^2)^(1-alpha)
    !
    ! ��Ԥ�
    !
    real(8), dimension(im), intent(in)   :: g
    !(in) �ʻ����ǡ���

    real(8)                              :: Int_g
    !(out) ��ʬ��

    Int_g = sum(g*g_R_Weight)

  end function Int_g

  function a_Avr_ag(ag)
    !
    ! 1 �����ʻ����ǡ������¤�� 2 ���������ʿ��
    !
    !      \int_0^a f(R) W(R) dR/\int_0^a W(R) dR, 
    !          W(R) = R^beta/(a^2-R^2)^(1-alpha)
    !
    ! ��׻�����. 
    !
    real(8), dimension(:,0:), intent(in)   :: ag
    !(in)���ϳʻ����ǡ���

    real(8), dimension(size(ag,1))         :: a_Avr_ag
    !(out) ʿ�Ѥ����ǡ���

    a_Avr_ag = a_Int_ag(ag)/sum(g_R_Weight)

  end function a_Avr_ag

  function Avr_g(g)
    !
    ! 1 �����ʻ����ǡ�����ʿ��
    !
    !      \int_0^a f(R) W(R) dR/\int_0^a W(R) dR, 
    !          W(R) = R^beta/(a^2-R^2)^(1-alpha)
    !
    ! ��׻�����. 
    !
    real(8), dimension(im), intent(in)   :: g
    !(in) �ʻ����ǡ���

    real(8)                              :: Avr_g
    !(out) ��ʬ��

    Avr_g = Int_g(g)/sum(g_R_Weight)
  end function Avr_g

!---- Dirichlet ���������(����ˡ) ----

  subroutine aq_BoundaryTau_D_2d(aq_data,value)
    !
    ! Dirichlet ����������Ŭ��(����ˡ, 2 ����������)
    ! * ��¦����(i=im)�Ǥ��ͤ�Ϳ����.
    !
    real(8), dimension(:,0:),intent(inout)         :: aq_data 
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(jmax,0:km)

    real(8), dimension(:), intent(in), optional  :: value
    !(in) ������(jmax)

    real(8), dimension(:,:,:), allocatable  :: alu
    integer, dimension(:,:), allocatable    :: kp
    real(8), dimension(size(aq_data,1),0:km):: aq_work
    real(8), dimension(size(aq_data,1),im)  :: ag_work
    real(8), dimension(size(aq_data,1))     :: value0           ! ������

    integer :: k, j, kstr, kend
    save    :: alu, kp

    if ( size(aq_data,2)-1 < km ) then
       call MessageNotify('E','aq_BoundaryTau_D', &
            'The spectral dimension of input data too small.')
    elseif ( size(aq_data,2)-1 > km ) then
       call MessageNotify('W','aq_BoundaryTau_D', &
            'The spectral dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0.0D0
    else
       value0 = value
    endif

    if ( first_Tau_D ) then
       first_Tau_D = .false.

       allocate(alu(size(aq_data,1),0:km,0:km),kp(size(aq_data,1),0:km))

       alu=0.0D0
       do k=0,km
          alu(:,k,k) = 1.0D0
       enddo

       do j=1,jmax
          if ( mod(md(j),2) .eq. mod(km,2) ) then
             kstr = md(j) ; kend = km
          else
             kstr = md(j) ; kend = km-1
          endif

          do k=kstr,kend,2
             aq_work = 0.0D0
             aq_work(j,k) = 1.0D0
             ag_work = ag_aq(aq_work)
             alu(j,kend,k) = ag_work(j,im)
          enddo
       enddo

       call ludecomp(alu,kp)
    endif

    do j=1,jmax
       if ( mod(md(j),2) .eq. mod(km,2) ) then
          aq_data(j,km)   = value0(j)
       else
          aq_data(j,km-1)   = value0(j)
       endif
    enddo

    aq_data = lusolve(alu,kp,aq_data)

  end subroutine aq_BoundaryTau_D_2d

  subroutine aq_BoundaryTau_D_1d(q_data,value)
    !
    ! Dirichlet ����������Ŭ��(����ˡ, 1 ����������)
    ! * ξ�����Ǥ��ͤ�Ϳ����.
    !
    real(8), dimension(0:km),intent(inout)       :: q_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(0:km)

    real(8), intent(in), optional                :: value
    !(in) ������

    real(8), dimension(1,0:km)                   :: aq_work
    real(8), dimension(1)                        :: vwork  ! ������

    if (.not. present(value)) then
       vwork(1)=0.0D0
    else
       vwork(1) = value
    endif

    aq_work(1,:)=q_data
    call aq_BoundaryTau_D_2d(aq_work,vwork)
    q_data=aq_work(1,:)

  end subroutine aq_BoundaryTau_D_1d

!---- Neumann ���������(����ˡ) ----

  subroutine aq_BoundaryTau_N_2d(aq_data,value)
    !
    ! ��¦���� Neumann ����������Ŭ��(����ˡ, 2 ����������)
    ! * i=im �Ǹ��ۤ��ͤ�Ϳ����.
    !
    real(8), dimension(:,0:),intent(inout)         :: aq_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(m,0:km)

    real(8), dimension(:), intent(in), optional    :: value
    !(in) ������(m)

    real(8), dimension(:,:,:), allocatable    :: alu
    integer, dimension(:,:), allocatable      :: kp
    real(8), dimension(size(aq_data,1),0:km)  :: aq_work
    real(8), dimension(size(aq_data,1),im)    :: ag_work
    real(8), dimension(size(aq_data,1))       :: value0           ! ������

    integer :: j, k, kstr, kend

    save    :: alu, kp

    if ( size(aq_data,2)-1 < km ) then
       call MessageNotify('E','aq_BoundaryTau_N', &
            'The spectral dimension of input data too small.')
    elseif ( size(aq_data,2)-1 > km ) then
       call MessageNotify('W','aq_BoundaryTau_N', &
            'The spectral dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0.0D0
    else
       value0 = value
    endif

    if ( first_Tau_N ) then
       first_Tau_N = .false.
       allocate(alu(jmax,0:km,0:km),kp(jmax,0:km))

       alu=0.0D0
       do k=0,km
          alu(:,k,k) = 1.0D0
       enddo

       do j=1,jmax
          if ( mod(md(j),2) .eq. mod(km,2) ) then
             kstr=md(j) ; kend = km
          else
             kstr=md(j) ; kend = km-1
          endif

          do k=kstr,kend,2
             aq_work = 0.0D0
             aq_work(j,k) = 1.0D0
             ag_work = ag_aq(aq_rDr_aq(aq_work))/spread(g_R,1,jmax)
             alu(j,kend,k) = ag_work(j,im)
          enddo
       enddo

       call ludecomp(alu,kp)
    endif

    do j=1,jmax
       if ( mod(md(j),2) .eq. mod(km,2) ) then
          aq_data(j,km)   = value0(j)
       else
          aq_data(j,km-1)   = value0(j)
       endif
    enddo

    aq_data = lusolve(alu,kp,aq_data)

  end subroutine aq_BoundaryTau_N_2d

  subroutine aq_BoundaryTau_N_1d(q_data,value)
    !
    ! Dirichlet/Neumann ����������Ŭ��(����ˡ, 1 ����������)
    ! * i=0 �Ǹ��ۤ��ͤ�Ϳ����.
    !
    real(8), dimension(0:km),intent(inout)       :: q_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(0:km)

    real(8), intent(in), optional                :: value
    !(in) ������

    real(8), dimension(1,0:km)                   :: aq_work
    real(8), dimension(1)                        :: vwork           ! ������

    if (.not. present(value)) then
       vwork(1)=0.0D0
    else
       vwork(1) = value
    endif

    aq_work(1,:)=q_data
    call aq_BoundaryTau_N_2d(aq_work,vwork)
    q_data=aq_work(1,:)

  end subroutine aq_BoundaryTau_N_1d

!---- Dirichlet ���������(����ˡ) ----

  subroutine ag_BoundaryGrid_D_2d(ag_data,value)
    !
    ! Dirichlet ����������Ŭ��(����ˡ, 2 ����������)
    ! * ��¦����(i=im)�Ǥ��ͤ�Ϳ����.
    !
    real(8), dimension(:,:),intent(inout)         :: ag_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(jmax,im)

    real(8), dimension(:), intent(in), optional  :: value
    !(in) ������(jmax)

    real(8), dimension(size(ag_data,1))       :: value0 

    if (.not. present(value)) then
       value0=0.0d0
    else
       value0 = value
    endif
    ag_data(:,im) = value0

  end subroutine ag_BoundaryGrid_D_2d

  subroutine ag_BoundaryGrid_D_1d(g_data,value)
    !
    ! Dirichlet ����������Ŭ��(����ˡ, 1 ����������)
    ! * ξ�����Ǥ��ͤ�Ϳ����.
    !
    real(8), dimension(im),intent(inout)       :: g_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(im)

    real(8), intent(in), optional              :: value
    !(in) ������

    real(8), dimension(1,im)                   :: ag_work
    real(8), dimension(1)                      :: vwork

    if (.not. present(value)) then
       vwork(1)=0.0d0
    else
       vwork(1) = value
    endif

    ag_work(1,:)=g_data
    call ag_BoundaryGrid_D_2d(ag_work,vwork)
    g_data=ag_work(1,:)

  end subroutine ag_BoundaryGrid_D_1d

!---- Neumann ���������(����ˡ) ----

  subroutine ag_BoundaryGrid_N_2d(ag_data,value)
    !
    ! ��¦���� Neumann ����������Ŭ��(����ˡ, 2 ����������)
    ! * i=im �Ǹ��ۤ��ͤ�Ϳ����.
    !
    real(8), dimension(:,:),intent(inout)         :: ag_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(m,im)

    real(8), dimension(:), intent(in), optional    :: value
    !(in) ������(m)

    real(8), dimension(:,:,:), allocatable    :: alu
    integer, dimension(:,:), allocatable      :: kp
    real(8), dimension(size(ag_data,1),im)    :: ag_work
    real(8), dimension(size(ag_data,1))       :: value0           ! ������

    integer :: i
    save    :: alu, kp

    if ( size(ag_data,2) < im ) then
       call MessageNotify('E','aq_BoundaryGrid_N', &
            'The dimension of input data too small.')
    elseif ( size(ag_data,2) > im ) then
       call MessageNotify('W','aq_BoundaryGrid_N', &
            'The dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0.0D0
    else
       value0 = value
    endif

    if ( first_Grid_N ) then
       first_Grid_N = .false.
       allocate(alu(jmax,im,im),kp(jmax,im))

       alu=0.0D0
       do i=1,im
          alu(:,i,i) = 1.0D0
       enddo

       do i=1,im
          ag_work = 0.0D0
          ag_work(:,i) = 1.0D0
          ag_work = ag_aq(aq_rDr_aq(aq_ag(ag_work)))/spread(g_R,1,jmax)
          alu(:,im,i) = ag_work(:,im)
       enddo

       call ludecomp(alu,kp)
    endif

    ag_data(:,im) = value0

    ag_data = lusolve(alu,kp,ag_data)

  end subroutine ag_BoundaryGrid_N_2d

  subroutine ag_BoundaryGrid_N_1d(g_data,value)
    !
    ! Dirichlet/Neumann ����������Ŭ��(����ˡ, 1 ����������)
    ! * i=0 �Ǹ��ۤ��ͤ�Ϳ����.
    !
    real(8), dimension(im),intent(inout)       :: g_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(0:km)

    real(8), intent(in), optional                :: value
    !(in) ������

    real(8), dimension(1,im)                   :: ag_work
    real(8), dimension(1)                        :: vwork           ! ������

    if (.not. present(value)) then
       vwork(1)=0.0D0
    else
       vwork(1) = value
    endif

    ag_work(1,:)=g_data
    call ag_BoundaryGrid_N_2d(ag_work,vwork)
    g_data=ag_work(1,:)

  end subroutine ag_BoundaryGrid_N_1d

!---- �����롼����
  subroutine gauss_radau(M,g_R,g_W)
    !
    ! M ����¿�༰ ��(r) ����������
    ! ������-������ʻ����ȥ������Ťߤ�׻�����
    !
    integer, intent(in)  :: M                
    ! (in) ¿�༰���ڼ���(0 < M, M even)
    real(8), intent(out) :: g_R(M/2)         ! ������-������ʻ���
    real(8), intent(out) :: g_W(M/2)         ! �������Ť�

    real(8) :: r1,r2,dr
    real(8) :: pi1, pi2
    integer :: i, ir
    character(len=4) :: CNUM
    real(8), parameter :: eps = 1.0D-15      ! ���ڸ�
    integer, parameter :: nrfact = 100       ! �ʻ��������ֳ֤������

    !--- ���ϥ����å�
    if ( mod(M,2) .ne. 0 ) then
       call MessageNotify('E','gauss_radau','M must be even')
    endif

    !--- ������--������ʻ���
    dr = 1.0D0/(nrfact*M)

    i = 0.0D0
    do ir = 1, nrfact*M
       r1  = ir*dr     ;  r2  = (ir+1)*dr 
       pi1 = pir(r1,M) ;  pi2 = pir(r2,M)

       if ( pi1*pi2 .le. 0.0D0 ) then
          i = i+1
          g_R(i) = bisec_pir( r1, r2, M , eps )

          if ( i .eq. M/2-1 ) then
             i = i+1
             g_R(M/2) = 1.0D0
             goto 10
          endif
       end if
    enddo

    write( CNUM,'(I4)') i
    call MessageNotify('E','gauss_radau',&
         'Only '//CNUM//' Gauss-Radau points are found')

    !--- �������Ť�
10  continue
    write( CNUM,'(I4)') i
    call MessageNotify('M','gauss_radau',CNUM//' Gauss-Radau points are set up.')

    do i=1,M/2-1
       g_W(i) = 2*(2*M+gamma-5)*g_R(i)**2     &
                /((M+gamma-beta-2)*(M+gamma-3)*Phi(g_R(i),M-2,0)**2)
    enddo

    g_W(M/2) = Inm(0,0) - sum(g_W(1:M/2-1))

  end subroutine gauss_radau

  function bisec_pir( r1, r2, n, eps )
    !
    !  2 ʬˡ�� ��(r,n) ���������� [r1,r2] �δ֤Ǹ� eps ��õ��
    !
    real(8)             :: r1, r2
    integer, intent(in) :: n
    real(8), intent(in) :: eps
    real(8)             :: bisec_pir
    
    real(8) :: rm, pim
    real(8) :: pi1, pi2

    pi1 = pir(r1,n)
    if ( abs(pi1) .lt. eps ) then
       bisec_pir = r1 
       return
    end if

    pi2 = pir(r2,n)
    if ( abs(pi2) .lt. eps ) then
       bisec_pir = r2
       return
    end if

10  rm = (r1+r2)/2
    pim = pir(rm,n)

    if ( abs(pim) .lt. eps ) then
       bisec_pir = rm 
       return
    end if

    if ( abs(r1-r2) .lt. eps ) then
       bisec_pir = rm 
       return
    end if
    
    if ( pim * pi1 .gt. 0.0D0 ) then
       r1 = rm
       pi1 = pim
    else
       r2 = rm
       pi2 = pim
    endif
    goto 10

  end function bisec_pir

  function pir(r,n)
    !
    ! ������-������ʻ�����������뤿��δؿ�
    !
    real(8), intent(in)  :: r                ! ư�º�ɸ ( 0 < r < ra )
    integer, intent(in)  :: n                ! ¿�༰����(0 <= n)
    real(8)              :: pir              ! ¿�༰����

    pir = qnm(r,n,0)-(n+gamma-beta-2)*(n+gamma-3)*qnm(r,n-2,0)/(n*(n+beta-1))

  end function pir

  function Phi(r,n_ind,m_ind)
    !
    ! ���������줿¿�༰�׻�
    !
    real(8), intent(in)  :: r                ! ư�º�ɸ ( 0 < r < ra )
    integer, intent(in)  :: n_ind            ! ¿�༰����(0 <= |m| <= n)
    integer, intent(in)  :: m_ind            ! ¿�༰����(0 <= |m| <= n)
    real(8)              :: Phi              ! ¿�༰����

    Phi = qnm(r,n_ind,m_ind)/sqrt(Inm(n_ind,m_ind))
  end function Phi

  function qnm(r,n_ind,m_ind)
    !
    ! �������ˤ��¿�༰�׻�
    !
    real(8), intent(in)  :: r                ! ư�º�ɸ ( 0 < r < ra )
    integer, intent(in)  :: n_ind            ! ¿�༰����(0 <= |m| <= n)
    integer, intent(in)  :: m_ind            ! ¿�༰����(0 <= |m| <= n)
    real(8)              :: qnm              ! ¿�༰����

    real(8) :: qnp2m, qnm2m
    integer :: m, n

    m = abs(m_ind)

    !--- ���ϥ����å�
    if ( n_ind .lt. 0 ) then
       call MessageNotify('E','qnm','n must be larger equal to 0')
    endif

    if ( m .gt. n_ind ) then
       call MessageNotify('E','qnm','abs(m) must be smaller equal to n')
    endif
    
    if ( mod(n_ind+m_ind,2) .ne. 0 ) then
       call MessageNotify('E','qnm','n+m must be even')
    endif

    !--- �����
    if ( m .eq. n_ind ) then
       qnm = r**m
       return
    endif

    if ( n_ind .eq. m+2 ) then
       qnm = (2*m+gamma-1)/2*((2*m+gamma+1)/(2*m+beta+1)*r**2 -1) * r**m
       return
    endif

    qnm2m = r**m
    qnm = (2*m+gamma-1)/2*((2*m+gamma+1)/(2*m+beta+1)*r**2 -1)* r**m

    !--- ������

    do n = m+2, n_ind-2, 2
       qnp2m =( ( 2*n + gamma - 1 ) &
                * (  ( 2*n + gamma - 3 )*( 2*n + gamma + 1) * r**2       &
                    - 2*n * (n + gamma -1) - 2*m * (m + beta -1)       &
                    -(gamma-3) * (beta+1)                            )*qnm &
               -(n-m+gamma-beta-2)*(n+m+gamma-3)*(2*n+gamma+1)*qnm2m )&
              /(( n - m + 2 )*( n + m + beta + 1 )*( 2*n + gamma - 3 ))
       qnm2m = qnm
       qnm = qnp2m
    enddo

    return
  end function qnm

  function Inm(n_ind,m_ind)
    !
    ! ¿�༰�ε��ʲ�������������ˤ�����
    !
    integer, intent(in)  :: n_ind            ! ¿�༰����(0 <= |m| <= n)
    integer, intent(in)  :: m_ind            ! ¿�༰����(0 <= |m| <= n)
    real(8)              :: Inm              ! ���ʲ����

    integer :: n,m
    real(8) :: Inm2m
    real(8) :: Inmln                         ! ���ʲ������log
    real(8) :: gammaln
    external gammaln

    m=abs(m_ind)

    !--- ���ϥ����å�
    if ( n_ind .lt. 0 ) then
       call MessageNotify('E','Inm','n must be larger equal to 0')
    endif

    if ( m .gt. n_ind ) then
       call MessageNotify('E','Inm','abs(m) must be smaller equal to n')
    endif
    
    if ( mod(n_ind+m_ind,2) .ne. 0 ) then
       call MessageNotify('E','Inm','n+m must be even')
    endif

    !--- �����
    Inmln = gammaln((gamma-beta)/2) + gammaln(m+(beta+1)/2) &
          - gammaln(m+(gamma+1)/2) - log(2.0D0)
    Inm = exp(Inmln)

    !--- ������
    do n=m+2, n_ind, 2
       Inm2m = Inm
       Inm = (2*n+gamma-5)*(n-m+gamma-beta-2)*(n+m+gamma-3) &
               /((n-m)*(n+m+beta-1)*(2*n+gamma-1)) * Inm2m
    enddo

    return
  end function Inm
       
end module aq_module
