!--
!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  au_module
!
!      spml/au_module �⥸�塼��� 1 ����ͭ���ΰ�β��Ǥ�ή�α�ư��
!      2 ���ΰ����Υ��եȤ��줿�����ӥ����մؿ� T_k(2r^2-1)��Ÿ������
!      ���ڥ��ȥ���ͷ׻������ Fortran90 �ؿ����󶡤���. 
!      ���Υ롼�����Υ�����˥����ӥ�����--������--������ʻ�����Ŭ��
!      ���Ƥ���, ��� 2 �����˺�ɸ,  ������ɸ, ���ɸ�θ�����
!      �ð�������򤷤ʤ��饹�ڥ��ȥ�׻���Ԥ�������Ѥ��뤳�Ȥ�
!      ǰƬ�ˤ����Ƥ���. 
!
!      2 �����ǡ����� 1 �����˴ؤ���Ʊ���˥��ڥ��ȥ�׻���¹Ԥ��뤿���
!      �ؿ����󶡤��Ƥ���, 2, 3 �����ΰ�Ǥη׻��Υ١������󶡤���. 
!
!
!����  2007/12/27  �ݹ�����  ��������
!      2008/01/10  �ݹ�����  �����Ƚ���
!      2009/01/09  �ݹ�����  au_Initial ��å����������դ��ɲ�
!      2009/01/29  ��������ʿ �����Ȥ�RDoc�Ѥ˽���
!
!++
module au_module
  !
  != au_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: au_module.f90,v 1.4 2009-02-28 21:33:46 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/au_module �⥸�塼��� 1 ����ͭ���ΰ�β��Ǥ�ή�α�ư��
  ! 2 ���ΰ����Υ��եȤ��줿�����ӥ����մؿ� T_k(2r^2/a^2-1)��
  ! Ÿ�����륹�ڥ��ȥ���ͷ׻������ Fortran90 �ؿ����󶡤���. 
  ! ���Υ롼�����Υ�����˥����ӥ�����--������--������ʻ�����Ŭ��
  ! ���Ƥ���, ��� 2 �����˺�ɸ,  ������ɸ, ���ɸ�θ�����
  ! �ð�������򤷤ʤ��饹�ڥ��ȥ�׻���Ԥ�������Ѥ��뤳�Ȥ�
  ! ǰƬ�ˤ����Ƥ���. 
  !
  ! 2 �����ǡ����� 1 �����˴ؤ���Ʊ���˥��ڥ��ȥ�׻���¹Ԥ��뤿���
  ! �ؿ����󶡤��Ƥ���, 2, 3 �����ΰ�Ǥη׻��Υ١������󶡤���. 
  !
  ! ����Ū��Ÿ���λ�����
  !
  !         f(r)=��f_k r^n T_k(2r^2/a^2-1)
  !
  ! �Ǥ���. 1 ���������ӥ����եǡ�����ʣ���¤�� 2 �����ǡ�����
  ! �����ݤˤϽŤߤλؿ� n ��ƥǡ����������Ǥ���. 
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (u_, g_, au_, ag_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   u_  :: �����ӥ����եǡ���
  !   g_  :: 1 �����ʻ����ǡ���
  !   au_ :: 1 ���������ӥ����եǡ�����ʣ���¤�� 2 �����ǡ���
  !   ag_ :: 1 �����ʻ����ǡ�����ʣ���¤�� 2 �����ǡ���.
  !
  !   ʣ���¤�� 2 �����ǡ������� 1 �������礭���� au_Initial ��
  !   ���ꤹ��Ťߤλؿ������� nd ���礭����Ʊ���Ǥʤ���Фʤ�ʤ�.
  !
  ! * �ؿ�̾�δ֤�ʸ����(Dr)��, ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_e,_au,_g, _ag) ��, �����ѿ��η��������ӥ����եǡ���
  !   ����ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !    _u  :: �����ӥ����եǡ���
  !    _g  :: 1 �����ʻ����ǡ���
  !    _au :: 1 ���������ӥ����եǡ�����ʣ���¤�� 2 �����ǡ���
  !    _ag :: 1 �����ʻ����ǡ�����ʣ���¤�� 2 �����ǡ���
  !
  !=== �ƥǡ����μ��������
  !
  ! * g : 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:im). 
  !   * im �� R ��ɸ�γʻ������Ǥ���, ���֥롼���� au_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���.
  !
  ! * u : �����ӥ����եǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:km). 
  !     km �� R �����κ����ȿ��Ǥ���, ���֥롼���� au_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���. ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ˤĤ��Ƥ�...
  !
  ! * ag : 1 ����(R)�ʻ����ǡ������¤�� 2 �����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(size(nd),0:im). 
  !     �� 2 ������ R ������ɽ��.
  !
  ! * au : 1 ���������ӥ����եǡ������¤�� 2 �����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(size(nd),0:km). 
  !     �� 2 ���������ڥ��ȥ��ɽ��.
  !
  ! * g_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ�����Ʊ��.
  !
  ! * u_ �ǻϤޤ�ؿ����֤��ͤϥ����ӥ����եǡ�����Ʊ��.
  !
  ! * ag_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ������¤�� 
  !   2 �����ǡ�����Ʊ��.
  !
  ! * au_ �ǻϤޤ�ؿ����֤��ͤ� 1 ���������ӥ����եǡ������¤��
  !   2 �����ǡ�����Ʊ��.
  !
  ! * �����ӥ����եǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ���������ӥ������Ѵ�������ΤΤ��ȤǤ���.
  !
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! au_Initial  :: �����ӥ������Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! g_R        :: �ʻ�����ɸ(R)���Ǽ���� 1 ��������
  ! g_R_Weight :: �Ťߺ�ɸ���Ǽ���� 1 ��������
  !
  !==== �����Ѵ�
  !
  ! g_u, ag_au :: �����ӥ����եǡ�������ʻҥǡ����ؤ��Ѵ�
  ! u_g, au_ag :: �ʻҥǡ�����������ӥ����եǡ����ؤ��Ѵ�
  !
  !==== ��ʬ
  !
  ! g_Dr_u, ag_Dr_au   :: �����ӥ����եǡ����� R ��ʬ����Ѥ�����
  ! g_Dr2_u, ag_Dr2_au :: �����ӥ����եǡ����� 2 �� R ��ʬ����Ѥ�����
  !
  !==== ��ʬ��ʿ��
  !
  ! a_Int_ag, a_Avr_ag :: 1 �����ʻ����ǡ������¤�� 2 �����������ʬ�����ʿ��
  ! Int_g, Avr_g       :: 1 �����ʻ����ǡ�������ʬ�����ʿ��
  !
  !==== ����������
  !
  ! au_Boundary_D, au_Boundary_N         :: �ǥ��ꥯ��,�Υ��ޥ�¦�������
  ! au_BoundaryTau_D, au_BoundaryTau_N   :: �ǥ��ꥯ��,�Υ��ޥ�¦�������
  ! au_BoundaryGrid_D, au_BoundaryGrid_N :: �ǥ��ꥯ��,�Υ��ޥ�¦�������
  !
  use dc_message
  use lumatrix
  implicit none
  private
  public g_R, g_R_Weight                        ! ��ɸ�ѿ�
  public au_Initial                             ! �����
  public ag_au, au_ag, g_u, u_g                 ! �����Ѵ�
  public ag_Dr_au, g_Dr_u                       ! ��ʬ
  public ag_Dr2_au, g_Dr2_u                     ! ��ʬ
  public a_Int_ag, Int_g, a_Avr_ag, Avr_g       ! ��ʬ��ʿ��
  public au_Boundary_D, au_Boundary_N           ! �������
  public au_BoundaryTau_D, au_BoundaryTau_N     ! �������
  public au_BoundaryGrid_D, au_BoundaryGrid_N   ! �������

  interface au_Boundary_D
     !
     ! ��¦�����ǥ��ꥯ�췿��������Ŭ��(����ˡ). 
     !
     !  * ��������Ŭ�Ѥ�������μ����ˤ�ä������ǥ��֥롼�����
     !    �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !    �����롼�����Ƥ�ɬ�פϤʤ�.
     !
     ! �����ȷ�̤η�
     !
     !  * 1 ���������ӥ����եǡ������¤�� 2 ��������ξ��
     !
     !    real(8), dimension(size(nd),0:km),intent(inout)    :: au_data
     !    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !
     !    real(8), dimension(size(nd)), intent(in), optional :: value
     !    !(in) Ŭ�Ѥ��붭����
     !
     !  * 1 ���������ӥ����եǡ����ξ��
     !
     !    real(8), dimension(0:km),intent(inout)       :: u_data
     !    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !
     !    real(8), intent(in), optional                :: value
     !    !(in) Ŭ�Ѥ��붭����
     !
     module procedure au_BoundaryTau_D_1d, au_BoundaryTau_D_2d
  end interface

  interface au_Boundary_N
     !
     ! ��¦�Υ��ޥ󷿶�������Ŭ��(����ˡ).
     ! i=0 �Ǹ��ۤ��ͤ�Ϳ����.
     !
     !  * ��������Ŭ�Ѥ�������μ����ˤ�ä������ǥ��֥롼�����
     !    �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !    �����롼�����Ƥ�ɬ�פϤʤ�.
     !
     ! �����ȷ�̤η�
     !
     !  * 1 ���������ӥ����եǡ������¤�� 2 ��������ξ��
     !
     !    real(8), dimension(:,0:km),intent(inout)       :: au_data
     !    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !
     !    real(8), dimension(size(at_data,1)), intent(in), optional :: value
     !    !(in) Ŭ�Ѥ��붭����
     !
     !  * 1 ���������ӥ����եǡ����ξ��
     !
     !    real(8), dimension(0:km),intent(inout)       :: t_data
     !    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !
     !    real(8), intent(in), optional                 :: value
     !    !(in) Ŭ�Ѥ��붭����
     !
     module procedure au_BoundaryTau_N_1d, au_BoundaryTau_N_2d
  end interface

  interface au_BoundaryTau_D
     !
     ! ��¦�����ǥ��ꥯ�췿��������Ŭ��(����ˡ). 
     !
     !  * ��������Ŭ�Ѥ�������μ����ˤ�ä������ǥ��֥롼�����
     !    �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !    �����롼�����Ƥ�ɬ�פϤʤ�.
     !
     ! �����ȷ�̤η�
     !
     !  * 1 ���������ӥ����եǡ������¤�� 2 ��������ξ��
     !
     !    real(8), dimension(size(nd),0:km),intent(inout)    :: au_data
     !    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !
     !    real(8), dimension(size(nd)), intent(in), optional :: value
     !    !(in) Ŭ�Ѥ��붭����
     !
     !  * 1 ���������ӥ����եǡ����ξ��
     !
     !    real(8), dimension(0:km),intent(inout)       :: u_data
     !    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !
     !    real(8), intent(in), optional                :: value
     !    !(in) Ŭ�Ѥ��붭����
     !
     module procedure au_BoundaryTau_D_1d, au_BoundaryTau_D_2d
  end interface

  interface au_BoundaryTau_N
     !
     ! ��¦�Υ��ޥ󷿶�������Ŭ��(����ˡ).
     ! i=0 �Ǹ��ۤ��ͤ�Ϳ����.
     !
     !  * ��������Ŭ�Ѥ�������μ����ˤ�ä������ǥ��֥롼�����
     !    �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !    �����롼�����Ƥ�ɬ�פϤʤ�.
     !
     ! �����ȷ�̤η�
     !
     !  * 1 ���������ӥ����եǡ������¤�� 2 ��������ξ��
     !
     !    real(8), dimension(size(nd),0:km),intent(inout)    :: au_data
     !    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !
     !    real(8), dimension(size(nd)), intent(in), optional :: value
     !    !(in) Ŭ�Ѥ��붭����
     !
     !  * 1 ���������ӥ����եǡ����ξ��
     !
     !    real(8), dimension(0:km),intent(inout)       :: t_data
     !    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !
     !    real(8), intent(in), optional                 :: value
     !    !(in) Ŭ�Ѥ��붭����
     !
     module procedure au_BoundaryTau_N_1d, au_BoundaryTau_N_2d
  end interface

  interface au_BoundaryGrid_D
     !
     ! ��¦�ǥ��ꥯ�췿��������Ŭ��(�¶��֤Ǥ�ɾ��).
     ! ��¦����(i=0)�Ǥ��ͤ�Ϳ����.
     !
     !  * ��������Ŭ�Ѥ�������μ����ˤ�ä������ǥ��֥롼�����
     !    �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !    �����롼�����Ƥ�ɬ�פϤʤ�.
     !
     ! �����ȷ�̤η�
     !
     !  * 1 ���������ӥ����եǡ������¤�� 2 ��������ξ��
     !
     !    real(8), dimension(size(nd),0:km),intent(inout)    :: au_data
     !    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !
     !    real(8), dimension(size(nd)), intent(in), optional :: value
     !    !(in) Ŭ�Ѥ��붭����
     !
     !  * 1 ���������ӥ����եǡ����ξ��
     !
     !    real(8), dimension(0:km),intent(inout)       :: u_data
     !    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !
     !    real(8), intent(in), optional                 :: value
     !    !(in) Ŭ�Ѥ��붭����
     !
     module procedure au_BoundaryGrid_D_1d, au_BoundaryGrid_D_2d
  end interface

  interface au_BoundaryGrid_N
     !
     ! ��¦�����Υ��ޥ󷿶�������Ŭ��(�¶��֤Ǥ�ɾ��).
     ! i=0 �Ǹ��ۤ��ͤ�Ϳ����.
     !
     !  * ��������Ŭ�Ѥ�������μ����ˤ�ä������ǥ��֥롼�����
     !    �Ȥ�ʬ���Ƥ���. �桼�������󥿡��ե������϶��̤Ǥ���Τ�
     !    �����롼�����Ƥ�ɬ�פϤʤ�.
     !
     ! �����ȷ�̤η�
     !
     !  * 1 ���������ӥ����եǡ������¤�� 2 ��������ξ��
     !
     !    real(8), dimension(size(nd),0:km),intent(inout)    :: au_data
     !    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !
     !    real(8), dimension(size(nd)), intent(in), optional :: value
     !    !(in) Ŭ�Ѥ��붭����
     !
     !  * 1 ���������ӥ����եǡ����ξ��
     !
     !    real(8), dimension(0:km),intent(inout)       :: u_data
     !    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���
     !
     !    real(8), intent(in), optional                :: value
     !    !(in) Ŭ�Ѥ��붭����
     !
     module procedure au_BoundaryGrid_N_1d, au_BoundaryGrid_N_2d
  end interface

  real(8), parameter                 :: pi=3.1415926535897932385D0

  integer :: im, km                        ! �ʻ�����, �����ȿ�
  real(8) :: ra                            ! �ΰ���礭��

  integer, allocatable :: nd(:)
  ! r^n �λؿ�

  real(8), allocatable :: g_R(:)
  ! �ʻ�����ɸ
  ! Chebyshev-Gauss-Radau �ʻ���

  real(8), allocatable :: g_R_Weight(:)
  ! �ʻ����Ťߺ�ɸ
  ! �Ƴʻ����ˤ�������ʬ�Τ���νŤߤ���Ǽ���Ƥ���

  real(8), allocatable :: CF(:,:), CB(:,:)   ! ���Ѵ������Ѵ��ѹ���

  save :: im, km, ra, CF, CB, g_R, g_R_Weight, nd

contains

! ---- ����� ---- 
  subroutine au_Initial(i_in,k_in,r_in,nd_in)
    !
    ! �����ӥ������Ѵ��γʻ�����, �ȿ�, �ΰ���礭��, �Ťߤ����ꤹ��.
    ! 
    ! ¾�δؿ����ѿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ��
    ! �������򤷤ʤ���Фʤ�ʤ�.
    !
    integer,intent(in) :: i_in              !(in) �ʻ�����
    integer,intent(in) :: k_in              !(in) �����ȿ�
    real(8),intent(in) :: r_in              !(in) ��¦�����κ�ɸ(Ⱦ��)
    integer,intent(in) :: nd_in(:)          !(in) �Ť� r^n �λؿ�

    real(8), allocatable :: c(:), w(:)   ! �Ťߴؿ�
    real(8) :: t
    integer :: ii,kk

    im=i_in ; km=k_in ; ra = r_in

    if ( km .gt. im ) then
       call MessageNotify('E','au_initial','KM shoud be less equal to IM')
    endif

    if ( allocated(nd) ) deallocate(nd)
    allocate(nd(size(nd_in)))
    nd = nd_in

    if ( allocated(CF) ) deallocate(CF)
    if ( allocated(CB) ) deallocate(CB)
    allocate(CF(0:im,0:im),CB(0:im,0:im))

    if ( allocated(C) ) deallocate(W)
    if ( allocated(W) ) deallocate(W)
    allocate(c(0:im),w(0:im))

    c = 1.0D0 ; c(0) = 2.0D0
    w = 2*pi/(2*im+1) ; w(0) = pi/(2*im+1)

    do ii=0,im
       do kk=0,im
          CF(kk,ii)=2.0D0/(pi*c(kk))*cos(2*pi*ii*kk/(2*im+1))*w(ii)
       enddo
    enddo

    do kk=0,im
       do ii=0,im
          CB(ii,kk)=cos(2*pi*ii*kk/(2*im+1))
       enddo
    enddo

    if ( allocated(g_R) ) deallocate(g_R)
    allocate(g_R(0:im))

    do ii=0,im
       t = 2*pi*ii/(2*im+1)
       g_R(ii) = ra*sqrt((1+cos(t))/2)    ! cos(t) = 2(r/a)^2 - 1
    enddo

    if ( allocated(g_R_Weight) ) deallocate(g_R_Weight)
    allocate(g_R_Weight(0:im))
    do ii=0,im
       g_R_Weight(ii) = 0.0
       do kk=0,km
          g_R_Weight(ii) = g_R_Weight(ii) &
               + 1.0D0/(1.0D0/4.0D0-kk**2) &
               * cos(2*kk*ii*pi/(2*im+1))/c(kk)
       enddo
       g_R_Weight(ii) = ra/(2*pi) * g_R_Weight(ii)* w(ii)
    enddo

    deallocate(c,w)

    call MessageNotify('M','au_initial','au_module (2009/01/09) is initialized')
  end subroutine au_Initial

! ---- ���Ѵ� ---- 
  function ag_au(au_data)
    !
    ! �����ӥ����եǡ�������ʻҥǡ������Ѵ�����(2 ����������).
    !
    double precision, dimension(:,:), intent(in)       :: au_data
    !(in) �����ӥ����եǡ���

    double precision, dimension(size(au_data,1),0:im)  :: ag_au
    !(out) �ʻ����ǡ���

    double precision, dimension(size(au_data,1),0:im)  :: au_work
    ! ���������

    integer :: m, mm, i

    mm = size(au_data,1)
    if ( mm /= size(nd) ) then
       call MessageNotify('E','ag_au', &
            '1st dim. of the Chebyshev data should be same as dim. of ND.')
    end if

    if ( size(au_data,2)-1 < km ) then
       call MessageNotify('E','ag_au', &
            'The Chebyshev dimension of input data too small.')
    elseif ( size(au_data,2)-1 > km ) then
       call MessageNotify('W','ag_au', &
            'The Chebyshev dimension of input data too large.')
    endif

    au_work = 0.0 ; au_work(:,0:km) = au_data
    do m=1,mm
       do i=0,im
          ag_au(m,i) = sum(CB(i,:)*au_work(m,:))*g_R(i)**nd(m)
       enddo
    enddo

  end function ag_au

  function g_u(u_data)
    !
    ! �����ӥ����եǡ�������ʻҥǡ������Ѵ�����(1 ����������).
    !
    double precision, dimension(:), intent(in)  :: u_data
    !(in) �����ӥ����եǡ���

    double precision, dimension(0:im)           :: g_u
    !(out) �ʻ����ǡ���

    double precision, dimension(1,size(u_data)) :: u_work
    ! ���������
    double precision, dimension(1,0:im)         :: g_work
    ! ���������

    u_work(1,:) = u_data  
    g_work = ag_au(u_work)
    g_u = g_work(1,:)

  end function g_u

! ---- ���Ѵ� ---- 
  function au_ag(ag_data)
    !
    ! �ʻҥǡ�����������ӥ����եǡ������Ѵ�����(2 ����������).
    !
    double precision, dimension(:,:), intent(in)      :: ag_data
    !(in) �ʻ����ǡ���

    double precision, dimension(size(ag_data,1),0:km) :: au_ag
    !(out) �����ӥ����եǡ���

    double precision, dimension(size(ag_data,1),0:im) :: au_work
    !���������

    integer :: m, mm, k

    mm = size(ag_data,1)
    if ( mm /= size(nd) ) then
       call MessageNotify('E','ag_au', &
            '1st dim. of the Chebyshev data should be same as dim. of ND.')
    end if

    if ( size(ag_data,2)-1 < im ) then
       call MessageNotify('E','au_ag', &
            'The Grid points of input data too small.')
    elseif ( size(ag_data,2)-1 > im ) then
       call MessageNotify('W','au_ag', &
            'The Grid points of input data too large.')
    endif

    do m=1,mm
       do k=0,im
          au_work(m,k) = sum(CF(k,:)*ag_data(m,:)/g_R**nd(m))
       enddo
    enddo
    au_ag = au_work(:,0:km)

  end function au_ag

  function u_g(g_data)  ! �ʻ� -> ���ڥ��ȥ�
    !
    ! �ʻҥǡ�����������ӥ����եǡ������Ѵ�����(1 ����������).
    !
    double precision, dimension(:), intent(in)     :: g_data
    !(in) �ʻ����ǡ���

    double precision, dimension(0:km)              :: u_g
    !(out) �����ӥ����եǡ���

    real(8), dimension(1,size(g_data)) :: ag_work
    real(8), dimension(1,0:km)         :: au_work

    ag_work(1,:) = g_data
    au_work = au_ag(ag_work)
    u_g = au_work(1,:)
    
  end function u_g

! ---- ��ʬ�׻� ---- 
  function au_Dx_au(au_data)
    !
    ! ���ϥ����ӥ����եǡ��� f_k T_k(x) ���Ф��� x ��ʬ�Υ����ӥ����շ���
    ! g_k T_k(x) = f_k T_k'(x) ��׻�����(2 ����������).
    !
    real(8), dimension(:,0:), intent(in)     :: au_data
    !(in) ���ϥ����ӥ����եǡ���

    real(8), dimension(size(au_data,1),0:size(au_data,2)-1) :: au_Dx_au
    !(in) ���ϥ����ӥ����եǡ���

    integer :: m, k
    integer :: nm, kmax

    nm=size(au_data,1)
    kmax=size(au_data,2)-1
    if ( kmax  < km ) then
       call MessageNotify('W','au_Dx_au', &
            'The Chebyshev dimension of input data too small.')
    elseif ( kmax > km ) then
       call MessageNotify('E','au_Dx_au', &
            'The Chebyshev dimension of input data too large.')
    endif

    do m=1,nm
       au_Dx_au(m,kmax)   = 0. 
       au_Dx_au(m,kmax-1) = 2 * kmax * au_data(m,kmax) 
    enddo

    do k=kmax-2,1,-1
       do m=1,nm
          au_Dx_au(m,k) = au_Dx_au(m,k+2) + 2*(k+1)*au_data(m,k+1)
       enddo
    enddo

    do m=1,nm
       au_Dx_au(m,0) = (au_Dx_au(m,2) + 2*au_data(m,1))/2.0D0
    enddo

  end function au_Dx_au

  function ag_Dr_au(au_data)
    !
    ! ���ϥ����ӥ����եǡ����� R ��ʬ����Ѥ���(2 ����������).
    !
    ! �����ӥ����եǡ����� R ��ʬ�Ȥ�, �б�����ʻ����ǡ����� R ��ʬ��
    ! ���Ѥ������ǡ����Υ����ӥ������Ѵ��Τ��ȤǤ���.
    !
    !
    real(8), dimension(:,0:), intent(in)     :: au_data
    !(in) ���ϥ����ӥ����եǡ���

    real(8), dimension(size(au_data,1),0:im) :: ag_Dr_au
    !(out) �����ӥ����եǡ����� R ��ʬ

    integer :: nm

    nm=size(au_data,1)

    ag_Dr_au = ag_au(au_Dx_au(au_data)) * 4*spread(g_R,1,nm)/ra**2&
             + spread(nd,2,im+1)/spread(g_R,1,nm) * ag_au(au_data)
  end function ag_Dr_au

  function g_Dr_u(u_data)
    !
    ! ���ϥ����ӥ����եǡ����� R ��ʬ����Ѥ���(1 ����������).
    !
    ! �����ӥ����եǡ����� R ��ʬ�Ȥ�, �б�����ʻ����ǡ����� R ��ʬ��
    ! ���Ѥ������ǡ����Υ����ӥ������Ѵ��Τ��ȤǤ���.
    !
    !
    real(8), dimension(:), intent(in)   :: u_data
    !(in) ���ϥ����ӥ����եǡ���

    real(8), dimension(0:im)            :: g_Dr_u
    !(out) �����ӥ����եǡ����� R ��ʬ

    real(8), dimension(1,size(u_data))  :: au_work
    ! ���������

    real(8), dimension(1,0:im)          :: ag_work
    ! ���������
    
    au_work(1,:) = u_data
    ag_work = ag_Dr_au(au_work)
    g_Dr_u = ag_work(1,:)

  end function g_Dr_u

  function ag_Dr2_au(au_data)
    !
    ! ���ϥ����ӥ����եǡ����� 2 �� R ��ʬ����Ѥ���(2 ����������).
    !
    ! �����ӥ����եǡ����� 2 �� R ��ʬ�Ȥ�, �б�����ʻ����ǡ�����
    ! 2 �� R ��ʬ����Ѥ������ǡ����Υ����ӥ������Ѵ��Τ��ȤǤ���.
    !
    !
    real(8), dimension(:,0:), intent(in)     :: au_data
    !(in) ���ϥ����ӥ����եǡ���

    real(8), dimension(size(au_data,1),0:im) :: ag_Dr2_au
    !(out) �����ӥ����եǡ����� 2 �� R ��ʬ

    real(8), dimension(size(au_data,1),0:size(au_data,2)-1)  :: au_g
    real(8), dimension(size(au_data,1),0:size(au_data,2)-1)  :: au_h
    ! �������

    integer :: nm

    nm=size(au_data,1)

    au_g = au_Dx_au(au_data)
    au_h = au_Dx_au(au_g)

    ag_Dr2_au = 16*spread(g_R,1,nm)**2/ra**4 * ag_au(au_h)  &
              + 4*spread(2*nd+1,2,im+1)/ra**2 * ag_au(au_g) &
              + spread(nd*(nd-1),2,im+1)/spread(g_R**2,1,nm) * ag_au(au_data)
  end function ag_Dr2_au

  function g_Dr2_u(u_data)
    !
    ! ���ϥ����ӥ����եǡ����� 2 �� R ��ʬ����Ѥ���(1 ����������).
    !
    ! �����ӥ����եǡ����� 2 �� R ��ʬ�Ȥ�, �б�����ʻ����ǡ����� 
    ! 2 �� R ��ʬ����Ѥ������ǡ����Υ����ӥ������Ѵ��Τ��ȤǤ���.
    !
    !
    real(8), dimension(:), intent(in)   :: u_data
    !(in) ���ϥ����ӥ����եǡ���

    real(8), dimension(0:im)            :: g_Dr2_u
    !(out) �����ӥ����եǡ����� 2 �� R ��ʬ

    real(8), dimension(1,size(u_data))  :: au_work
    ! ���������

    real(8), dimension(1,0:im)          :: ag_work
    ! ���������
    
    au_work(1,:) = u_data
    ag_work = ag_Dr2_au(au_work)
    g_Dr2_u = ag_work(1,:)

  end function g_Dr2_u

  !--------------- ��ʬ�׻� -----------------
    function a_Int_ag(ag)
      !
      ! 1 �����ʻ����ǡ������¤�� 2 �����������ʬ
      !
      real(8), dimension(:,0:), intent(in)     :: ag
      !(in)���ϳʻ����ǡ���

      real(8), dimension(size(ag,1))           :: a_Int_ag
      !(out) ��ʬ�����ǡ���
      integer :: i

      if ( size(ag,2) < im+1 ) then
         call MessageNotify('E','a_Int_ag', &
              'The Grid points of input data too small.')
      elseif ( size(ag,2) > im+1 ) then
         call MessageNotify('W','a_Int_ag', &
              'The Grid points of input data too large.')
      endif

      a_Int_ag = 0.0d0
      do i=0,im
         a_Int_ag(:) = a_Int_ag(:) + ag(:,i)*g_R_Weight(i)
      enddo
    end function a_Int_ag

    function Int_g(g)
      !
      ! 1 �����ʻ����ǡ�������ʬ�����ʿ��.
      !
      real(8), dimension(0:im), intent(in)   :: g
      !(in) �ʻ����ǡ���

      real(8)                                :: Int_g
      !(out) ��ʬ��

      Int_g = sum(g*g_R_Weight)
    end function Int_g

    function a_Avr_ag(ag)
      !
      ! 1 �����ʻ����ǡ������¤�� 2 ���������ʿ��
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
      real(8), dimension(0:im), intent(in)   :: g
      !(in) �ʻ����ǡ���

      real(8)                                :: Avr_g
      !(out) ��ʬ��

      Avr_g = Int_g(g)/sum(g_R_Weight)
    end function Avr_g

!---- Dirichlet ���������(����ˡ) ----

  subroutine au_BoundaryTau_D_2d(au_data,value)
    !
    ! Dirichlet ����������Ŭ��(����ˡ, 2 ����������)
    ! ��¦����(i=0)�Ǥ��ͤ�Ϳ����.
    !
    real(8), dimension(:,0:),intent(inout)         :: au_data ! �ǡ���(m,0:km)
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(m,0:km)

    real(8), dimension(:), intent(in), optional  :: value
    !(in) ������(m)

    real(8), dimension(:,:,:), allocatable  :: alu
    integer, dimension(:,:), allocatable    :: kp
    real(8), dimension(size(nd),0:km)        :: au_work
    real(8), dimension(size(nd),0:im)        :: ag_work
    real(8), dimension(size(au_data,1))   :: value0           ! ������

    logical :: first = .true.
    integer :: k
    save    :: alu, kp, first

    if ( size(au_data,2)-1 < km ) then
       call MessageNotify('E','au_BoundaryTau_D', &
            'The Chebyshev dimension of input data too small.')
    elseif ( size(au_data,2)-1 > km ) then
       call MessageNotify('W','au_BoundaryTau_D', &
            'The Chebyshev dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0
    else
       value0 = value
    endif

    if ( first ) then
       first = .false.

       allocate(alu(size(nd),0:km,0:km),kp(size(nd),0:km))

       alu=0.0D0
       do k=0,km
          alu(:,k,k) = 1.0D0
       enddo

       do k=0,km
          au_work = 0.0
          au_work(:,k) = 1.0
          ag_work = ag_au(au_work)
          alu(:,km,k) = ag_work(:,0)
       enddo

       call ludecomp(alu,kp)
    endif

    au_data(:,km)   = value0
    au_data = lusolve(alu,kp,au_data)

  end subroutine au_BoundaryTau_D_2d

  subroutine au_BoundaryTau_D_1d(u_data,value)
    !
    ! Dirichlet ����������Ŭ��(����ˡ, 1 ����������)
    ! ξ�����Ǥ��ͤ�Ϳ����.
    !
    real(8), dimension(0:km),intent(inout)       :: u_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(0:km)

    real(8), intent(in), optional                :: value
    !(in) ������

    real(8), dimension(1,0:km)                   :: au_work
    real(8), dimension(1)                        :: vwork           ! ������

    if (.not. present(value)) then
       vwork(1)=0
    else
       vwork(1) = value
    endif

    au_work(1,:)=u_data
    call au_BoundaryTau_D_2d(au_work,vwork)
    u_data=au_work(1,:)

  end subroutine au_BoundaryTau_D_1d

!---- Neumann ���������(����ˡ) ----

  subroutine au_BoundaryTau_N_2d(au_data,value)
    !
    ! ��¦���� Neumann ����������Ŭ��(����ˡ, 2 ����������)
    ! i=0 �Ǹ��ۤ��ͤ�Ϳ����.
    !
    real(8), dimension(:,0:),intent(inout)         :: au_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(m,0:km)

    real(8), dimension(:), intent(in), optional    :: value
    !(in) ������(m)

    real(8), dimension(:,:,:), allocatable  :: alu
    integer, dimension(:,:), allocatable    :: kp
    real(8), dimension(size(nd),0:km)        :: au_work
    real(8), dimension(size(nd),0:im)        :: ag_work
    real(8), dimension(size(au_data,1))   :: value0           ! ������

    logical :: first = .true.
    integer :: k
    save    :: alu, kp, first

    if ( size(au_data,2)-1 < km ) then
       call MessageNotify('E','au_BoundaryTau_DN', &
            'The Chebyshev dimension of input data too small.')
    elseif ( size(au_data,2)-1 > km ) then
       call MessageNotify('W','au_BoundaryTau_DN', &
            'The Chebyshev dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0
    else
       value0 = value
    endif

    if ( first ) then
       first = .false.
       allocate(alu(size(nd),0:km,0:km),kp(size(nd),0:km))

       alu=0.0D0
       do k=0,km
          alu(:,k,k) = 1.0D0
       enddo

       do k=0,km
          au_work = 0.0
          au_work(:,k) = 1.0
          ag_work = ag_Dr_au(au_work)
          alu(:,km,k) = ag_work(:,0)
       enddo

       call ludecomp(alu,kp)
    endif

    au_data(:,km)   = value0
    au_data = lusolve(alu,kp,au_data)

  end subroutine au_BoundaryTau_N_2d

  subroutine au_BoundaryTau_N_1d(u_data,value)
    !
    ! Dirichlet/Neumann ����������Ŭ��(����ˡ, 1 ����������)
    ! i=0 �Ǹ��ۤ��ͤ�Ϳ����.
    !
    real(8), dimension(0:km),intent(inout)       :: u_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(0:km)

    real(8), intent(in), optional                :: value
    !(in) ������

    real(8), dimension(1,0:km)                   :: au_work
    real(8), dimension(1)                        :: vwork           ! ������

    if (.not. present(value)) then
       vwork(1)=0
    else
       vwork(1) = value
    endif

    au_work(1,:)=u_data
    call au_BoundaryTau_N_2d(au_work,vwork)
    u_data=au_work(1,:)

  end subroutine au_BoundaryTau_N_1d

!---- Dirichlet ���������(�¶��֤Ǥ�ɾ��) ----

  subroutine au_BoundaryGrid_D_2d(au_data,value)
    !
    ! Dirichlet ����������Ŭ��(�¶��֤Ǥ�ɾ��, 2 ����������)
    ! ��¦�����Ǥ��ͤ�Ϳ����.
    !
    real(8), dimension(:,0:),intent(inout)         :: au_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(m,0:km)

    real(8), dimension(:), intent(in), optional    :: value
    !(in) ������(m)

    real(8), dimension(:,:,:), allocatable     :: alu
    integer, dimension(:,:), allocatable       :: kp
    real(8), dimension(size(au_data,1),0:im)   :: ag_data
    real(8), dimension(size(nd),0:km)          :: au_work
    real(8), dimension(size(au_data,1))        :: value0 ! ������

    logical :: first = .true.
    integer :: k
    save    :: alu, kp, first

    if ( im /= km ) then
       call MessageNotify('E','au_BoundaryGrid_DD', &
            'Chebyshev truncation and number of grid points should be same.')
    endif

    if ( size(au_data,2)-1 < km ) then
       call MessageNotify('E','au_BoundaryGrid_DD', &
            'The Chebyshev dimension of input data too small.')
    elseif ( size(au_data,2)-1 > km ) then
       call MessageNotify('W','au_BoundaryGrid_DD', &
            'The Chebyshev dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0
    else
       value0=value   
    endif

    if ( first ) then
       first = .false.
       allocate(alu(size(nd),0:im,0:km),kp(size(nd),0:im))

       do k=0,km
          au_work = 0
          au_work(:,k)=1.0
          alu(:,:,k) = ag_au(au_work)
       enddo

       call ludecomp(alu,kp)
    endif

    ag_data = ag_au(au_data)
    ag_data(:,0)  = value0
    au_data = lusolve(alu,kp,ag_data)

  end subroutine au_BoundaryGrid_D_2d

  subroutine au_BoundaryGrid_D_1d(u_data,value)
    !
    ! Dirichlet ����������Ŭ��(�¶��֤Ǥ�ɾ��, 1 ����������)
    ! ��¦�����Ǥ��ͤ�Ϳ����.
    !
    real(8), dimension(0:km),intent(inout)       :: u_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(0:km)

    real(8), intent(in), optional                :: value
    !(in) ������

    real(8), dimension(1,0:km)                   :: au_work
    real(8), dimension(1)                        :: vwork           ! ������

    if (.not. present(value)) then
       vwork(1)=0
    else
       vwork(1) = value
    endif

    au_work(1,:)=u_data
    call au_BoundaryGrid_D_2d(au_work,vwork)
    u_data=au_work(1,:)

  end subroutine au_BoundaryGrid_D_1d

!---- Neumann ���������(�¶��֤Ǥ�ɾ��) ----

  subroutine au_BoundaryGrid_N_2d(au_data,value)
    !
    ! ��¦���� Neumann ����������Ŭ��(�¶��֤Ǥ�ɾ��, 2 ����������)
    ! i=0 �Ǹ��ۤ��ͤ�Ϳ����.
    !
    real(8), dimension(:,0:),intent(inout)         :: au_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(m,0:km)

    real(8), dimension(:), intent(in), optional    :: value
    !(in) ������(m)

    real(8), dimension(:,:,:), allocatable   :: alu
    integer, dimension(:,:), allocatable     :: kp
    real(8), dimension(size(au_data,1),0:im) :: ag_data
    real(8), dimension(size(nd),0:km)        :: au_work
    real(8), dimension(size(nd),0:im)        :: ag_work
    real(8), dimension(size(au_data,1))      :: value0   ! ������

    logical :: first = .true.
    integer :: k
    save    :: alu, kp, first

    if ( im /= km ) then
       call MessageNotify('E','au_BoundaryGrid_N', &
            'Chebyshev truncation and number of grid points should be same.')
    endif

    if ( size(au_data,2)-1 < km ) then
       call MessageNotify('E','au_BoundaryGrid_N', &
            'The Chebyshev dimension of input data too small.')
    elseif ( size(au_data,2)-1 > km ) then
       call MessageNotify('W','au_BoundaryGrid_DN', &
            'The Chebyshev dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0
    else
       value0 = value
    endif

    if ( first ) then
       first = .false.
       allocate(alu(size(nd),0:im,0:km),kp(size(nd),0:im))

       do k=0,km
          au_work = 0
          au_work(:,k)=1.0
          ag_work = ag_au(au_work)
          alu(:,:,k) = ag_work
       enddo

       do k=0,km
          au_work = 0
          au_work(:,k)=1.0
          ag_work = ag_Dr_au(au_work)
          alu(:,0,k) = ag_work(:,0)
       enddo

       call ludecomp(alu,kp)
    endif

    ag_data = ag_au(au_data)
    ag_data(:,0)  = value0
    au_data = lusolve(alu,kp,ag_data)

  end subroutine au_BoundaryGrid_N_2d

  subroutine au_BoundaryGrid_N_1d(u_data,value)
    !
    ! ��¦���� Neumann ����������Ŭ��(�¶��֤Ǥ�ɾ��, 1 ����������)
    ! i=0 �Ǹ��ۤ��ͤ�Ϳ����.
    !
    real(8), dimension(0:km),intent(inout)       :: u_data
    !(inout) ��������Ŭ�Ѥ�������ӥ����եǡ���(0:km)

    real(8), intent(in), optional                :: value
    !(in) ������

    real(8), dimension(1,0:km)                   :: au_work
    real(8), dimension(1)                        :: vwork           ! ������

    if (.not. present(value)) then
       vwork(1)=0
    else
       vwork(1) = value
    endif

    au_work(1,:)=u_data
    call au_BoundaryGrid_N_2d(au_work,vwork)
    u_data=au_work(1,:)

  end subroutine au_BoundaryGrid_N_1d

end module au_module

