!----------------------------------------------------------------------
!   COPYRIGHT (c) 2005-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_ab_galerkin_ND2
!
!      �����ӥ����աݥ��顼����ˡ
!      ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭������ѥ⥸�塼��
!          f'(:,i=0)=f(:,i=im)=0 [ f'(:,x=xmax)=f(:,x=xmin)=0 ]
!
!      k ���Υ��顼������줬
!
!        \phi_k(x)=T_k(x) + C_1 T_1(x) + C_0 T_0(x) 
!
!      �η���(Type2)
!
!����  2005/12/30  �ݹ�����  ��������
!      2006/01/07  �ݹ�����  debug
!      2006/01/20  �ݹ�����  �����Ѵ��꼰���ѹ�
!      2006/01/23  �ݹ�����  �⥸�塼��̾�ѹ�
!      2006/01/25  �ݹ�����  �⥸�塼�������ե�å�Ƴ��
!
module at_ab_galerkin_ND

  use lumatrix
  use at_module
  use dc_message

  implicit none

  private
  public :: at_ab_galerkin_ND_Initial  ! ��������֥롼����
  public :: ab_at, b_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: ab_ag, b_g                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: at_ab, t_b                 ! ���顼����->�����ӥ������Ѵ�
  public :: ag_ab, g_b                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: ab_Dx_ab, b_Dx_b           ! X ��ʬ

  real(8), allocatable :: TB(:,:)      ! ���顼����->�����ӥ������Ѵ�����
  real(8), allocatable :: BT(:,:)      ! �����ӥ�����->���顼�����Ѵ�����
  integer, allocatable :: kp(:)        ! LU ʬ���ѥԥܥåȳ�Ǽ����

  real(8), allocatable :: alpha(:)     ! �����ӥ�����<->���顼��������ѷ���
  real(8), allocatable :: beta(:)      ! �����ӥ�����<->���顼��������·���

  integer :: im                        ! �ʻ�����              
  integer :: km                        ! �����ӥ����������ȿ�  
  integer,parameter :: ks=2            ! ���顼���������㼡��

  logical :: ND_Initialized=.false.    ! ������ե�å�

  save im, km, TB, BT, kp, alpha, beta, ND_Initialized
  public TB, BT, alpha, beta

contains

  !
  ! �����ӥ����աݥ��顼����ˡ 
  ! ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭������ѥ⥸�塼��
  ! ��������֥롼����
  !
  subroutine at_ab_galerkin_ND_Initial(i_in,k_in)

    integer, intent(IN) :: i_in     ! (in)�ʻ�����
    integer, intent(IN) :: k_in     ! (in)�����ӥ����������ȿ�  

    integer :: k, l, m, n

    im=i_in ; km=k_in

    allocate(TB(0:km,ks:km),BT(ks:km,ks:km),kp(ks:km))
    allocate(alpha(0:km),beta(0:km))

    ! ��ü�Υ��ޥ���ü�ǥ��ꥯ�������Ѵ���������
    TB = 0.0D0
    do k=ks,km
       TB(0,k) = -k**2 +(-1)**(k+1)
       TB(1,k) = -k**2
       TB(k,k) = 1.0D0
    enddo

    beta=1.0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! ��ü�Υ��ޥ���ü�ǥ��ꥯ�������Ѵ��չ���
    alpha=1.0 ; alpha(0)=2.0D0

    BT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             BT(m,n) = BT(m,n) + alpha(l)*TB(l,m)*TB(l,n)
          enddo
       enddo
    enddo

    call LUDecomp(BT,kp)

    call MessageNotify('M','at_ab_galerkin_ND_Initial(Type2)',&
                           'Conversion matrices initialized')

    ND_Initialized=.true.

  end subroutine at_ab_galerkin_ND_Initial

  !
  ! ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭�����
  ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
  !
  function ab_at(at_data)
    real(8), intent(IN) :: at_data(:,0:)                !(in)  �����ӥ����շ���
    real(8)             :: ab_at(size(at_data,1),ks:km) !(out) ���顼���󷸿�  

    real(8)             :: ab_work(size(at_data,1),ks:km)  ! ���������

    integer :: k,m

    if ( .not. ND_Initialized ) &
         call MessageNotify('E','ab_at',&
                            'at_ab_galerkin_ND_module(Type2) not initialized')

    ab_work =0.0
    do m=ks,km
       do k=0,km
          ab_work(:,m) = ab_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TB(k,m)
       enddo
    enddo

    ab_at = LUSolve(BT,kp,ab_work)
  end function ab_at

  !
  ! ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭�����
  ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
  !
  function b_t(t_data)

    real(8), intent(IN) :: t_data(0:km)        !(in)  �����ӥ����շ���
    real(8)             :: b_t(ks:km)          !(out) ���顼���󷸿�  

    real(8)             :: b_work(ks:km)       ! ���������

    integer :: k,m

    if ( .not. ND_Initialized ) &
         call MessageNotify('E','b_t',&
                            'at_ab_galerkin_ND_module(Type2) not initialized')

    b_work =0.0
    do m=ks,km
       do k=0,km
          b_work(m) = b_work(m) &
               + alpha(k) * beta(k) * t_data(k) * TB(k,m)
       enddo
    enddo

    b_t = LUSolve(BT,kp,b_work)
  end function b_t

  !
  ! ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭�����
  ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(2�����ǡ���)
  !
  function at_ab(ab_data)

    real(8), intent(IN)  :: ab_data(:,ks:)              !(in)  ���顼���󷸿�
    real(8)              :: at_ab(size(ab_data,1),0:km) !(out) �����ӥ����շ���

    integer :: m, n

    if ( .not. ND_Initialized ) &
         call MessageNotify('E','at_ab',&
                            'at_ab_galerkin_ND_module(Type2) not initialized')

    at_ab = 0.0D0
    do m=0,km
       do n=ks,km
          at_ab(:,m) = at_ab(:,m) + TB(m,n)*ab_data(:,n)/beta(m)
       enddo
    enddo

  end function at_ab

  !
  ! ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭�����
  ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(1�����ǡ���)
  !
  function t_b(b_data)

    real(8), intent(IN)  :: b_data(ks:km)       !(in)  ���顼���󷸿�  
    real(8)              :: t_b(0:km)           !(out) �����ӥ����շ���

    integer :: m, n

    if ( .not. ND_Initialized ) &
         call MessageNotify('E','t_b',&
                            'at_ab_galerkin_ND_module(Type2) not initialized')

    t_b = 0.0D0
    do m=0,km
       do n=ks,km
          t_b(m) = t_b(m) + TB(m,n)*b_data(n)/beta(m)
       enddo
    enddo

  end function t_b

  !
  ! ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭�����
  ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
  !
  function ab_ag(ag_data)
    real(8), intent(IN)  :: ag_data(:,0:)                !(in)  �ʻ����ǡ���
    real(8)              :: ab_ag(size(ag_data,1),ks:km) !(out) ���顼���󷸿�

    ab_ag = ab_at(at_ag(ag_data))
  end function ab_ag

  !
  ! ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭�����
  ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
  !
  function b_g(g_data)
    real(8), intent(IN)  :: g_data(0:im)        !(in)  �ʻ����ǡ���
    real(8)              :: b_g(ks:km)          !(out) ���顼���󷸿�

    b_g = b_t(t_g(g_data))
  end function b_g

  !
  ! ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭�����
  ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(2�����ǡ���)
  !
  function ag_ab(ab_data)
    real(8), intent(IN) :: ab_data(:,ks:)              !(in)  ���顼���󷸿�  
    real(8)             :: ag_ab(size(ab_data,1),0:im) !(out) �ʻ����ǡ���  
    
    ag_ab = ag_at(at_ab(ab_data))
  end function ag_ab

  !
  ! ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭�����
  ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(1�����ǡ���)
  !
  function g_b(b_data)
    real(8), intent(IN) :: b_data(ks:km)        !(in)  ���顼���󷸿�  
    real(8)             :: g_b(0:im)            !(out) �ʻ����ǡ���  
    
    g_b = g_t(t_b(b_data))
  end function g_b

  !
  ! ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭�����
  ! X ��ʬ�׻�(1 ����)
  !
  function ab_Dx_ab(ab_data)
    real(8), intent(IN) :: ab_data(:,ks:)                  !(in) ���顼���󷸿�
    real(8)             :: ab_Dx_ab(size(ab_data,1),ks:km) !(out) ��ʬ���顼����
    ab_Dx_ab = ab_at(at_Dx_at(at_ab(ab_data)))
  end function ab_Dx_ab

  !
  ! ��ü�Υ��ޥ���ü�ǥ��ꥯ�춭�����
  ! X ��ʬ�׻�(1 ����)
  !
  function b_Dx_b(b_data)
    real(8), intent(IN) :: b_data(ks:km)
    real(8)             :: b_Dx_b(ks:km)

    b_Dx_b = b_t(t_Dx_t(t_b(b_data)))

  end function b_Dx_b

end module at_ab_galerkin_ND
