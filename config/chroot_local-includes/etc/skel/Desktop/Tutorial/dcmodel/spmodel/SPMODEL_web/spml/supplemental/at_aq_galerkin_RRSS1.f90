!----------------------------------------------------------------------
!   COPYRIGHT (c) 2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_aq_galerkin_RRSS1
!
!      �����ӥ����աݥ��顼����ˡ
!      �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
!
!      case RR: ξüǴ����(ξü���ͤ� 1 ����ʬ�� 0)
!               f(:,i=0)=f(:,i=im)=f'(:,i=0)=f'(:,i=im)=0 
!               [ f(:,x=xmax)=f(:,x=xmin)=f'(:,x=xmax)=0=f'(:,x=xmin)=0 ]
!
!      case SS: ξü��ͳ���٤���(ξü���ͤ� 2 ����ʬ�� 0)
!               f(:,i=0)=f(:,i=im)=f''(:,i=0)=f''(:,i=im)=0 
!               [ f(:,x=xmax)=f(:,x=xmin)=f''(:,x=xmax)=0=f''(:,x=xmin)=0 ]
!
!      case RS: ��üǴ����, ��ü��ͳ���٤���
!               (ξü���ͤ� 0, ��¦�� 1 ����ʬ, �⤦������ 2 ����ʬ�� 0)
!               f(:,i=0)=f(:,i=im)=f'(:,i=0)=f''(:,i=im)=0 
!               [ f(:,x=xmax)=f(:,x=xmin)=f''(:,x=xmax)=0=f'(:,x=xmin)=0 ]
!
!      case SR: ��ü��ͳ���٤���, ��üǴ����
!               (ξü���ͤ� 0, ��¦�� 2 ����ʬ, �⤦������ 1 ����ʬ�� 0)
!               f(:,i=0)=f(:,i=im)=f''(:,i=0)=f'(:,i=im)=0 
!               [ f(:,x=xmax)=f(:,x=xmin)=f'(:,x=xmax)=0=f''(:,x=xmin)=0 ]
!
!      k ���Υ��顼������줬
!        \phi_k(x)=T_k(x) + C_{k-1}T_{k-1}(x) + C_{k-2}T_{k-2}(x) 
!                         + C_{k-3}T_{k-3}(x) + C_{k-4}T_{k-4}(x) 
!      �η���(Type1)
!
!����  2006/01/24  �ݹ�����  ��������
!      2006/01/25  �ݹ�����  �⥸�塼�������ե�å�Ƴ��
!
module at_aq_galerkin_RRSS

  use lumatrix
  use at_module
  use dc_message

  implicit none

  private
  public :: at_aq_galerkin_RRSS_Initial  ! ��������֥롼����
  public :: aq_at, q_t                   ! �����ӥ�����->���顼�����Ѵ�
  public :: aq_ag, q_g                   ! �ʻ���->���顼����ǡ����Ѵ�
  public :: at_aq, t_q                   ! ���顼����->�����ӥ������Ѵ�
  public :: ag_aq, g_q                   ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: aq_Dx_aq, q_Dx_q             ! X ��ʬ

  real(8), allocatable :: TQ(:,:)   ! ���顼����->�����ӥ������Ѵ�����
  real(8), allocatable :: QT(:,:)   ! �����ӥ�����->���顼�����Ѵ�����
  integer, allocatable :: kp(:)     ! LU ʬ���ѥԥܥåȳ�Ǽ����

  real(8), allocatable :: alpha(:)  ! �����ӥ�����<->���顼��������ѷ���
  real(8), allocatable :: beta(:)   ! �����ӥ�����<->���顼��������·���

  integer :: im                     ! �ʻ�����              
  integer :: km                     ! �����ӥ����������ȿ�  
  integer,parameter :: ks=4         ! ���顼���������㼡��
  character(LEN=2) :: bc            ! �������(RR/SS/RS/SR)

  logical :: RRSS_Initialized=.false.    ! ������ե�å�

  save im, km, bc, TQ, QT, kp, alpha, beta, RRSS_Initialized
  public TQ, QT, alpha, beta

contains

  !
  ! �����ӥ����աݥ��顼����ˡ 
  ! ξüǴ�����ѥ⥸�塼��
  ! ��������֥롼����
  !
  subroutine at_aq_galerkin_RRSS_Initial(i_in,k_in,bc_in)

    integer, intent(IN)          :: i_in     ! (in)�ʻ�����
    integer, intent(IN)          :: k_in     ! (in)�����ӥ����������ȿ�  
    character(LEN=2), intent(IN) :: bc_in    ! (in)�������(RR/SS/RS/SR)

    integer :: k, l, m, n
    real(8) :: delta

    im=i_in ; km=k_in ; bc=bc_in

    allocate(TQ(0:km,ks:km),QT(ks:km,ks:km),kp(ks:km))
    allocate(alpha(0:km),beta(0:km))


    select case (bc)

    !---- ξüǴ�������Ѵ���������
    case ('RR','rr')
       TQ = 0.0D0
       do k=ks,km
          TQ(k,k)   = 1.0
          TQ(k-2,k) = -2.0*(K-2.0D0)/(K-3.0D0)
          TQ(k-4,k) = (K-1.0D0)/(K-3.0D0)
       enddo
       call MessageNotify('M','at_aq_galerkin_RRSS_Initial(Type1)',&
                          'Conversion matrices for RR-B.C. initialized.')

    !---- ξü��ͳ���٤������Ѵ���������
    case ('SS','ss')
       TQ = 0.0D0
       do k=ks,km
          TQ(k,k)   = 1.0
          TQ(k-2,k) = -(T2(k)-T2(k-4))/(T2(k-2)-T2(k-4))
          TQ(k-4,k) = (T2(k)-T2(k-2))/(T2(k-2)-T2(k-4))
       enddo
       call MessageNotify('M','at_aq_galerkin_RRSS_Initial(Type1)',&
                          'Conversion matrices for SS-B.C. initialized.')


    !---- ��üǴ����, ��ü��ͳ���٤����Ѵ���������
    case ('RS','rs')
       TQ = 0.0D0
       do k=ks,km
          delta = (T1(k-1)-T1(k-3))*(T2(k-2)-T2(k-4)) &
                + (T2(k-1)-T2(k-3))*(T1(k-2)-T1(k-4))
          TQ(k,k)   = 1.0
          TQ(k-3,k) = ( (T2(k-2)-T2(k-4))*(T1(k)-T1(k-2)) &
                        - (T1(k-2)-T1(k-4))*(T2(k)-T2(k-2)) )/delta
          TQ(k-4,k) = ( (T2(k-1)-T2(k-3))*(T1(k)-T1(k-2)) &
                        + (T1(k-1)-T1(k-3))*(T2(k)-T2(k-2)) )/delta
          TQ(k-1,k) = -TQ(k-3,k)
          TQ(k-2,k) = -TQ(k-4,k) - 1.0D0
       enddo
       call MessageNotify('M','at_aq_galerkin_RRSS_Initial(Type1)',&
                          'Conversion matrices for RS-B.C. initialized.')

    !---- ��ü��ͳ���٤���, ��üǴ�����Ѵ���������
    case ('SR','sr')
       TQ = 0.0D0
       do k=ks,km
          delta = (T2(k-1)-T2(k-3))*(T1(k-2)-T1(k-4)) &
                + (T1(k-1)-T1(k-3))*(T2(k-2)-T2(k-4))
          TQ(k,k)   = 1.0
          TQ(k-3,k) = ( (T1(k-2)-T1(k-4))*(T2(k)-T2(k-2)) &
                        - (T2(k-2)-T2(k-4))*(T1(k)-T1(k-2)) )/delta
          TQ(k-4,k) = ( (T1(k-1)-T1(k-3))*(T2(k)-T2(k-2)) &
                        + (T2(k-1)-T2(k-3))*(T1(k)-T1(k-2)) )/delta
          TQ(k-1,k) = -TQ(k-3,k)
          TQ(k-2,k) = -TQ(k-4,k) - 1.0D0
       enddo
       call MessageNotify('M','at_aq_galerkin_RRSS_Initial(Type1)',&
                          'Conversion matrices for SR-B.C. initialized.')

    case default
       call MessageNotify('E','at_aq_galerkin_RRSS_Initial(Type1)',&
                'Argument for B.C. not valid. Should be RR/SS/RS/SR.')

    end select

    beta=1.0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! ξüǴ�������Ѵ��չ���
    alpha=1.0 ; alpha(0)=2.0D0

    QT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             QT(m,n) = QT(m,n) + alpha(l)*TQ(l,m)*TQ(l,n)
          enddo
       enddo
    enddo

    call LUDecomp(QT,kp)

    RRSS_Initialized=.true.

    contains
      !
      ! k �������ӥ�����¿�༰ 2 ����ʬ�� x=1 �Ǥ���
      !
      function T2(k)            
        integer, intent(IN) :: k
        real(8)             :: T2

        T2 = k**2 * (k**2 -1) / 3.0D0
      end function T2
      !
      ! k �������ӥ�����¿�༰ 1 ����ʬ�� x=1 �Ǥ���
      !
      function T1(k)            
        integer, intent(IN) :: k
        real(8)             :: T1

        T1 = k**2
      end function T1

  end subroutine at_aq_galerkin_RRSS_Initial

  !
  ! ξüǴ����
  ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
  !
  function aq_at(at_data)
    real(8), intent(IN) :: at_data(:,0:)                !(in)  �����ӥ����շ���
    real(8)             :: aq_at(size(at_data,1),ks:km) !(out) ���顼���󷸿�  

    real(8)             :: aq_work(size(at_data,1),ks:km)  ! ���������
    
    integer :: k,m

    if ( .not. RRSS_Initialized ) &
         call MessageNotify('E','aq_at',&
                            'at_aq_galerkin_RRSS_module(Type1) not initialized')

    aq_work =0.0
    do m=ks,km
       do k=0,km
          aq_work(:,m) = aq_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TQ(k,m)
       enddo
    enddo

    aq_at = LUSolve(QT,kp,aq_work)
  end function aq_at

  !
  ! ξüǴ����
  ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
  !
  function q_t(t_data)

    real(8), intent(IN) :: t_data(0:km)        !(in)  �����ӥ����շ���
    real(8)             :: q_t(ks:km)          !(out) ���顼���󷸿�  

    real(8)             :: q_work(ks:km)       ! ���������

    integer :: k,m

    if ( .not. RRSS_Initialized ) &
         call MessageNotify('E','q_t',&
                            'at_aq_galerkin_RRSS_module(Type1) not initialized')

    q_work =0.0
    do m=ks,km
       do k=0,km
          q_work(m) = q_work(m) &
               + alpha(k) * beta(k) * t_data(k) * TQ(k,m)
       enddo
    enddo

    q_t = LUSolve(QT,kp,q_work)
  end function q_t

  !
  ! ξüǴ����
  ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(2�����ǡ���)
  !
  function at_aq(aq_data)

    real(8), intent(IN)  :: aq_data(:,ks:)              !(in)  ���顼���󷸿�
    real(8)              :: at_aq(size(aq_data,1),0:km) !(out) �����ӥ����շ���

    integer :: m, n

    if ( .not. RRSS_Initialized ) &
         call MessageNotify('E','at_aq',&
                            'at_aq_galerkin_RRSS_module(Type1) not initialized')

    at_aq = 0.0D0
    do m=0,km
       do n=ks,km
          at_aq(:,m) = at_aq(:,m) + TQ(m,n)*aq_data(:,n)/beta(m)
       enddo
    enddo
  end function at_aq

  !
  ! ξüǴ����
  ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(1�����ǡ���)
  !
  function t_q(q_data)

    real(8), intent(IN)  :: q_data(ks:km)       !(in)  ���顼���󷸿�  
    real(8)              :: t_q(0:km)           !(out) �����ӥ����շ���

    integer :: m, n

    if ( .not. RRSS_Initialized ) &
         call MessageNotify('E','t_q',&
                            'at_aq_galerkin_RRSS_module(Type1) not initialized')

    t_q = 0.0D0
    do m=0,km
       do n=ks,km
          t_q(m) = t_q(m) + TQ(m,n)*q_data(n)/beta(m)
       enddo
    enddo
  end function t_q

  !
  ! ξüǴ����
  ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
  !
  function aq_ag(ag_data)
    real(8), intent(IN)  :: ag_data(:,0:)                !(in)  �ʻ����ǡ���
    real(8)              :: aq_ag(size(ag_data,1),ks:km) !(out) ���顼���󷸿�

    aq_ag = aq_at(at_ag(ag_data))
  end function aq_ag

  !
  ! ξüǴ����
  ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
  !
  function q_g(g_data)
    real(8), intent(IN)  :: g_data(0:im)        !(in)  �ʻ����ǡ���
    real(8)              :: q_g(ks:km)          !(out) ���顼���󷸿�

    q_g = q_t(t_g(g_data))
  end function q_g

  !
  ! ξüǴ����
  ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(2�����ǡ���)
  !
  function ag_aq(aq_data)
    real(8), intent(IN) :: aq_data(:,ks:)              !(in)  ���顼���󷸿�  
    real(8)             :: ag_aq(size(aq_data,1),0:im) !(out) �ʻ����ǡ���  
    
    ag_aq = ag_at(at_aq(aq_data))
  end function ag_aq

  !
  ! ξüǴ����
  ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(1�����ǡ���)
  !
  function g_q(q_data)
    real(8), intent(IN) :: q_data(ks:km)        !(in)  ���顼���󷸿�  
    real(8)             :: g_q(0:im)            !(out) �ʻ����ǡ���  
    
    g_q = g_t(t_q(q_data))
  end function g_q

  !
  ! ξüǴ����
  ! X ��ʬ�׻�(1 ����)
  !
  function aq_Dx_aq(aq_data)
    real(8), intent(IN) :: aq_data(:,ks:)                  !(in) ���顼���󷸿�
    real(8)             :: aq_Dx_aq(size(aq_data,1),ks:km) !(out) ��ʬ���顼����
    aq_Dx_aq = aq_at(at_Dx_at(at_aq(aq_data)))
  end function aq_Dx_aq

  !
  ! ξüǴ����
  ! X ��ʬ�׻�(1 ����)
  !
  function q_Dx_q(q_data)
    real(8), intent(IN) :: q_data(ks:km)
    real(8)             :: q_Dx_q(ks:km)

    q_Dx_q = q_t(t_Dx_t(t_q(q_data)))

  end function q_Dx_q

end module at_aq_galerkin_RRSS
