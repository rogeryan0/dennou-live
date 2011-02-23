!--
!----------------------------------------------------------------------
! COPYRIGHT (c) 2006-2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_aq_galerkin_RRFF1
!
!      �����ӥ����աݥ��顼����ˡ
!      �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
!
!      case RR: ξüǴ����(ξü���ͤ� 1 ����ʬ�� 0)
!               f(:,i=0)=f(:,i=im)=f'(:,i=0)=f'(:,i=im)=0 
!               [ f(:,x=xmax)=f(:,x=xmin)=f'(:,x=xmax)=0=f'(:,x=xmin)=0 ]
!
!      case FF: ξü��ͳ���٤���(ξü���ͤ� 2 ����ʬ�� 0)
!               f(:,i=0)=f(:,i=im)=f''(:,i=0)=f''(:,i=im)=0 
!               [ f(:,x=xmax)=f(:,x=xmin)=f''(:,x=xmax)=0=f''(:,x=xmin)=0 ]
!
!      case RF: ��üǴ����, ��ü��ͳ���٤���
!               (ξü���ͤ� 0, ��¦�� 1 ����ʬ, �⤦������ 2 ����ʬ�� 0)
!               f(:,i=0)=f(:,i=im)=f'(:,i=0)=f''(:,i=im)=0 
!               [ f(:,x=xmax)=f(:,x=xmin)=f''(:,x=xmax)=0=f'(:,x=xmin)=0 ]
!
!      case FR: ��ü��ͳ���٤���, ��üǴ����
!               (ξü���ͤ� 0, ��¦�� 2 ����ʬ, �⤦������ 1 ����ʬ�� 0)
!               f(:,i=0)=f(:,i=im)=f''(:,i=0)=f'(:,i=im)=0 
!               [ f(:,x=xmax)=f(:,x=xmin)=f'(:,x=xmax)=0=f''(:,x=xmin)=0 ]
!
!      k ���Υ��顼������줬
!        \phi_k(x)=T_k(x) + C_{k-1}T_{k-1}(x) + C_{k-2}T_{k-2}(x) 
!                         + C_{k-3}T_{k-3}(x) + C_{k-4}T_{k-4}(x) 
!      �η���(Type1)
!
!      �꼰���ˤĤ��Ƥϲ���ʸ��
!
!         �֥����ӥ����մؿ�Ÿ�������Ѥ������顼����ˡ��
!
!      (cheb_gal.pdf)�򻲾ȤΤ���. 
!
!����  2006/01/24  �ݹ�����  ��������
!      2006/01/25  �ݹ�����  �⥸�塼�������ե�å�Ƴ��
!      2006/02/28  �ݹ�����  �����Ȥ� RDoc �Ѥ��ѹ�
!      2009/07/22  ��������ʿ at_module �ȶ�����凉���å���·����
!
!++
module at_aq_galerkin_RRFF
  !
  != at_aq_galerkin_RRFF
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: at_aq_galerkin_RRFF.f90,v 1.1 2009-07-22 03:35:11 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/at_aq_galerkin_RRFF �ϥ����ӥ����աݥ��顼����ˡ,
  ! �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����Ф���,
  !
  ! * case RR: ξüǴ����(ξü���ͤ� 1 ����ʬ�� 0)
  !            f(:,i=0)=f(:,i=im)=f'(:,i=0)=f'(:,i=im)=0 
  !            [ f(:,x=xmax)=f(:,x=xmin)=f'(:,x=xmax)=0=f'(:,x=xmin)=0 ]
  !
  ! * case FF: ξü��ͳ���٤���(ξü���ͤ� 2 ����ʬ�� 0)
  !            f(:,i=0)=f(:,i=im)=f''(:,i=0)=f''(:,i=im)=0 
  !            [ f(:,x=xmax)=f(:,x=xmin)=f''(:,x=xmax)=0=f''(:,x=xmin)=0 ]
  !
  ! * case RF: ��üǴ����, ��ü��ͳ���٤���
  !            (ξü���ͤ� 0, ��¦�� 1 ����ʬ, �⤦������ 2 ����ʬ�� 0)
  !            f(:,i=0)=f(:,i=im)=f'(:,i=0)=f''(:,i=im)=0 
  !            [ f(:,x=xmax)=f(:,x=xmin)=f''(:,x=xmax)=0=f'(:,x=xmin)=0 ]
  !
  ! * case FR: ��ü��ͳ���٤���, ��üǴ����
  !            (ξü���ͤ� 0, ��¦�� 2 ����ʬ, �⤦������ 1 ����ʬ�� 0)
  !            f(:,i=0)=f(:,i=im)=f''(:,i=0)=f'(:,i=im)=0 
  !            [ f(:,x=xmax)=f(:,x=xmin)=f'(:,x=xmax)=0=f''(:,x=xmin)=0 ]
  !
  ! �� k ���Υ��顼������줬
  !     \phi_k(x)=T_k(x) + C_{k-1}T_{k-1}(x) + C_{k-2}T_{k-2}(x) 
  !                      + C_{k-3}T_{k-3}(x) + C_{k-4}T_{k-4}(x) 
  ! �η���(Type1)���Ѥ��Ʒ׻����뤿��Υ⥸�塼��Ǥ���.
  !
  ! �꼰���ˤĤ��Ƥϲ���ʸ��֥����ӥ����մؿ�Ÿ�������Ѥ������顼����ˡ��
  ! (cheb_gal.pdf[link:link:cheb_gal.pdf])�򻲾ȤΤ���. 
  !
  use lumatrix
  use at_module
  use dc_message

  implicit none

  private
  public :: at_aq_galerkin_RRFF_Initial  ! ��������֥롼����
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
  character(LEN=2) :: bc            ! �������(RR/FF/RF/FR)

  logical :: RRFF_Initialized=.false.    ! ������ե�å�

  save im, km, bc, TQ, QT, kp, alpha, beta, RRFF_Initialized
  public TQ, QT, alpha, beta

contains

  subroutine at_aq_galerkin_RRFF_Initial(i_in,k_in,bc_in)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
    !
    ! ��������֥롼����
    !
    integer, intent(in)          :: i_in     
    ! (in)�ʻ�����
    integer, intent(in)          :: k_in     
    ! (in)�����ӥ����������ȿ�  
    character(LEN=2), intent(in) :: bc_in    
    ! (in)�������(RR/FF/RF/FR)

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
          TQ(k,k)   = 1.0D0
          TQ(k-2,k) = -2.0*(K-2.0D0)/(K-3.0D0)
          TQ(k-4,k) = (K-1.0D0)/(K-3.0D0)
       enddo
       call MessageNotify('M','at_aq_galerkin_RRFF_Initial',&
                          'Conversion matrices for RR-B.C. initialized.')

    !---- ξü��ͳ���٤������Ѵ���������
    case ('FF','ff','SS','ss')
       TQ = 0.0D0
       do k=ks,km
          TQ(k,k)   = 1.0D0
          TQ(k-2,k) = -(T2(k)-T2(k-4))/(T2(k-2)-T2(k-4))
          TQ(k-4,k) = (T2(k)-T2(k-2))/(T2(k-2)-T2(k-4))
       enddo
       call MessageNotify('M','at_aq_galerkin_RRFF_Initial',&
                          'Conversion matrices for FF-B.C. initialized.')


    !---- ��üǴ����, ��ü��ͳ���٤����Ѵ���������
    case ('RF','rf','RS','rs')
       TQ = 0.0D0
       do k=ks,km
          delta = (T1(k-1)-T1(k-3))*(T2(k-2)-T2(k-4)) &
                + (T2(k-1)-T2(k-3))*(T1(k-2)-T1(k-4))
          TQ(k,k)   = 1.0D0
          TQ(k-3,k) = ( (T2(k-2)-T2(k-4))*(T1(k)-T1(k-2)) &
                        - (T1(k-2)-T1(k-4))*(T2(k)-T2(k-2)) )/delta
          TQ(k-4,k) = ( (T2(k-1)-T2(k-3))*(T1(k)-T1(k-2)) &
                        + (T1(k-1)-T1(k-3))*(T2(k)-T2(k-2)) )/delta
          TQ(k-1,k) = -TQ(k-3,k)
          TQ(k-2,k) = -TQ(k-4,k) - 1.0D0
       enddo
       call MessageNotify('M','at_aq_galerkin_RRFF_Initial',&
                          'Conversion matrices for RF-B.C. initialized.')

    !---- ��ü��ͳ���٤���, ��üǴ�����Ѵ���������
    case ('FR','fr','SR','sr')
       TQ = 0.0D0
       do k=ks,km
          delta = (T2(k-1)-T2(k-3))*(T1(k-2)-T1(k-4)) &
                + (T1(k-1)-T1(k-3))*(T2(k-2)-T2(k-4))
          TQ(k,k)   = 1.0D0
          TQ(k-3,k) = ( (T1(k-2)-T1(k-4))*(T2(k)-T2(k-2)) &
                        - (T2(k-2)-T2(k-4))*(T1(k)-T1(k-2)) )/delta
          TQ(k-4,k) = ( (T1(k-1)-T1(k-3))*(T2(k)-T2(k-2)) &
                        + (T2(k-1)-T2(k-3))*(T1(k)-T1(k-2)) )/delta
          TQ(k-1,k) = -TQ(k-3,k)
          TQ(k-2,k) = -TQ(k-4,k) - 1.0D0
       enddo
       call MessageNotify('M','at_aq_galerkin_RRFF_Initial',&
                          'Conversion matrices for FR-B.C. initialized.')

    case default
       call MessageNotify('E','at_aq_galerkin_RRFF_Initial',&
                'Argument for B.C. not valid. Should be RR/FF/RF/FR.')

    end select

    beta=1.0D0 ; beta(0)=0.5D0
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

    RRFF_Initialized=.true.

    contains
      !
      ! k �������ӥ�����¿�༰ 2 ����ʬ�� x=1 �Ǥ���
      !
      function T2(k)            
        integer, intent(in) :: k
        real(8)             :: T2

        T2 = k**2 * (k**2 -1.0D0) / 3.0D0
      end function T2
      !
      ! k �������ӥ�����¿�༰ 1 ����ʬ�� x=1 �Ǥ���
      !
      function T1(k)            
        integer, intent(in) :: k
        real(8)             :: T1

        T1 = k**2.0D0
      end function T1

  end subroutine at_aq_galerkin_RRFF_Initial

  function aq_at(at_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
    !
    ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
    !
    real(8), intent(in) :: at_data(:,0:)                
    !(in)  �����ӥ����շ���
    real(8)             :: aq_at(size(at_data,1),ks:km) 
    !(out) ���顼���󷸿�  

    real(8)             :: aq_work(size(at_data,1),ks:km)  
    ! ���������
    
    integer :: k,m

    if ( .not. RRFF_Initialized ) &
         call MessageNotify('E','aq_at',&
                            'at_aq_galerkin_RRFF_module not initialized')

    aq_work = 0.0D0
    do m=ks,km
       do k=0,km
          aq_work(:,m) = aq_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TQ(k,m)
       enddo
    enddo

    aq_at = LUSolve(QT,kp,aq_work)
  end function aq_at

  function q_t(t_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
    !
    ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
    !
    real(8), intent(in) :: t_data(0:km)        
    !(in)  �����ӥ����շ���
    real(8)             :: q_t(ks:km)          
    !(out) ���顼���󷸿�  

    real(8)             :: q_work(ks:km)       
    ! ���������

    integer :: k,m

    if ( .not. RRFF_Initialized ) &
         call MessageNotify('E','q_t',&
                            'at_aq_galerkin_RRFF_module not initialized')

    q_work =0.0D0
    do m=ks,km
       do k=0,km
          q_work(m) = q_work(m) &
               + alpha(k) * beta(k) * t_data(k) * TQ(k,m)
       enddo
    enddo

    q_t = LUSolve(QT,kp,q_work)
  end function q_t

  function at_aq(aq_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
    !
    ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(2�����ǡ���)
    !
    real(8), intent(in)  :: aq_data(:,ks:)              
    !(in)  ���顼���󷸿�
    real(8)              :: at_aq(size(aq_data,1),0:km) 
    !(out) �����ӥ����շ���

    integer :: m, n

    if ( .not. RRFF_Initialized ) &
         call MessageNotify('E','at_aq',&
                            'at_aq_galerkin_RRFF_module not initialized')

    at_aq = 0.0D0
    do m=0,km
       do n=ks,km
          at_aq(:,m) = at_aq(:,m) + TQ(m,n)*aq_data(:,n)/beta(m)
       enddo
    enddo
  end function at_aq

  function t_q(q_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
    !
    ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(1�����ǡ���)
    !
    real(8), intent(in)  :: q_data(ks:km)       
    !(in)  ���顼���󷸿�  
    real(8)              :: t_q(0:km)           
    !(out) �����ӥ����շ���

    integer :: m, n

    if ( .not. RRFF_Initialized ) &
         call MessageNotify('E','t_q',&
                            'at_aq_galerkin_RRFF_module not initialized')

    t_q = 0.0D0
    do m=0,km
       do n=ks,km
          t_q(m) = t_q(m) + TQ(m,n)*q_data(n)/beta(m)
       enddo
    enddo
  end function t_q

  function aq_ag(ag_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
    !
    ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
    !
    real(8), intent(in)  :: ag_data(:,0:)                
    !(in)  �ʻ����ǡ���
    real(8)              :: aq_ag(size(ag_data,1),ks:km) 
    !(out) ���顼���󷸿�

    aq_ag = aq_at(at_ag(ag_data))
  end function aq_ag

  function q_g(g_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
    !
    ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
    !
    real(8), intent(in)  :: g_data(0:im)        
    !(in)  �ʻ����ǡ���
    real(8)              :: q_g(ks:km)          
    !(out) ���顼���󷸿�

    q_g = q_t(t_g(g_data))
  end function q_g

  function ag_aq(aq_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
    !
    ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(2�����ǡ���)
    !
    real(8), intent(in) :: aq_data(:,ks:)              
    !(in)  ���顼���󷸿�  
    real(8)             :: ag_aq(size(aq_data,1),0:im) 
    !(out) �ʻ����ǡ���  
    
    ag_aq = ag_at(at_aq(aq_data))
  end function ag_aq

  function g_q(q_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
    !
    ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(1�����ǡ���)
    !
    real(8), intent(in) :: q_data(ks:km)        
    !(in)  ���顼���󷸿�  
    real(8)             :: g_q(0:im)            
    !(out) �ʻ����ǡ���  
    
    g_q = g_t(t_q(q_data))
  end function g_q

  function aq_Dx_aq(aq_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
    !
    ! X ��ʬ�׻�(2 ����)
    !
    real(8), intent(in) :: aq_data(:,ks:)                  
    !(in) ���顼���󷸿�
    real(8)             :: aq_Dx_aq(size(aq_data,1),ks:km) 
    !(out) ��ʬ���顼����
    aq_Dx_aq = aq_at(at_Dx_at(at_aq(aq_data)))
  end function aq_Dx_aq

  function q_Dx_q(q_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! �󰵽�ή�Τ�ή���ؿ���ή��ݥƥ󥷥����
    !
    ! X ��ʬ�׻�(1 ����)
    !
    real(8), intent(in) :: q_data(ks:km)
    real(8)             :: q_Dx_q(ks:km)

    q_Dx_q = q_t(t_Dx_t(t_q(q_data)))

  end function q_Dx_q

end module at_aq_galerkin_RRFF
