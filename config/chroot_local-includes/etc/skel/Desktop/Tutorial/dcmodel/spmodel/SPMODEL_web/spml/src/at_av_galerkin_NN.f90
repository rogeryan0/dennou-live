!--
!----------------------------------------------------------------------
!  Copyright (c) 2005-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_av_galerkin_NN1
!
!      �����ӥ����աݥ��顼����ˡ
!      ξü�Υ��ޥ󶭳�����ѥ⥸�塼��
!          f'(:,i=0)=f'(:,i=im)=0
!
!      k ���Υ��顼������줬
!
!        \phi_2(x)=T_0(x)
!        \phi_k(x)=T_k(x) + C_{k-1}T_{k-1}(x) + C_{k-2}T_{k-2}(x) 
!
!      �η���(Type1)
!
!      �꼰���ˤĤ��Ƥϲ���ʸ��
!
!         �֥����ӥ����մؿ�Ÿ�������Ѥ������顼����ˡ��
!
!      (cheb_gal.pdf)�򻲾ȤΤ���. 
!
!����  2005/01/20  �ݹ�����  ��������
!      2006/01/23  �ݹ�����  �⥸�塼��̾�ѹ�
!      2006/01/25  �ݹ�����  �⥸�塼�������ե�å�Ƴ��
!      2006/02/28  �ݹ�����  �����Ȥ� RDoc �Ѥ��ѹ�
!      2009/01/29  ��������ʿ  �����Ȥ� RDoc �Ѥ��ѹ�
!
!++
module at_av_galerkin_NN
  !
  != at_av_galerkin_NN
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: at_av_galerkin_NN.f90,v 1.3 2009-02-28 21:33:46 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !
  !== ����
  !
  ! spml/at_av_galerkin_NN �⥸�塼��ϥ����ӥ����աݥ��顼����ˡ,
  ! ξü�Υ��ޥ󶭳�����ѥ⥸�塼��
  !
  !       f'(:,i=0)=f'(:,i=im)=0
  !
  ! k ���Υ��顼������줬
  !
  !     \phi_2(x)=T_0(x)
  !     \phi_k(x)=T_k(x) + C_{k-1}T_{k-1}(x) + C_{k-2}T_{k-2}(x) 
  !
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
  public :: at_av_galerkin_NN_Initial  ! ��������֥롼����
  public :: av_at, v_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: av_ag, v_g                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: at_av, t_v                 ! ���顼����->�����ӥ������Ѵ�
  public :: ag_av, g_v                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: av_Dx_av, v_Dx_v           ! X ��ʬ

  real(8), allocatable :: TV(:,:)      ! ���顼����->�����ӥ������Ѵ�����
  real(8), allocatable :: VT(:,:)      ! �����ӥ�����->���顼�����Ѵ�����
  integer, allocatable :: kp(:)        ! LU ʬ���ѥԥܥåȳ�Ǽ����

  real(8), allocatable :: alpha(:)     ! �����ӥ�����<->���顼��������ѷ���
  real(8), allocatable :: beta(:)      ! �����ӥ�����<->���顼��������·���

  integer :: im                        ! �ʻ�����              
  integer :: km                        ! �����ӥ����������ȿ�  
  integer,parameter :: ks=2            ! ���顼���������㼡��

  logical :: NN_Initialized=.false.    ! ������ե�å�

  save im, km, TV, VT, kp, alpha, beta, NN_Initialized
  public TV, VT, alpha, beta

contains

  subroutine at_av_galerkin_NN_Initial(i_in,k_in)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�Υ��ޥ󶭳�����ѥ⥸�塼��
    !
    ! ��������֥롼����
    !
    integer, intent(IN) :: i_in     ! (in)�ʻ�����
    integer, intent(IN) :: k_in     ! (in)�����ӥ����������ȿ�  

    integer :: k, l, m, n

    im=i_in ; km=k_in

    allocate(TV(0:km,ks:km),VT(ks:km,ks:km),kp(ks:km))
    allocate(alpha(0:km),beta(0:km))

    ! ξü�ǥ��ꥯ�������Ѵ���������
    TV = 0.0D0
    TV(0,ks) = 1.0D0
    do k=ks+1,km
       TV(k,k) = 1.0D0
       TV(k-2,k) = -1.0D0*k**2/(k-2)**2
    enddo

    beta=1.0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! ξü�ǥ��ꥯ�������Ѵ��չ���
    alpha=1.0 ; alpha(0)=2.0D0

    VT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             VT(m,n) = VT(m,n) + alpha(l)*TV(l,m)*TV(l,n)
          enddo
       enddo
    enddo

    call LUDecomp(VT,kp)

    call MessageNotify('M','at_av_galerkin_NN_Initial',&
                           'Conversion matrices initialized')

    NN_Initialized=.true.

  end subroutine at_av_galerkin_NN_Initial

  function av_at(at_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�Υ��ޥ󶭳����
    !
    ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
    !
    real(8), intent(IN) :: at_data(:,0:)                !(in)  �����ӥ����շ���
    real(8)             :: av_at(size(at_data,1),ks:km) !(out) ���顼���󷸿�  

    real(8)             :: av_work(size(at_data,1),ks:km)  ! ���������
    
    integer :: k,m

    if ( .not. NN_Initialized ) &
         call MessageNotify('E','av_at',&
                            'at_av_galerkin_NN_module not initialized')

    av_work =0.0
    do m=ks,km
       do k=0,km
          av_work(:,m) = av_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TV(k,m)
       enddo
    enddo

    av_at = LUSolve(VT,kp,av_work)

  end function av_at

  function v_t(t_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�Υ��ޥ󶭳����
    !
    ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
    !
    real(8), intent(IN) :: t_data(0:km)        !(in)  �����ӥ����շ���
    real(8)             :: v_t(ks:km)          !(out) ���顼���󷸿�  

    real(8)             :: v_work(ks:km)       ! ���������

    integer :: k,m

    if ( .not. NN_Initialized ) &
         call MessageNotify('E','v_t',&
                            'at_av_galerkin_NN_module not initialized')

    v_work =0.0
    do m=ks,km
       do k=0,km
          v_work(m) = v_work(m) &
               + alpha(k) * beta(k) * t_data(k) * TV(k,m)
       enddo
    enddo

    v_t = LUSolve(VT,kp,v_work)

  end function v_t

  function at_av(av_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�Υ��ޥ󶭳����
    !
    ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(2�����ǡ���)
    !
    real(8), intent(IN)  :: av_data(:,ks:)              !(in)  ���顼���󷸿�
    real(8)              :: at_av(size(av_data,1),0:km) !(out) �����ӥ����շ���
    integer :: m, n

    if ( .not. NN_Initialized ) &
         call MessageNotify('E','at_av',&
                            'at_av_galerkin_NN_module not initialized')

    at_av = 0.0D0
    do m=0,km
       do n=ks,km
          at_av(:,m) = at_av(:,m) + TV(m,n)*av_data(:,n)/beta(m)
       enddo
    enddo

  end function at_av

  function t_v(v_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�Υ��ޥ󶭳����
    !
    ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(1�����ǡ���)
    !
    real(8), intent(IN)  :: v_data(ks:km)       !(in)  ���顼���󷸿�  
    real(8)              :: t_v(0:km)           !(out) �����ӥ����շ���

    integer :: m, n

    if ( .not. NN_Initialized ) &
         call MessageNotify('E','t_v',&
                            'at_av_galerkin_NN_module not initialized')

    t_v = 0.0D0
    do m=0,km
       do n=ks,km
          t_v(m) = t_v(m) + TV(m,n)*v_data(n)/beta(m)
       enddo
    enddo

  end function t_v

  function av_ag(ag_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�Υ��ޥ󶭳����
    !
    ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
    !
    real(8), intent(IN)  :: ag_data(:,0:)                !(in)  �ʻ����ǡ���
    real(8)              :: av_ag(size(ag_data,1),ks:km) !(out) ���顼���󷸿�

    av_ag = av_at(at_ag(ag_data))
  end function av_ag

  function v_g(g_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�Υ��ޥ󶭳����
    !
    ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
    !
    real(8), intent(IN)  :: g_data(0:im)        !(in)  �ʻ����ǡ���
    real(8)              :: v_g(ks:km)          !(out) ���顼���󷸿�

    v_g = v_t(t_g(g_data))
  end function v_g

  function ag_av(av_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�Υ��ޥ󶭳����
    !
    ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(2�����ǡ���)
    !
    real(8), intent(IN) :: av_data(:,ks:)              !(in)  ���顼���󷸿�  
    real(8)             :: ag_av(size(av_data,1),0:im) !(out) �ʻ����ǡ���  
    
    ag_av = ag_at(at_av(av_data))
  end function ag_av

  function g_v(v_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�Υ��ޥ󶭳����
    !
    ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(1�����ǡ���)
    !
    real(8), intent(IN) :: v_data(ks:km)        !(in)  ���顼���󷸿�  
    real(8)             :: g_v(0:im)            !(out) �ʻ����ǡ���  
    
    g_v = g_t(t_v(v_data))
  end function g_v

  function av_Dx_av(av_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�Υ��ޥ󶭳����
    !
    ! X ��ʬ�׻�(1 ����)
    !
    real(8), intent(IN) :: av_data(:,ks:)                  !(in) ���顼���󷸿�
    real(8)             :: av_Dx_av(size(av_data,1),ks:km) !(out) ��ʬ���顼����
    av_Dx_av = av_at(at_Dx_at(at_av(av_data)))
  end function av_Dx_av

  function v_Dx_v(v_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�Υ��ޥ󶭳����
    !
    ! X ��ʬ�׻�(1 ����)
    !
    real(8), intent(IN) :: v_data(ks:km)
    real(8)             :: v_Dx_v(ks:km)

    v_Dx_v = v_t(t_Dx_t(t_v(v_data)))

  end function v_Dx_v

end module at_av_galerkin_NN
