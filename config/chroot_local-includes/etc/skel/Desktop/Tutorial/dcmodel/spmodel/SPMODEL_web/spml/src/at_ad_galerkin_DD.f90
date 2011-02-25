!--
!----------------------------------------------------------------------
!  Copyright (c) 2005-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_ad_galerkin_DD1
!
!      �����ӥ����աݥ��顼����ˡξü�ǥ��ꥯ�춭������ѥ⥸�塼��
!        f(:,i=0)=f(:,i=im)=0
!
!      k ���Υ��顼������줬
!
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
module at_ad_galerkin_DD
  !
  != at_ad_galerkin_DD
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: at_ad_galerkin_DD.f90,v 1.3 2009-02-28 21:33:46 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/at_ad_galerkin_DD �⥸�塼��ϥ����ӥ����աݥ��顼����ˡ,
  ! ξü�ǥ��ꥯ�춭�����
  !
  !      f(:,i=0)=f(:,i=im)=0
  !
  ! ��  k ���Υ��顼������줬
  !
  !    \phi_k(x)=T_k(x) + C_{k-1}T_{k-1}(x) + C_{k-2}T_{k-2}(x) 
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
  public :: at_ad_galerkin_DD_Initial  ! ��������֥롼����
  public :: ad_at, d_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: ad_ag, d_g                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: at_ad, t_d                 ! ���顼����->�����ӥ������Ѵ�
  public :: ag_ad, g_d                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: ad_Dx_ad, d_Dx_d           ! X ��ʬ

  real(8), allocatable :: TD(:,:)      ! ���顼����->�����ӥ������Ѵ�����
  real(8), allocatable :: DT(:,:)      ! �����ӥ�����->���顼�����Ѵ�����
  integer, allocatable :: kp(:)        ! LU ʬ���ѥԥܥåȳ�Ǽ����

  real(8), allocatable :: alpha(:)     ! �����ӥ�����<->���顼��������ѷ���
  real(8), allocatable :: beta(:)      ! �����ӥ�����<->���顼��������·���

  integer :: im                        ! �ʻ�����              
  integer :: km                        ! �����ӥ����������ȿ�  
  integer,parameter :: ks=2            ! ���顼���������㼡��

  logical :: DD_Initialized=.false.    ! ������ե�å�

  save im, km, TD, DT, kp, alpha, beta, DD_Initialized
  public TD, DT, alpha, beta

contains

  subroutine at_ad_galerkin_DD_Initial(i_in,k_in)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�ǥ��ꥯ�춭������ѥ⥸�塼��
    !
    ! ��������֥롼����
    !

    integer, intent(IN) :: i_in     ! (in)�ʻ�����
    integer, intent(IN) :: k_in     ! (in)�����ӥ����������ȿ�  

    integer :: k, l, m, n

    im=i_in ; km=k_in

    allocate(TD(0:km,ks:km),DT(ks:km,ks:km),kp(ks:km))
    allocate(alpha(0:km),beta(0:km))

    ! ξü�ǥ��ꥯ�������Ѵ���������
    TD = 0.0D0
    do k=ks,km
       TD(k,k) = 1.0D0
       TD(k-2,k) = -1.0D0
    enddo

    beta=1.0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! ξü�ǥ��ꥯ�������Ѵ��չ���
    alpha=1.0 ; alpha(0)=2.0D0

    DT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             DT(m,n) = DT(m,n) + alpha(l)*TD(l,m)*TD(l,n)
          enddo
       enddo
    enddo

    call LUDecomp(DT,kp)

    call MessageNotify('M','at_ad_galerkin_DD_Initial',&
                           'Conversion matrices initialized')

    DD_Initialized = .true.

  end subroutine at_ad_galerkin_DD_Initial

  function ad_at(at_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�ǥ��ꥯ�춭�����
    !
    ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
    !
    real(8), intent(IN) :: at_data(:,0:)                !(in)  �����ӥ����շ���
    real(8)             :: ad_at(size(at_data,1),ks:km) !(out) ���顼���󷸿�  

    real(8)             :: ad_work(size(at_data,1),ks:km)  ! ���������
    
    integer :: k,m

    if ( .not. DD_Initialized ) &
         call MessageNotify('E','ad_at',&
                            'at_ad_galerkin_DD_module not initialized')

    ad_work =0.0
    do m=ks,km
       do k=0,km
          ad_work(:,m) = ad_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TD(k,m)
       enddo
    enddo

    ad_at = LUSolve(DT,kp,ad_work)

  end function ad_at

  function d_t(t_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�ǥ��ꥯ�춭�����
    !
    ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
    !

    real(8), intent(IN) :: t_data(0:km)        !(in)  �����ӥ����շ���
    real(8)             :: d_t(ks:km)          !(out) ���顼���󷸿�  

    real(8)             :: d_work(ks:km)       ! ���������

    integer :: k,m

    if ( .not. DD_Initialized ) &
         call MessageNotify('E','d_t',&
                            'at_ad_galerkin_DD_module not initialized')

    d_work =0.0
    do m=ks,km
       do k=0,km
          d_work(m) = d_work(m) &
               + alpha(k) * beta(k) * t_data(k) * TD(k,m)
       enddo
    enddo

    d_t = LUSolve(DT,kp,d_work)

  end function d_t

  function at_ad(ad_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�ǥ��ꥯ�춭�����
    !
    ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(2�����ǡ���)
    !

    real(8), intent(IN)  :: ad_data(:,ks:)              !(in)  ���顼���󷸿�
    real(8)              :: at_ad(size(ad_data,1),0:km) !(out) �����ӥ����շ���
    integer :: m, n

    if ( .not. DD_Initialized ) &
         call MessageNotify('E','at_ad',&
                            'at_ad_galerkin_DD_module not initialized')

    at_ad = 0.0D0
    do m=0,km
       do n=ks,km
          at_ad(:,m) = at_ad(:,m) + TD(m,n)*ad_data(:,n)/beta(m)
       enddo
    enddo

  end function at_ad

  function t_d(d_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�ǥ��ꥯ�춭�����
    !
    ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(1�����ǡ���)
    !
    real(8), intent(IN)  :: d_data(ks:km)       !(in)  ���顼���󷸿�  
    real(8)              :: t_d(0:km)           !(out) �����ӥ����շ���

    integer :: m, n

    if ( .not. DD_Initialized ) &
         call MessageNotify('E','t_d',&
                            'at_ad_galerkin_DD_module not initialized')

    t_d = 0.0D0
    do m=0,km
       do n=ks,km
          t_d(m) = t_d(m) + TD(m,n)*d_data(n)/beta(m)
       enddo
    enddo

  end function t_d

  function ad_ag(ag_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�ǥ��ꥯ�춭�����
    !
    ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
    !

    real(8), intent(IN)  :: ag_data(:,0:)                !(in)  �ʻ����ǡ���
    real(8)              :: ad_ag(size(ag_data,1),ks:km) !(out) ���顼���󷸿�

    ad_ag = ad_at(at_ag(ag_data))
  end function ad_ag

  function d_g(g_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�ǥ��ꥯ�춭�����
    !
    ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
    !
    real(8), intent(IN)  :: g_data(0:im)        !(in)  �ʻ����ǡ���
    real(8)              :: d_g(ks:km)          !(out) ���顼���󷸿�

    d_g = d_t(t_g(g_data))
  end function d_g

  function ag_ad(ad_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�ǥ��ꥯ�춭�����
    !
    ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(2�����ǡ���)
    !
    real(8), intent(IN) :: ad_data(:,ks:)              !(in)  ���顼���󷸿�  
    real(8)             :: ag_ad(size(ad_data,1),0:im) !(out) �ʻ����ǡ���  
    
    ag_ad = ag_at(at_ad(ad_data))
  end function ag_ad

  function g_d(d_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�ǥ��ꥯ�춭�����
    !
    ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(1�����ǡ���)
    !
    real(8), intent(IN) :: d_data(ks:km)        !(in)  ���顼���󷸿�  
    real(8)             :: g_d(0:im)            !(out) �ʻ����ǡ���  
    
    g_d = g_t(t_d(d_data))
  end function g_d

  function ad_Dx_ad(ad_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�ǥ��ꥯ�춭�����
    !
    ! X ��ʬ�׻�(2 �����ǡ���)
    !
    real(8), intent(IN) :: ad_data(:,ks:)                  !(in) ���顼���󷸿�
    real(8)             :: ad_Dx_ad(size(ad_data,1),ks:km) !(out) ��ʬ���顼����
    ad_Dx_ad = ad_at(at_Dx_at(at_ad(ad_data)))
  end function ad_Dx_ad

  function d_Dx_d(d_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ξü�ǥ��ꥯ�춭�����
    !
    ! X ��ʬ�׻�(1 �����ǡ���)
    !
    real(8), intent(IN) :: d_data(ks:km)
    real(8)             :: d_Dx_d(ks:km)

    d_Dx_d = d_t(t_Dx_t(t_d(d_data)))

  end function d_Dx_d

end module at_ad_galerkin_DD