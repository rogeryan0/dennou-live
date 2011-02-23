!--
!----------------------------------------------------------------------
!   COPYRIGHT (c) 2005-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_ap_galerkin_DN1
!
!      �����ӥ����աݥ��顼����ˡ
!      ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳�����ѥ⥸�塼��
!          f(:,i=0)=f'(:,i=im)=0 [ f(:,x=xmax)=f'(:,x=xmin)=0 ]
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
!����  2006/01/20  �ݹ�����  ��������
!      2006/01/23  �ݹ�����  �⥸�塼��̾�ѹ�
!      2006/01/25  �ݹ�����  �⥸�塼�������ե�å�Ƴ��
!      2006/02/28  �ݹ�����  �����Ȥ� RDoc �Ѥ��ѹ�
!      2009/01/29  ��������ʿ  �����Ȥ� RDoc �Ѥ��ѹ�
!
!++
module at_ap_galerkin_DN
  !
  != at_ap_galerkin_DN
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: at_ap_galerkin_DN.f90,v 1.3 2009-02-28 21:33:46 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/at_ap_galerkin_DN �ϥ����ӥ����աݥ��顼����ˡ, 
  ! ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳����
  !   
  !       f(:,i=0)=f'(:,i=im)=0 [ f(:,x=xmax)=f'(:,x=xmin)=0 ]
  !
  ! ��  k ���Υ��顼������줬
  !
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
  public :: at_ap_galerkin_DN_Initial  ! ��������֥롼����
  public :: ap_at, p_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: ap_ag, p_g                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: at_ap, t_p                 ! ���顼����->�����ӥ������Ѵ�
  public :: ag_ap, g_p                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: ap_Dx_ap, p_Dx_p           ! X ��ʬ

  real(8), allocatable :: TP(:,:)      ! ���顼����->�����ӥ������Ѵ�����
  real(8), allocatable :: PT(:,:)      ! �����ӥ�����->���顼�����Ѵ�����
  integer, allocatable :: kp(:)        ! LU ʬ���ѥԥܥåȳ�Ǽ����

  real(8), allocatable :: alpha(:)     ! �����ӥ�����<->���顼��������ѷ���
  real(8), allocatable :: beta(:)      ! �����ӥ�����<->���顼��������·���

  integer :: im                        ! �ʻ�����              
  integer :: km                        ! �����ӥ����������ȿ�  
  integer,parameter :: ks=2            ! ���顼���������㼡��

  logical :: DN_Initialized=.false.    ! ������ե�å�

  save im, km, TP, PT, kp, alpha, beta, DN_Initialized
  public TP, PT, alpha, beta

contains

  subroutine at_ap_galerkin_DN_Initial(i_in,k_in)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳�����ѥ⥸�塼��
    !
    ! ��������֥롼����
    !
    integer, intent(IN) :: i_in     ! (in)�ʻ�����
    integer, intent(IN) :: k_in     ! (in)�����ӥ����������ȿ�  

    integer :: k, l, m, n

    im=i_in ; km=k_in

    allocate(TP(0:km,ks:km),PT(ks:km,ks:km),kp(ks:km))
    allocate(alpha(0:km),beta(0:km))

    ! ��ü�ǥ��ꥯ����ü�Υ��ޥ������Ѵ���������
    TP = 0.0D0
    do k=ks,km
       TP(k,k) = 1.0D0
       TP(k-1,k) =  1.0D0*(k**2-(k-2)**2)/((k-1)**2+(k-2)**2)
       TP(k-2,k) = -1.0D0*(k**2+(k-1)**2)/((k-1)**2+(k-2)**2)
    enddo

    beta=1.0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! ��ü�ǥ��ꥯ����ü�Υ��ޥ������Ѵ��չ���
    alpha=1.0 ; alpha(0)=2.0D0

    PT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             PT(m,n) = PT(m,n) + alpha(l)*TP(l,m)*TP(l,n)
          enddo
       enddo
    enddo

    call LUDecomp(PT,kp)

    call MessageNotify('M','at_ap_galerkin_DN_Initial',&
                           'Conversion matrices initialized')

    DN_Initialized=.true.

  end subroutine at_ap_galerkin_DN_Initial

  function ap_at(at_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳����
    !
    ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
    !
    real(8), intent(IN) :: at_data(:,0:)                !(in)  �����ӥ����շ���
    real(8)             :: ap_at(size(at_data,1),ks:km) !(out) ���顼���󷸿�  

    real(8)             :: ap_work(size(at_data,1),ks:km)  ! ���������

    integer :: k,m

    if ( .not. DN_Initialized ) &
         call MessageNotify('E','ap_at',&
                            'at_ap_galerkin_DN_module not initialized')

    ap_work =0.0
    do m=ks,km
       do k=0,km
          ap_work(:,m) = ap_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TP(k,m)
       enddo
    enddo

    ap_at = LUSolve(PT,kp,ap_work)
  end function ap_at

  function p_t(t_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳����
    !
    ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
    !
    real(8), intent(IN) :: t_data(0:km)        !(in)  �����ӥ����շ���
    real(8)             :: p_t(ks:km)          !(out) ���顼���󷸿�  

    real(8)             :: p_work(ks:km)       ! ���������

    integer :: k,m

    if ( .not. DN_Initialized ) &
         call MessageNotify('E','p_t',&
                            'at_ap_galerkin_DN_module not initialized')

    p_work =0.0
    do m=ks,km
       do k=0,km
          p_work(m) = p_work(m) &
               + alpha(k) * beta(k) * t_data(k) * TP(k,m)
       enddo
    enddo

    p_t = LUSolve(PT,kp,p_work)
  end function p_t

  function at_ap(ap_data)
    !
    ! ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳����
    ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(2�����ǡ���)
    !
    real(8), intent(IN)  :: ap_data(:,ks:)              !(in)  ���顼���󷸿�
    real(8)              :: at_ap(size(ap_data,1),0:km) !(out) �����ӥ����շ���

    integer :: m, n

    if ( .not. DN_Initialized ) &
         call MessageNotify('E','at_ap',&
                            'at_ap_galerkin_DN_module not initialized')

    at_ap = 0.0D0
    do m=0,km
       do n=ks,km
          at_ap(:,m) = at_ap(:,m) + TP(m,n)*ap_data(:,n)/beta(m)
       enddo
    enddo

  end function at_ap

  function t_p(p_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳����
    !
    ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(1�����ǡ���)
    !
    real(8), intent(IN)  :: p_data(ks:km)       !(in)  ���顼���󷸿�  
    real(8)              :: t_p(0:km)           !(out) �����ӥ����շ���

    integer :: m, n

    if ( .not. DN_Initialized ) &
         call MessageNotify('E','t_p',&
                            'at_ap_galerkin_DN_module not initialized')

    t_p = 0.0D0
    do m=0,km
       do n=ks,km
          t_p(m) = t_p(m) + TP(m,n)*p_data(n)/beta(m)
       enddo
    enddo

  end function t_p

  function ap_ag(ag_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳����
    !
    ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
    !
    real(8), intent(IN)  :: ag_data(:,0:)                !(in)  �ʻ����ǡ���
    real(8)              :: ap_ag(size(ag_data,1),ks:km) !(out) ���顼���󷸿�

    ap_ag = ap_at(at_ag(ag_data))
  end function ap_ag

  function p_g(g_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳����
    !
    ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
    !
    real(8), intent(IN)  :: g_data(0:im)        !(in)  �ʻ����ǡ���
    real(8)              :: p_g(ks:km)          !(out) ���顼���󷸿�

    p_g = p_t(t_g(g_data))
  end function p_g

  function ag_ap(ap_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳����
    !
    ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(2�����ǡ���)
    !
    real(8), intent(IN) :: ap_data(:,ks:)              !(in)  ���顼���󷸿�  
    real(8)             :: ag_ap(size(ap_data,1),0:im) !(out) �ʻ����ǡ���  
    
    ag_ap = ag_at(at_ap(ap_data))
  end function ag_ap

  function g_p(p_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳����
    !
    ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(1�����ǡ���)
    !
    real(8), intent(IN) :: p_data(ks:km)        !(in)  ���顼���󷸿�  
    real(8)             :: g_p(0:im)            !(out) �ʻ����ǡ���  
    
    g_p = g_t(t_p(p_data))
  end function g_p

  function ap_Dx_ap(ap_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳����
    !
    ! X ��ʬ�׻�(2 ����)
    !
    real(8), intent(IN) :: ap_data(:,ks:)                  !(in) ���顼���󷸿�
    real(8)             :: ap_Dx_ap(size(ap_data,1),ks:km) !(out) ��ʬ���顼����
    ap_Dx_ap = ap_at(at_Dx_at(at_ap(ap_data)))
  end function ap_Dx_ap

  function p_Dx_p(p_data)
    !
    ! �����ӥ����աݥ��顼����ˡ
    ! ��ü�ǥ��ꥯ����ü�Υ��ޥ󶭳����
    !
    ! X ��ʬ�׻�(1 ����)
    !
    real(8), intent(IN) :: p_data(ks:km)
    real(8)             :: p_Dx_p(ks:km)

    p_Dx_p = p_t(t_Dx_t(t_p(p_data)))

  end function p_Dx_p

end module at_ap_galerkin_DN
