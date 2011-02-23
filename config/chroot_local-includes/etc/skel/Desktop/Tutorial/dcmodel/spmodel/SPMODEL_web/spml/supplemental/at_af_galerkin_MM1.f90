!----------------------------------------------------------------------
!  COPYRIGHT (c) 2005-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_af_galerkin_MM1
!
!      �����ӥ����աݥ��顼����ˡ
!      �ǥ��ꥯ�졦�Υ��ޥ󺮹維������ѥ⥸�塼��
!          cfdx1_xmax f'(:,i=0)  + cfdx0_xmax f(:,i=0)  = 0,
!          cfdx1_xmin f'(:,i=im) + cfdx0_xmin f(:,i=im) = 0,
!
!      k ���Υ��顼������줬
!
!        \phi_k(x)=T_k(x) + C_{k-1}T_{k-1}(x) + C_{k-2}T_{k-2}(x) 
!
!      �η���(Type1)
!
!���
!      �⼡(k>2)�η������󼰤� 0 �Ȥʤ���ˤϷ׻��Ǥ��ʤ�. 
!
!����  2006/01/22  �ݹ�����  ��������
!      2006/01/23  �ݹ�����  �⥸�塼��̾�ѹ�
!      2006/01/25  �ݹ�����  �⥸�塼�������ե�å�Ƴ��
!      2006/02/15  �ݹ�����  �������� 0 �����̤ʾ��
!                            (cfdx0_xmin=cfdx0_xmax=0)���б�
!      2006/02/26  �ݹ�����  k=2 �� �������󼰤� 0 �ξ��˴����б�
!
module at_af_galerkin_MM

  use lumatrix
  use at_module
  use dc_message

  implicit none

  private
  public :: at_af_galerkin_MM_Initial  ! ��������֥롼����
  public :: af_at, f_t                 ! �����ӥ�����->���顼�����Ѵ�
  public :: af_ag, f_g                 ! �ʻ���->���顼����ǡ����Ѵ�
  public :: at_af, t_f                 ! ���顼����->�����ӥ������Ѵ�
  public :: ag_af, g_f                 ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: af_Dx_af, f_Dx_f           ! X ��ʬ

  real(8), allocatable :: TF(:,:)      ! ���顼����->�����ӥ������Ѵ�����
  real(8), allocatable :: FT(:,:)      ! �����ӥ�����->���顼�����Ѵ�����
  integer, allocatable :: kp(:)        ! LU ʬ���ѥԥܥåȳ�Ǽ����

  real(8), allocatable :: alpha(:)     ! �����ӥ�����<->���顼��������ѷ���
  real(8), allocatable :: beta(:)      ! �����ӥ�����<->���顼��������·���

  integer :: im                        ! �ʻ�����              
  integer :: km                        ! �����ӥ����������ȿ�  
  integer :: jm                        ! �� 1 �������礭��
  integer,parameter :: ks=2            ! ���顼���������㼡��

  real(8) :: cfd0_xmin                 ! ������ﷸ��(0����ʬ at x=xmin)
  real(8) :: cfd1_xmin                 ! ������ﷸ��(1����ʬ at x=xmin)
  real(8) :: cfd0_xmax                 ! ������ﷸ��(0����ʬ at x=xmax)
  real(8) :: cfd1_xmax                 ! ������ﷸ��(1����ʬ at x=xmax)

  logical :: MM_Initialized=.false.    ! ������ե�å�
  real(8),parameter :: EPS = 1.0D-14   ! ����Ƚ����������

  save im, km, jm, TF, FT, kp, alpha, beta, MM_Initialized
  save cfd0_xmin, cfd1_xmin, cfd0_xmax, cfd1_xmax
  public TF, FT, alpha, beta

contains

  !
  ! �����ӥ����աݥ��顼����ˡ 
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維������ѥ⥸�塼��
  ! ��������֥롼����
  !
  subroutine at_af_galerkin_MM_Initial(i_in,k_in,              &
                                    cfdx0_xmax,cfdx1_xmax,  &
                                    cfdx0_xmin,cfdx1_xmin    )

    integer, intent(IN) :: i_in      ! (in)�ʻ�����
    integer, intent(IN) :: k_in      ! (in)�����ӥ����������ȿ�  

    real(8), intent(IN),optional :: cfdx0_xmin ! ������ﷸ��(0����ʬ@x=xmin)
    real(8), intent(IN),optional :: cfdx1_xmin ! ������ﷸ��(1����ʬ@x=xmin)
    real(8), intent(IN),optional :: cfdx0_xmax ! ������ﷸ��(0����ʬ@x=xmax)
    real(8), intent(IN),optional :: cfdx1_xmax ! ������ﷸ��(1����ʬ@x=xmax)

    real(8) :: Dfac                  ! ��ʬ�Ѵ�����
    real(8) :: Delta                 ! ��������ι���

    integer :: k, l, m, n
    real(8) :: a,b,c,d,e,f

    !---------- �ʻ������������ȿ����� ----------
    im=i_in ; km=k_in

    !---------- ������ﷸ������ ----------
    Dfac = (g_X(0)-g_X(im))/2.0D0

    if( present(cfdx0_xmin) ) then
       cfd0_xmin = cfdx0_xmin
    else
       call MessageNotify('M','at_af_galerkin_MM_Initial(Type1)',&
                          'cfdx0_xmin set to zero internally.')
       cfd0_xmin = 0.0D0
    endif

    if( present(cfdx0_xmax) ) then
       cfd0_xmax = cfdx0_xmax
    else
       call MessageNotify('M','at_af_galerkin_MM_Initial(Type1)',&
                          'cfdx0_xmax set to zero internally.')
       cfd0_xmax = 0.0D0
    endif

    if( present(cfdx1_xmin) ) then
       cfd1_xmin = cfdx1_xmin/Dfac
    else
       call MessageNotify('M','at_af_galerkin_MM_Initial(Type1)',&
                          'cfdx1_xmin set to zero internally.')
       cfd1_xmin = 0.0D0
    endif

    if( present(cfdx1_xmax) ) then
       cfd1_xmax = cfdx1_xmax/Dfac
    else
       call MessageNotify('M','at_af_galerkin_MM_Initial(Type1)',&
                          'cfdx1_xmax set to zero internally.')
       cfd1_xmax = 0.0D0
    endif

    !---------- �ǥ��ꥯ�졦�Υ��ޥ󺮹������Ѵ��������� ----------
    if ( allocated(TF) ) deallocate(TF)
    if ( allocated(FT) ) deallocate(FT)
    if ( allocated(kp) ) deallocate(kp)
    if ( allocated(alpha) ) deallocate(alpha)
    if ( allocated(beta) ) deallocate(beta)
    allocate(TF(0:km,ks:km),FT(ks:km,ks:km),kp(ks:km))
    allocate(alpha(0:km),beta(0:km))

    TF = 0.0D0

    do k=ks,ks
       a = cfd1_xmax * (k-1)**2 + cfd0_xmax
       b = cfd1_xmax * (k-2)**2 + cfd0_xmax 
       c = cfd1_xmin * (k-1)**2 - cfd0_xmin 
       d = -cfd1_xmin * (k-2)**2 + cfd0_xmin 
       e = -( cfd1_xmax * k**2 + cfd0_xmax )
       f = -(-cfd1_xmin * k**2 + cfd0_xmin )
       Delta = a*d -b*c

       if ( abs(Delta)/max(abs(a),abs(b),abs(c),abs(d)) .LT. EPS ) then
          call MessageNotify('W','at_af_galerkin_MM_Initial(Type1)',&
               'Determinant of coefficent matrix equals zero at k=2.' )
          call MessageNotify('W','at_af_galerkin_MM_Initial(Type1)',&
               'Type of the base function of k=2 is modified to &
               &C_1 T_1(x) + C_0 T_0(x)' )
          TF(k-1,k) = -cfdx0_xmax
          TF(k-2,k) = cfdx1_xmax + cfdx0_xmax
       else
          TF(k-1,k) = 1.0D0/Delta * (  d*e - b*f )
          TF(k-2,k) = 1.0D0/Delta * ( -c*e + a*f )
          TF(k,k) = 1.0D0
       endif
    enddo

    do k=ks+1,km
       a = cfd1_xmax * (k-1)**2 + cfd0_xmax
       b = cfd1_xmax * (k-2)**2 + cfd0_xmax 
       c = cfd1_xmin * (k-1)**2 - cfd0_xmin 
       d = -cfd1_xmin * (k-2)**2 + cfd0_xmin 
       e = -( cfd1_xmax * k**2 + cfd0_xmax )
       f = -(-cfd1_xmin * k**2 + cfd0_xmin )
       Delta = a*d -b*c

       if ( abs(Delta)/max(abs(a),abs(b),abs(c),abs(d)) .LT. EPS ) then
          call MessageNotify('E','at_af_galerkin_MM_Initial(Type1)',&
               'Determinant of coefficent matrix equals zero.' )
       else
          TF(k-1,k) = 1.0D0/Delta * (  d*e - b*f )
          TF(k-2,k) = 1.0D0/Delta * ( -c*e + a*f )
          TF(k,k) = 1.0D0
       endif
    enddo

    beta=1.0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! �ǥ��ꥯ�졦�Υ��ޥ󺮹������Ѵ��չ���
    alpha=1.0 ; alpha(0)=2.0D0

    FT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             FT(m,n) = FT(m,n) + alpha(l)*TF(l,m)*TF(l,n)
          enddo
       enddo
    enddo

    call LUDecomp(FT,kp)

    call MessageNotify('M','at_af_galerkin_MM_Initial(Type1)',&
                           'Conversion matrices initialized')

    MM_Initialized=.true.

  end subroutine at_af_galerkin_MM_Initial

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
  !
  function af_at(at_data)
    real(8), intent(IN) :: at_data(:,0:)                !(in)  �����ӥ����շ���
    real(8)             :: af_at(size(at_data,1),ks:km) !(out) ���顼���󷸿�  

    real(8)             :: af_work(size(at_data,1),ks:km)  ! ���������

    integer :: k,m

    if ( .not. MM_Initialized ) &
         call MessageNotify('E','af_at',&
                            'at_af_galerkin_MM_module(Type1) not initialized')

    af_work =0.0
    do m=ks,km
       do k=0,km
          af_work(:,m) = af_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TF(k,m)
       enddo
    enddo

    af_at = LUSolve(FT,kp,af_work)
  end function af_at

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
  !
  function f_t(t_data)

    real(8), intent(IN) :: t_data(0:km)        !(in)  �����ӥ����շ���
    real(8)             :: f_t(ks:km)          !(out) ���顼���󷸿�  

    real(8)             :: f_work(ks:km)       ! ���������

    integer :: k,m

    if ( .not. MM_Initialized ) &
         call MessageNotify('E','f_t',&
                            'at_af_galerkin_MM_module(Type1) not initialized')

    f_work =0.0
    do m=ks,km
       do k=0,km
          f_work(m) = f_work(m) &
               + alpha(k) * beta(k) * t_data(k) * TF(k,m)
       enddo
    enddo

    f_t = LUSolve(FT,kp,f_work)
  end function f_t

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(2�����ǡ���)
  !
  function at_af(af_data)

    real(8), intent(IN)  :: af_data(:,ks:)              !(in)  ���顼���󷸿�
    real(8)              :: at_af(size(af_data,1),0:km) !(out) �����ӥ����շ���
    integer :: m, n

    if ( .not. MM_Initialized ) &
         call MessageNotify('E','at_af',&
                            'at_af_galerkin_MM_module(Type1) not initialized')

    at_af = 0.0D0
    do m=0,km
       do n=ks,km
          at_af(:,m) = at_af(:,m) + TF(m,n)*af_data(:,n)/beta(m)
       enddo
    enddo

  end function at_af

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(1�����ǡ���)
  !
  function t_f(f_data)

    real(8), intent(IN)  :: f_data(ks:km)       !(in)  ���顼���󷸿�  
    real(8)              :: t_f(0:km)           !(out) �����ӥ����շ���

    integer :: m, n

    if ( .not. MM_Initialized ) &
         call MessageNotify('E','f_t',&
                            'at_af_galerkin_MM_module(Type1) not initialized')

    t_f = 0.0D0
    do m=0,km
       do n=ks,km
          t_f(m) = t_f(m) + TF(m,n)*f_data(n)/beta(m)
       enddo
    enddo

  end function t_f

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
  !
  function af_ag(ag_data)
    real(8), intent(IN)  :: ag_data(:,0:)                !(in)  �ʻ����ǡ���
    real(8)              :: af_ag(size(ag_data,1),ks:km) !(out) ���顼���󷸿�

    af_ag = af_at(at_ag(ag_data))
  end function af_ag

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(1�����ǡ���)
  !
  function f_g(g_data)
    real(8), intent(IN)  :: g_data(0:im)        !(in)  �ʻ����ǡ���
    real(8)              :: f_g(ks:km)          !(out) ���顼���󷸿�

    f_g = f_t(t_g(g_data))
  end function f_g

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(2�����ǡ���)
  !
  function ag_af(af_data)
    real(8), intent(IN) :: af_data(:,ks:)              !(in)  ���顼���󷸿�  
    real(8)             :: ag_af(size(af_data,1),0:im) !(out) �ʻ����ǡ���  
    
    ag_af = ag_at(at_af(af_data))
  end function ag_af

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(1�����ǡ���)
  !
  function g_f(f_data)
    real(8), intent(IN) :: f_data(ks:km)        !(in)  ���顼���󷸿�  
    real(8)             :: g_f(0:im)            !(out) �ʻ����ǡ���  
    
    g_f = g_t(t_f(f_data))
  end function g_f

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! X ��ʬ�׻�(1 ����)
  !
  function af_Dx_af(af_data)
    real(8), intent(IN) :: af_data(:,ks:)                  !(in) ���顼���󷸿�
    real(8)             :: af_Dx_af(size(af_data,1),ks:km) !(out) ��ʬ���顼����
    af_Dx_af = af_at(at_Dx_at(at_af(af_data)))
  end function af_Dx_af

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! X ��ʬ�׻�(1 ����)
  !
  function f_Dx_f(f_data)
    real(8), intent(IN) :: f_data(ks:km)
    real(8)             :: f_Dx_f(ks:km)

    f_Dx_f = f_t(t_Dx_t(t_f(f_data)))

  end function f_Dx_f

end module at_af_galerkin_MM
