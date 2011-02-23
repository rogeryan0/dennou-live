!----------------------------------------------------------------------
!   COPYRIGHT (c) 2005-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_ah_galerkin_MM1ex
!
!      �����ӥ����աݥ��顼����ˡ
!      �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����(���ѷ���)�ѥ⥸�塼��
!
!          cfdx1_xmax(j) f'(j,i=0)  + cfdx0_xmax f(j,i=0)  = 0,
!          cfdx1_xmin(j) f'(j,i=im) + cfdx0_xmin f(j,i=im) = 0,
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
module at_ah_galerkin_MMex

  use lumatrix
  use at_module
  use dc_message

  implicit none

  private
  public :: at_ah_galerkin_MMex_Initial   ! ��������֥롼����
  public :: ah_at                         ! �����ӥ�����->���顼�����Ѵ�
  public :: ah_ag                         ! �ʻ���->���顼����ǡ����Ѵ�
  public :: at_ah                         ! ���顼����->�����ӥ������Ѵ�
  public :: ag_ah                         ! ���顼����->�ʻ����ǡ����Ѵ�
  public :: ah_Dx_ah                      ! X ��ʬ

  real(8), allocatable :: TH(:,:,:)       ! ���顼����->�����ӥ������Ѵ�����
  real(8), allocatable :: HT(:,:,:)       ! �����ӥ�����->���顼�����Ѵ�����
  integer, allocatable :: kp(:,:)         ! LU ʬ���ѥԥܥåȳ�Ǽ����

  real(8), allocatable :: alpha(:)     ! �����ӥ�����<->���顼��������ѷ���
  real(8), allocatable :: beta(:)      ! �����ӥ�����<->���顼��������·���

  integer :: im                        ! �ʻ�����              
  integer :: km                        ! �����ӥ����������ȿ�  
  integer :: jm                        ! �� 1 �������礭��
  integer,parameter :: ks=2            ! ���顼���������㼡��

  logical :: MMex_Initialized=.false.   ! ������ե�å�
  real(8), parameter :: EPS = 1.0D-14   ! ����Ƚ����������

  real(8), allocatable :: cfd0_xmin(:) ! ������ﷸ��(0����ʬ at x=xmin)
  real(8), allocatable :: cfd1_xmin(:) ! ������ﷸ��(1����ʬ at x=xmin)
  real(8), allocatable :: cfd0_xmax(:) ! ������ﷸ��(0����ʬ at x=xmax)
  real(8), allocatable :: cfd1_xmax(:) ! ������ﷸ��(1����ʬ at x=xmax)

  save im, km, jm, TH, HT, kp, alpha, beta, MMex_Initialized
  save cfd0_xmin, cfd1_xmin, cfd0_xmax, cfd1_xmax
  public TH, HT, alpha, beta

contains

  !
  ! �����ӥ����աݥ��顼����ˡ 
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維������ѥ⥸�塼��
  ! ��������֥롼����
  !
  subroutine at_ah_galerkin_MMex_Initial(i_in,k_in,j_in,      &
                                      cfdx0_xmax,cfdx1_xmax,  &
                                      cfdx0_xmin,cfdx1_xmin    )

    integer, intent(IN) :: i_in      ! (in)�ʻ�����
    integer, intent(IN) :: k_in      ! (in)�����ӥ����������ȿ�  
    integer, intent(IN) :: j_in      ! (in)�� 1 �������礭��

    real(8), intent(IN),optional :: cfdx0_xmin(j_in) ! ������ﷸ��(0����ʬ@x=xmin)
    real(8), intent(IN),optional :: cfdx1_xmin(j_in) ! ������ﷸ��(1����ʬ@x=xmin)
    real(8), intent(IN),optional :: cfdx0_xmax(j_in) ! ������ﷸ��(0����ʬ@x=xmax)
    real(8), intent(IN),optional :: cfdx1_xmax(j_in) ! ������ﷸ��(1����ʬ@x=xmax)

    real(8) :: Dfac                  ! ��ʬ�Ѵ�����
    real(8) :: Delta(j_in)           ! ��������ι���

    integer :: j, k, l, m, n
    real(8) :: a(j_in),b(j_in),c(j_in),d(j_in),e(j_in),f(j_in)
    character(len=10) :: cj


    !---------- �ʻ������������ȿ����� ----------
    im=i_in ; km=k_in ; jm = j_in

    !---------- ������ﷸ������ ----------
    if ( allocated(cfd0_xmin) ) deallocate(cfd0_xmin)
    if ( allocated(cfd1_xmin) ) deallocate(cfd1_xmin)
    if ( allocated(cfd0_xmax) ) deallocate(cfd0_xmax)
    if ( allocated(cfd1_xmax) ) deallocate(cfd1_xmax)

    allocate(cfd0_xmin(jm),cfd1_xmin(jm))
    allocate(cfd0_xmax(jm),cfd1_xmax(jm))

    Dfac = (g_X(0)-g_X(im))/2.0D0

    if( present(cfdx0_xmin) ) then
       cfd0_xmin = cfdx0_xmin
    else
       call MessageNotify('M','at_ah_galerkin_MM_Initial(Type1)',&
                          'cfdx0_xmin set to zero internally.')
       cfd0_xmin = 0.0D0
    endif

    if( present(cfdx0_xmax) ) then
       cfd0_xmax = cfdx0_xmax
    else
       call MessageNotify('M','at_ah_galerkin_MM_Initial(Type1)',&
                          'cfdx0_xmax set to zero internally.')
       cfd0_xmax = 0.0D0
    endif

    if( present(cfdx1_xmin) ) then
       cfd1_xmin = cfdx1_xmin/Dfac
    else
       call MessageNotify('M','at_ah_galerkin_MM_Initial(Type1)',&
                          'cfdx1_xmin set to zero internally.')
       cfd1_xmin = 0.0D0
    endif

    if( present(cfdx1_xmax) ) then
       cfd1_xmax = cfdx1_xmax/Dfac
    else
       call MessageNotify('M','at_ah_galerkin_MM_Initial(Type1)',&
                          'cfdx1_xmax set to zero internally.')
       cfd1_xmax = 0.0D0
    endif

    !---------- �ǥ��ꥯ�졦�Υ��ޥ󺮹������Ѵ��������� ----------
    if ( allocated(TH) ) deallocate(TH)
    if ( allocated(HT) ) deallocate(HT)
    if ( allocated(kp) ) deallocate(kp)
    if ( allocated(alpha) ) deallocate(alpha)
    if ( allocated(beta) ) deallocate(beta)
    allocate(TH(jm,0:km,ks:km),HT(jm,ks:km,ks:km),kp(jm,ks:km))
    allocate(alpha(0:km),beta(0:km))

    TH = 0.0D0
    do k=ks,ks
       a = cfd1_xmax * (k-1)**2 + cfd0_xmax
       b = cfd1_xmax * (k-2)**2 + cfd0_xmax 
       c = cfd1_xmin * (k-1)**2 - cfd0_xmin 
       d = -cfd1_xmin * (k-2)**2 + cfd0_xmin 
       e = -( cfd1_xmax * k**2 + cfd0_xmax )
       f = -(-cfd1_xmin * k**2 + cfd0_xmin )
       Delta = a*d -b*c

       do j=1,jm
          if ( abs(Delta(j))/max(abs(a(j)),abs(b(j)),abs(c(j)),abs(d(j))) &
               .LT. EPS ) then
             write(cj,'(I10)') j
             call MessageNotify('W','at_ah_galerkin_MM_Initial(Type1)',&
                  'Determinant of coefficent matrix equals zero.' )
             call MessageNotify('W','at_af_galerkin_MM_Initial(Type1)',&
                  'Type of the base function at the ('                 &
                  //trim(adjustl(cj))//',2)-th element is modified to '&
                  //'C_1 T_1(x) + C_0 T_0(x)' )

             TH(j,k-1,k) = -cfdx0_xmax(j)
             TH(j,k-2,k) = cfdx1_xmax(j) + cfdx0_xmax(j)
          else
             TH(j,k-1,k) = 1.0D0/Delta(j) * (  d(j)*e(j) - b(j)*f(j) )
             TH(j,k-2,k) = 1.0D0/Delta(j) * ( -c(j)*e(j) + a(j)*f(j) )
             TH(j,k,k) = 1.0D0
          end if
       enddo
    end do

    do k=ks+1,km
       a = cfd1_xmax * (k-1)**2 + cfd0_xmax
       b = cfd1_xmax * (k-2)**2 + cfd0_xmax 
       c = cfd1_xmin * (k-1)**2 - cfd0_xmin 
       d = -cfd1_xmin * (k-2)**2 + cfd0_xmin 
       e = -( cfd1_xmax * k**2 + cfd0_xmax )
       f = -(-cfd1_xmin * k**2 + cfd0_xmin )
       Delta = a*d -b*c

       do j=1,jm
          if ( abs(Delta(j))/max(abs(a(j)),abs(b(j)),abs(c(j)),abs(d(j))) &
               .LT. EPS ) then
             write(6,*)j,k
             call MessageNotify('E','at_ah_galerkin_MM_Initial(Type1)',&
                  'Determinant of coefficent matrix equals zero.' )
          else
             TH(j,k-1,k) = 1.0D0/Delta(j) * (  d(j)*e(j) - b(j)*f(j) )
             TH(j,k-2,k) = 1.0D0/Delta(j) * ( -c(j)*e(j) + a(j)*f(j) )
             TH(j,k,k) = 1.0D0
          end if
       enddo
    end do

    beta=1.0 ; beta(0)=0.5D0
    if (im .eq. km ) beta(km)=0.5D0

    ! �ǥ��ꥯ�졦�Υ��ޥ󺮹������Ѵ��չ���
    alpha=1.0 ; alpha(0)=2.0D0

    HT = 0.0D0
    do m=ks,km
       do n=ks,km
          do l=0,km
             HT(:,m,n) = HT(:,m,n) + alpha(l)*TH(:,l,m)*TH(:,l,n)
          enddo
       enddo
    enddo

    call LUDecomp(HT,kp)

    call MessageNotify('M','at_ah_galerkin_MMex_Initial(Type1)',&
                           'Conversion matrices initialized')

    MMex_Initialized=.true.

  end subroutine at_ah_galerkin_MMex_Initial

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! �����ӥ����շ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
  !
  function ah_at(at_data)
    real(8), intent(IN) :: at_data(:,0:)                !(in)  �����ӥ����շ���
    real(8)             :: ah_at(size(at_data,1),ks:km) !(out) ���顼���󷸿�  

    real(8)             :: ah_work(size(at_data,1),ks:km)  ! ���������

    integer :: k,m

    if ( .not. MMex_Initialized ) &
         call MessageNotify('E','ah_at',&
                            'at_ah_galerkin_MMex_module(Type1) not initialized')

    ah_work =0.0
    do m=ks,km
       do k=0,km
          ah_work(:,m) = ah_work(:,m) &
               + alpha(k) * beta(k) * at_data(:,k) * TH(:,k,m)
       enddo
    enddo

    ah_at = LUSolve(HT,kp,ah_work)
  end function ah_at

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! ���顼���󷸿� -> �����ӥ����շ����Ѵ�(2�����ǡ���)
  !
  function at_ah(ah_data)

    real(8), intent(IN)  :: ah_data(:,ks:)              !(in)  ���顼���󷸿�
    real(8)              :: at_ah(size(ah_data,1),0:km) !(out) �����ӥ����շ���

    integer :: m, n

    if ( .not. MMex_Initialized ) &
         call MessageNotify('E','at_ah',&
                            'at_ah_galerkin_MMex_module(Type1) not initialized')

    at_ah = 0.0D0
    do m=0,km
       do n=ks,km
          at_ah(:,m) = at_ah(:,m) + TH(:,m,n)*ah_data(:,n)/beta(m)
       enddo
    enddo

  end function at_ah

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! �ʻ����ǡ��� -> ���顼���󷸿��Ѵ�(2�����ǡ���)
  !
  function ah_ag(ag_data)
    real(8), intent(IN)  :: ag_data(:,0:)                !(in)  �ʻ����ǡ���
    real(8)              :: ah_ag(size(ag_data,1),ks:km) !(out) ���顼���󷸿�

    ah_ag = ah_at(at_ag(ag_data))
  end function ah_ag

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! ���顼���󷸿� -> �ʻ����ǡ����Ѵ�(2�����ǡ���)
  !
  function ag_ah(ah_data)
    real(8), intent(IN) :: ah_data(:,ks:)              !(in)  ���顼���󷸿�  
    real(8)             :: ag_ah(size(ah_data,1),0:im) !(out) �ʻ����ǡ���  
    
    ag_ah = ag_at(at_ah(ah_data))
  end function ag_ah

  !
  ! �ǥ��ꥯ�졦�Υ��ޥ󺮹維�����
  ! X ��ʬ�׻�(1 ����)
  !
  function ah_Dx_ah(ah_data)
    real(8), intent(IN) :: ah_data(:,ks:)                  !(in) ���顼���󷸿�
    real(8)             :: ah_Dx_ah(size(ah_data,1),ks:km) !(out) ��ʬ���顼����
    ah_Dx_ah = ah_at(at_Dx_at(at_ah(ah_data)))
  end function ah_Dx_ah

end module at_ah_galerkin_MMex
