!----------------------------------------------------------------------
!     Copyright (c) 2008--2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wq_module ����ץ�ץ����
!      ����Υݥ�����®�پ�Ȼ�����(Crank-Nicolson scheme)
!      ��¦���٤���
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��3 �������ΰ�ǤγȻ����������β��ϲ��
!
!      (wu_diffusion.pdf)�򻲾ȤΤ���. 
!
!���  ***** wr_LaplaPol2Pol_wr ̤�����Τ�����Ѷػ� *****
!
!����  2008/04/05  �ݹ�����
!      2008/07/20  �ݹ�����  wq_Lapla_wq ���ѹ�
!      2008/08/10  �ݹ�����  ����ź�����ѹ� im -> 0:im-1
!      2009/09/18  �ݹ�����  ��٥å��뷸���Х��ե��å���
!
program wq_diff_cn_polvel_grid_F
  !
  !  wq_module ����ץ�ץ����
  !      ����Υݥ�����®�پ�Ȼ�����(Crank-Nicolson scheme)
  !      ��¦���٤�Ǵ����
  !
  !      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
  !
  !         ��3 �������ΰ�ǤγȻ����������β��ϲ��
  !
  !      (wu_diffusion.pdf)�򻲾ȤΤ���. 
  !
  use dc_message
  use lumatrix
  use wq_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16, km=8   ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=15         ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: ra=1.5               ! ��Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(0:im-1,jm,km) :: xyr_Polvel     ! �ݥ�����®�پ�
  real(8), dimension((nm+1)**2,km) :: wr_Polvel      ! �ݥ�����®�پ�
  real(8), dimension((nm+1)**2,km) :: wr_LaplaPolvel ! ��^2�ݥ�����

  real(8), dimension(0:im-1,jm,km) :: xyr_PolvelInit  ! �ݥ�����®�پ�(�����)
  real(8), dimension(0:im-1,jm,km) :: xyr_PolvelSol   ! �ݥ�����®�پ�(���ϲ�)

 ! Crank Nicholson ����
  real(8)             :: DifLUMT_Polvel((nm+1)*(nm+1),km,km)
                         ! Crank Nicholson ��Ū�׻��ѳȻ�����(�ݥ�����®�پ�)
  integer             :: kpivot_Polvel((nm+1)*(nm+1),km)
                         ! Crank Nicholson ��Ū�׻��ѳȻ�����ԥܥå�

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100, ndisp=10        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: nu=1.0                  ! Ǵ������

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! ư���ȿ�
  real(8) :: sigma                   ! ��ĹΨ
  real(8) :: C(2)                    ! ��Ω��η���

 !---- ����¾ ----
  integer :: it=0
  real(8) :: time=0                  ! ����
  integer :: k 
  integer :: n,m,l                   ! �ȿ�

 !---------------- ��ɸ�ͤ����� ---------------------
  call wq_Initial(im,jm,km,nm,lm,ra)

  call CNDiffusionMatrixPolvel( nu, dt, DifLUMT_Polvel, kpivot_Polvel )

 !------------------- ��������� ----------------------
  write(6,*) 'Total horizontal, longitudinal, radial wavenumbers: n,m,l?'
  read(5,*)n,m,l

  alpha = CalAlpha(n,l)
  sigma = -nu* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Growuh rate       : ', sigma

  C = CalcCoeff(n,alpha)

  wr_Polvel = 0.0
  do k=1,km
     wr_Polvel(l_nm(n,m),k) &
        =   C(1)*r_Rad(k)**n  + C(2)*sbj(n,alpha*r_Rad(k))
  enddo

  xyr_Polvel = xyr_wr(wr_Polvel)
  xyr_PolvelInit = xyr_Polvel
  wr_Polvel = wr_xyr(xyr_Polvel)
  wr_LaplaPolvel = wr_wq(wq_Lapla_wq(wq_wr(wr_Polvel)))

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                               ! Crank-Nicolson ˡ�ˤ�������ʬ
     time = it * dt

     wr_LaplaPolvel = wr_LaplaPolvel &
          + dt/2 * nu * wr_wq(wq_Lapla_wq(wq_wr(wr_LaplaPolvel)))
     wr_Polvel = wr_LaplaPol2Pol_wr(wr_LaplaPolvel,cond='F')
     wr_Polvel = LuSolve(DifLUMT_Polvel,kpivot_Polvel,wr_Polvel)
     wr_LaplaPolvel = wr_wq(wq_Lapla_wq(wq_wr(wr_Polvel)))

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains
  !
  ! Spherical bessel functions
  !
  function a(n,x)
    !
    ! ��٥å���ȡ���η��� a_n(x)
    !   j_n(x) = a_n(x)sin(x) + b_n(x) cos(x)
    !   n_n(x) = b_n(x)sin(x) - a_n(x) cos(x)
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: a

    select case(n)
    case (0)
       a = 1/x
    case (1)
       a = 1/x**2
    case (2)
       a = (3-x**2)/x**3
    case (3)
       a = (15-6*x**2)/x**4
    case (4)
       a = (105-45*x**2+x**4)/x**5
    case (5)
       a = (945-420*x**2+15*x**4)/x**6
    case default
       write(6,*) 'Index is out of range.'
       stop
    end select
  end function a

  function b(n,x)
    !
    ! ��٥å���ȡ���η��� b_n(x)
    !    j_n(x) = a_n(x)sin(x) + b_n(x) cos(x)
    !    n_n(x) = b_n(x)sin(x) - a_n(x) cos(x)
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: b

    select case(n)
    case (0)
       b = 0.0D0
    case (1)
       b = -1/x
    case (2)
       b = -3/x**2
    case (3)
       b = -(15-x**2)/x**3
    case (4)
       b = -(105-10*x**2)/x**4
    case (5)
       b = -(945-105*x**2+x**4)/x**5
    case default
       write(6,*) 'Index is out of range.'
       stop
    end select
  end function b

  function sbj(n,x)
    !
    ! ��1���٥å���ؿ�
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbj

    sbj = a(n,x)*sin(x) + b(n,x)*cos(x)
  end function sbj

  function sbn(n,x)
    !
    ! ��2���٥å���ؿ�
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbn

    sbn = b(n,x)*sin(x) - a(n,x)*cos(x)
  end function sbn

  function sbj1(n,x)
    !
    ! �� 1 ���٥å���ؿ��� 1 ����ʬ
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbj1

    sbj1 = n*sbj(n,x)/x - sbj(n+1,x)
  end function sbj1

  function sbn1(n,x)
    !
    ! �� 2 ���٥å���ؿ��� 1 ����ʬ
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbn1

    sbn1 = n*sbn(n,x)/x - sbn(n+1,x)
  end function sbn1

  function sbj2(n,x)
    !
    ! �� 1 ���٥å���ؿ��� 1 ����ʬ
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbj2

    sbj2 = -2*sbj1(n,x)/x - (1-n*(n+1)/x**2)*sbj(n,x)
  end function sbj2

  recursive function detNxN(a) result(determinant)
    !
    ! NxN ����ι��󼰤η׻�
    !
    implicit none
    real(8),intent(IN) :: a(:,:)
    real(8) :: determinant
    real(8),allocatable :: b(:,:)
    integer :: i,n

    if ( size(a,1) /= size(a,2) )then
       write(6,*) '**** detNxN **** Matrix is not square. '
       stop
    endif

    n=size(a,1)

    if ( n == 2 ) then
       determinant = a(1,1)*a(2,2)-a(2,1)*a(1,2)
    else
       allocate(b(n-1,n-1))
       b = a(2:n,2:n)
       determinant = a(1,1) * detNxN(b)
       do i=2,n-1
          b(1:i-1,:) = a(1:i-1,2:n)
          b(i:n-1,:) = a(i+1:n,2:n)
          determinant = determinant + (-1)**(i-1)*a(i,1)*detNxN(b)
       enddo
       b = a(1:n-1,2:n)
       determinant = determinant + (-1)**(n-1)*a(n,1)*detNxN(b)
       deallocate(b)
    end if

    return 

  end function detNxN

  function det(l,x)
    !
    ! �������󼰦��η׻�
    !
    integer, intent(IN) :: l                 ! ����
    real(8), intent(IN) :: x                 ! 
    real(8)             :: det               ! ����
    real(8)             :: a(2,2)            ! ����

    a(1,1) = ra**l             ; a(1,2) = sbj(l,ra*x)
    a(2,1) = l*(l-1)*ra**(l-2) ; a(2,2) = x**2*sbj2(l,ra*x)

    det=detNxN(a)

  end function det

  function CalcCoeff(l,alpha)
    !
    integer, intent(IN) :: l                 ! ����
    real(8), intent(IN) :: alpha             ! ư���ȿ�
    real(8)             :: CalcCoeff(2)      ! ����

    CalcCoeff(1) = sbj(l,ra*alpha) 
    CalcCoeff(2) = -ra**l

  end function CalcCoeff

  function CalAlpha(l,n)
    !
    ! ��=0 �β�����
    !
    integer, intent(IN) :: l                 ! ����
    integer, intent(IN) :: n                 ! �������ֹ�
    real(8)             :: CalAlpha
    real(8), parameter  :: eps = 1.0D-14     ! �������

    real(8) :: PI
    real(8) :: xs, xl, xm
    real(8) :: ValS, ValL, ValM

    real(8) :: dx
    integer :: nx=20
    integer :: nn

    nn = n
    PI = atan(1.0D0)*4.0D0
    dx = PI/ra/nx
    xl = dx ; ValL = det(l,xl)

    !
    ! det = 0 �Ȥʤ����� x=0 ����� dx ������õ���Ƥ���
    ! n ���ܤ򶴤� xl, xs �����ꤹ��.
    !
100 continue
       xs =xl ; ValS = ValL
       xl = xl+dx
       ValL = det(l,xl)
       if( ValL*ValS .le. 0.0 ) nn=nn-1
       if( nn .lt. 0 ) goto 199
       goto 100
199 continue
        
1000 xm = (xs + xl)/2.0
    ValM = det(l,xm)
      
    if ( ValS * ValM .GT. 0.0D0 ) then
       xs = xm ; ValS = det(l,xs)
    else
       xl = xm ; ValL = det(l,xl)
    endif
     
    if ( abs(xl-xs) .lt. eps ) then
       CalAlpha = xm
       goto 99
    endif
     
    goto 1000
     
99  continue
  end function CalAlpha
    
    !------------------- �Ȼ��� ----------------------
    subroutine CNDiffusionMatrixPolvel( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson �Ȼ���Ū�׻��ѹ��������
      ! (1-D dt/2��^2, LU ʬ��)
      ! 
      real(8), intent(IN)   :: Diffc    ! �Ȼ�����
      real(8), intent(IN)   :: dt       ! ���ֹ�

      ! Crank Nicholson ����
      real(8), intent(OUT)  :: DiffLUMatrix((nm+1)*(nm+1),km,km)
                              ! Crank Nicholson �Ȼ���Ū�׻��ѹ���
                              ! (1-D dt/2��^2, LU ʬ��)

      integer, intent(OUT)  :: kpivot((nm+1)*(nm+1),km)     ! �ԥܥåȾ���
                              ! Crank Nicholson �Ȼ���Ū�׻��ѹ���ԥܥåȾ���

      real(8)               :: wr_I((nm+1)*(nm+1),km)        ! ������ѿ�
      real(8)               :: wr_DI((nm+1)*(nm+1),km)       ! ������ѿ�

      integer :: k

      DiffLUMatrix = 0.0
      do k=1,km
         wr_I = 0.0 ; wr_I(:,k) = 1.0             ! ���ȿ���ʬ��Ω
         wr_DI = - Diffc * dt/2.0 &
                  * wr_wq(wq_Lapla_wq(wq_Lapla_wq(wq_wr(wr_I))))
         wr_DI = wr_LaplaPol2Pol_wr(wr_DI,cond='F')
         DiffLUMatrix(:,:,k) = wr_I + wr_DI
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixPolvel

 !------------------- ���ϥ롼���� ----------------------
  subroutine output_gtool4_init
    !
    ! �ҥ��ȥ꡼���Ͻ�����롼����
    !
    !   �ե��������
    !   �ѿ����
    !
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='wq_diff_cn_polvel_grid_F.nc', &
           title='Diffusion model in a sphere', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_Lon/pi*180)                       ! �ѿ�����
    call HistoryPut('lat',y_Lat/pi*180)                       ! �ѿ�����
    call HistoryPut('rad',r_Rad)                              ! �ѿ�����

    call HistoryAddVariable( &                                ! �ѿ����
           varname='polvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal velocity field', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polvelsol', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal velocity field(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polvelerror', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal velocity field(error)', &
           units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    !
    ! �ҥ��ȥ꡼����
    !
    !   ��ʪ���̤� gtool4 �ե�����ؤν��Ϥ�Ԥ�
    !
    write(6,*) 'it = ',it
    xyr_Polvel = xyr_wr(wr_Polvel)
    xyr_PolvelSol = xyr_PolvelInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('polvel',xyr_Polvel)
    call HistoryPut('polvelsol',xyr_PolvelSol)
    call HistoryPut('polvelerror',xyr_Polvel-xyr_PolvelSol)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    !
    ! �ҥ��ȥ꡼���Ͻ�λ����
    !
    call HistoryClose
  end subroutine output_gtool4_close

end program wq_diff_cn_polvel_grid_F
