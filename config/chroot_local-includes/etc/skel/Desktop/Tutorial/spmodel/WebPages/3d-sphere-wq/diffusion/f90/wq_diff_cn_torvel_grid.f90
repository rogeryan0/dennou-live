!----------------------------------------------------------------------
!     Copyright (c) 2008--2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wq_module
!      ����Υȥ�����®�پ�Ȼ�����(Crank-Nicolson scheme)
!
!      ����ˡ�ˤ�붭�����Ŭ��
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��3 �������ΰ�ǤγȻ����������β��ϲ��
!
!      (wu_diffusion.pdf)�򻲾ȤΤ���. 
!
!����  2008/04/05  �ݹ�����
!      2008/07/20  �ݹ�����  wq_Lapla_wq ���ѹ�
!      2008/08/10  �ݹ�����  ����ź�����ѹ� im -> 0:im-1
!      2009/09/18  �ݹ�����  ��٥å��뷸���Х��ե��å���
!
program wq_diff_cn_torvel_grid

  use dc_message
  use lumatrix
  use wq_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16, km=8  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=15        ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: ra=1.5               ! ��Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(0:im-1,jm,km) :: xyr_Torvel   ! �ȥ�����®��
  real(8), dimension((nm+1)**2,km) :: wr_Torvel    ! �ȥ�����®��

  real(8), dimension(0:im-1,jm,km) :: xyr_TorvelInit  ! �ȥ�����®��(�����)
  real(8), dimension(0:im-1,jm,km) :: xyr_TorvelSol   ! �ȥ�����®��(���ϲ�)

 ! Crank Nicholson ��Ū�׻��ѳȻ�����(����)
  real(8)             :: DifLUMT_Torvel((nm+1)*(nm+1),km,km)
  integer             :: kpivot_Torvel((nm+1)*(nm+1),km)    ! �ԥܥå�

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100, ndisp=10        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: nu=1.0                  ! �Ȼ�����

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! ư���ȿ�
  real(8) :: sigma                   ! ��ĹΨ

 !---- ����¾ ----
  integer :: it=0
  real(8) :: time=0                  ! ����
  integer :: k 
  integer :: n,m,l                   ! �ȿ�

 !---------------- ��ɸ�ͤ����� ---------------------
  call wq_Initial(im,jm,km,nm,lm,ra)

  call CNDiffusionMatrix( nu, dt, DifLUMT_Torvel, kpivot_Torvel )

 !------------------- ��������� ----------------------
  write(6,*)'n,m,l?'
  read(5,*)n,m,l

  alpha = CalAlpha(n,l)
  sigma = -nu* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Growth rate       : ', sigma

  wr_Torvel = 0.0
  do k=1,km
     wr_Torvel(l_nm(n,m),k) = sbj(n,alpha*r_Rad(k))
  enddo
  xyr_Torvel = xyr_wr(wr_Torvel)
  xyr_TorvelInit = xyr_Torvel

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                               ! Crank-Nicolson ˡ�ˤ�������ʬ
     time = it * dt
     wr_Torvel = wr_Torvel + dt/2 * nu * wr_wq(wq_Lapla_wq(wq_wr(wr_Torvel)))
     call wr_TorBoundaryGrid(wr_Torvel,cond='F')
     wr_Torvel = LUSolve(DifLUMT_Torvel,kpivot_Torvel,wr_Torvel)

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
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbj

    sbj = a(n,x)*sin(x) + b(n,x)*cos(x)
  end function sbj

  function sbj1(n,x)
    !
    ! �� 1 ���٥å���ؿ��� 1 ����ʬ
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbj1

    sbj1 = n*sbj(n,x)/x - sbj(n+1,x)
  end function sbj1

   !-----------------------------------------
   ! x*sbj1(l,ra*x) - sbj(l,ra*x)/ra=0 �β�����
   !
    function CalAlpha(l,n)
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
      xl = dx ; ValL = xl*sbj1(l,ra*xl) - sbj(l,ra*xl)/ra

      !
      ! det = 0 �Ȥʤ����� x=0 ����� dx ������õ���Ƥ���
      ! n ���ܤ򶴤� xl, xs �����ꤹ��.
      !
100   continue
      xs =xl ; ValS = ValL
      xl = xl+dx
      ValL = xl*sbj1(l,ra*xl) - sbj(l,ra*xl)/ra
      if( ValL*ValS .le. 0.0 ) nn=nn-1
      if( nn .lt. 0 ) goto 199
      goto 100
199 continue

1000  xm = (xs + xl)/2.0
      ValM = xm*sbj1(l,ra*xm) - sbj(l,ra*xm)/ra
      
      if ( ValS * ValM .GT. 0.0D0 ) then
         xs = xm ; ValS=ValM
      else
         xl = xm ; ValL=ValM
      endif

      if ( abs(xl-xs) .lt. eps ) then
         CalAlpha = xm
         goto 99
      endif

      goto 1000

99    continue
    end function CalAlpha

 !------------------- �Ȼ��� ----------------------
  subroutine CNDiffusionMatrix( Diffc, dt, DiffLUMatrix, kpivot )

    real(8), intent(IN)          :: Diffc    ! �Ȼ�����
    real(8), intent(IN)          :: dt       ! ���ֹ�

    ! Crank Nicholson �Ȼ���Ū�׻��ѹ���(1-D dt/2��^2, LU ʬ��)
    real(8), dimension((nm+1)*(nm+1),km,km), intent(OUT)  :: DiffLUMatrix
    ! �ԥܥåȾ���
    integer, dimension((nm+1)*(nm+1),km), intent(OUT)     :: kpivot

    ! ������ѿ�
    real(8), dimension((nm+1)*(nm+1),km)           :: wr_I
    real(8), dimension((nm+1)*(nm+1),km)           :: wr_DI

    integer :: k

    DiffLUMatrix = 0.0
    do k=1,km
       wr_I = 0.0 ; wr_I(:,k) = 1.0             ! ���ȿ���ʬ��Ω
       wr_DI =  - Diffc * dt/2.0 * wr_wq(wq_Lapla_wq(wq_wr(wr_I)))
       call wr_TorBoundaryGrid(wr_DI,cond='F')
       DiffLUMatrix(:,:,k) = wr_I + wr_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrix

 !------------------- ���ϥ롼���� ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='wq_diff_cn_torvel_grid.nc', &
           title='Diffusion model in a spherical shell', &
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
           varname='torvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='Toroidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='torvelsol', dims=(/'lon','lat','rad','t  '/), & 
           longname='Toroidal velocity potential(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='torvelerror', dims=(/'lon','lat','rad','t  '/), & 
           longname='Toroidal velocity potential(error)', &
           units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    xyr_Torvel = xyr_wr(wr_Torvel)
    xyr_Torvel = xyr_wq(wq_RadDRad_wq(wq_xyr(xyr_TorVel)))/xyr_Rad &
                - xyr_TorVel/xyr_Rad
    write(6,*)xyr_Torvel(1,1,km)

    xyr_Torvel = xyr_wr(wr_Torvel)
    xyr_TorvelSol = xyr_TorvelInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('torvel',xyr_Torvel)
    call HistoryPut('torvelsol',xyr_TorvelSol)
    call HistoryPut('torvelerror',xyr_Torvel-xyr_TorvelSol)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program wq_diff_cn_torvel_grid
