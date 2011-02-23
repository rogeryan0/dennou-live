!----------------------------------------------------------------------
!     Copyright (c) 2008--2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wq_module
!      ����γȻ����� : �ǥ��ꥯ�춭�����(Crank-Nicolson scheme)
!
!      �����ӥ����ա�����ˡ�ˤ�붭�����Ŭ��
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��3 �������ΰ�ǤγȻ����������β��ϲ��
!
!      (wu_diffusion.pdf)�򻲾ȤΤ���. 
!
!����  2008/11/29  �ݹ�����
!      2009/09/18  �ݹ�����  ��٥å��뷸���Х��ե��å���
!
program wq_diff_cn_D_tau

  use dc_message
  use lumatrix
  use wq_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16, km=16   ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=31         ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: ra=1.5               ! ��Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(0:im-1,jm,km)   :: xyr_Temp   ! �ȥ����뼧��
  real(8), dimension((nm+1)**2,0:lm) :: wq_Temp    ! �ȥ����뼧��
  real(8), dimension((nm+1)**2,km)   :: wr_Temp    ! �ȥ����뼧��

  real(8), dimension((nm+1)**2)      :: w_TempBoundary  ! ��������

  real(8), dimension(0:im-1,jm,km) :: xyr_TempInit ! �ȥ����뼧��(�����)
  real(8), dimension(0:im-1,jm,km) :: xyr_TempBase ! �ȥ����뼧��(���ܾ�)
  real(8), dimension(0:im-1,jm,km) :: xyr_TempSol  ! �ȥ����뼧��(���ϲ�)

 ! Crank Nicholson ��Ū�׻��ѳȻ�����(�ȥ����뼧��)
  real(8)             :: DifLUMT_Temp((nm+1)*(nm+1),0:lm,0:lm)
  integer             :: kpivot_Temp((nm+1)*(nm+1),0:lm)    ! �ԥܥå�

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100, ndisp=10        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: kappa=1.0                  ! �Ȼ�����

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

  call CNDiffusionMatrix( kappa, dt, DifLUMT_Temp, kpivot_Temp )

 !------------------- ���ܾ졦�������� ----------------------
  wr_Temp = 0.0D0
  wr_Temp(l_nm(2,1),:) = r_Rad**2
  xyr_TempBase = xyr_wr(wr_Temp)

  w_TempBoundary = 0.0 ; w_TempBoundary(l_nm(2,1)) = Ra**2

 !------------------- ��������� ----------------------
  write(6,*)'n,m,l?'
  read(5,*)n,m,l

  alpha = CalAlpha(n,l)
  sigma = -kappa* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Growqh rate       : ', sigma

  wr_Temp = 0.0
  do k=1,km
     wr_Temp(l_nm(n,m),k) = sbj(n,alpha*r_Rad(k))
  enddo
  xyr_Temp = xyr_wr(wr_Temp) + xyr_TempBase
  xyr_TempInit = xyr_Temp
  wq_Temp = wq_xyr(xyr_Temp)

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                               ! Crank-Nicolson ˡ�ˤ�������ʬ
     time = it * dt
     wq_Temp = wq_Temp + dt/2 * kappa * wq_Lapla_wq(wq_Temp)
     call wq_BoundaryTau(wq_Temp,w_TempBoundary,cond='D')
     wq_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,wq_Temp)

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

   !-----------------------------------------
   ! sbj(ra*x)=0 �β�����
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
      xl = dx ; ValL = sbj(l,ra*xl)

      !
      ! det = 0 �Ȥʤ����� x=0 ����� dx ������õ���Ƥ���
      ! n ���ܤ򶴤� xl, xs �����ꤹ��.
      !
100   continue
      xs =xl ; ValS = ValL
      xl = xl+dx
      ValL = sbj(l,ra*xl)
      if( ValL*ValS .le. 0.0 ) nn=nn-1
      if( nn .lt. 0 ) goto 199
      goto 100
199 continue

1000  xm = (xs + xl)/2.0
      ValM = sbj(l,ra*xm)
      
      if ( ValS * ValM .GT. 0.0D0 ) then
         xs = xm ; ValS=sbj(l,ra*xs)
      else
         xl = xm ; ValL=sbj(l,ra*xl)
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
    real(8), dimension((nm+1)*(nm+1),0:lm,0:lm), intent(OUT)  :: DiffLUMatrix
    ! �ԥܥåȾ���
    integer, dimension((nm+1)*(nm+1),0:lm), intent(OUT)       :: kpivot

    ! ������ѿ�
    real(8), dimension((nm+1)*(nm+1),0:lm)           :: wq_I
    real(8), dimension((nm+1)*(nm+1),0:lm)           :: wq_DI

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wq_I = 0.0 ; wq_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
       wq_DI =  - Diffc * dt/2.0 * wq_Lapla_wq(wq_I)
       call wq_Boundary(wq_DI,cond='D')         ! DI �ζ����ͤ� 0 
       DiffLUMatrix(:,:,l) = wq_I + wq_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrix

 !------------------- ���ϥ롼���� ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='wq_diff_cn_D_tau.nc', &
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
           varname='temp', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='tempdist', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature disturbance', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='tempsol', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='tempdistsol', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature disturbance(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='temperror', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature(error)', &
           units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    xyr_Temp = xyr_wq(wq_Temp)
    xyr_TempSol = (xyr_TempInit-xyr_TempBase)* exp( sigma * time ) &
                + xyr_TempBase

    call HistoryPut('t',real(it*dt))
    call HistoryPut('temp',xyr_Temp)
    call HistoryPut('tempdist',xyr_Temp-xyr_TempBase)
    call HistoryPut('tempsol',xyr_TempSol)
    call HistoryPut('tempdistsol',xyr_TempSol-xyr_TempBase)
    call HistoryPut('temperror',xyr_Temp-xyr_TempSol)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program wq_diff_cn_D_tau
