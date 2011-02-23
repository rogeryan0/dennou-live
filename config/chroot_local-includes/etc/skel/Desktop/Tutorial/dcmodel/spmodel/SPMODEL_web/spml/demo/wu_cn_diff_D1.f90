!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wu_module
!      ����β��ٳȻ�����(Crank-Nicolson scheme)
!
!      �������ͤ� 0 �ξ��
!      �����ӥ����ա�����ˡ�ˤ�붭�����Ŭ��
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��3 �������ΰ�ǤγȻ����������β��ϲ��
!
!      (wu_diffusion.pdf)�򻲾ȤΤ���. 
!
!����  2008/01/03  �ݹ�����
!
program wu_cn_diff_D1

  use dc_message
  use lumatrix
  use wu_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16, km=16  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=16         ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: ra=1.5               ! ��Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(im,jm,0:km)     :: xyr_Temp   ! ����
  real(8), dimension((nm+1)**2,0:lm) :: wu_Temp    ! ����
  real(8), dimension((nm+1)**2,0:km) :: wr_Temp    ! ����

  real(8), dimension(im,jm,0:km)     :: xyr_TempInit  ! ����(�����)
  real(8), dimension(im,jm,0:km)     :: xyr_TempSol   ! ����(���ϲ�)

  real(8)             :: xy_Tempbndry(im,jm)       ! ������
  real(8)             :: w_Tempbndry((nm+1)**2)    ! ������

 ! Crank Nicholson ��Ū�׻��ѳȻ�����(����)
  real(8)             :: DifLUMT_Temp((nm+1)*(nm+1),0:lm,0:lm)
  integer             :: kpivot_Temp((nm+1)*(nm+1),0:lm)    ! �ԥܥå�

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100, ndisp=10        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  character(len=2), parameter :: TempBC='D'     ! ���ٶ������(D/N)
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
  call wu_Initial(im,jm,km,nm,lm,ra)

  call CNDiffusionMatrix( kappa, dt, DifLUMT_Temp, kpivot_Temp )

 !------------------- ��������� ----------------------
  write(6,*)'n,m,l?'
  read(5,*)n,m,l

  alpha = CalAlpha(n,l)
  sigma = -kappa* alpha **2
  write(6,*) alpha, sigma

  wr_Temp = 0.0
  do k=0,km
     wr_Temp(l_nm(n,m),k) = sbj(n,alpha*r_Rad(k))
  enddo
  xyr_Temp = xyr_wr(wr_Temp)
  xyr_TempInit = xyr_Temp
  wu_Temp = wu_xyr(xyr_Temp)

 !------------------- ���������� ----------------------
  xy_Tempbndry(:,:) = 0

  w_Tempbndry = w_xy(xy_Tempbndry)

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ
     time = it * dt
     wu_Temp = wu_Temp + dt/2 * wu_wr( kappa * wr_Lapla_wu(wu_Temp) )
     call wu_Boundary(wu_Temp,w_Tempbndry,cond=TempBC)
     wu_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,wu_Temp)

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

      PI = atan(1.0D0)*4.0D0

      xs=PI/2.0D0  + n*PI
      xl=PI/2.0D0  + (n+1)*PI

      ValS = sbj(l,ra*xs)
      ValL = sbj(l,ra*xl)
      if ( ValS * ValL .GT. 0.0D0 ) &
           call MessageNotify('E','InvXtanX',&
           'Initial values of ValS and ValL are the same sign.')
!!$          write(6,*) 'vals, vall',vals, vall
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
    real(8), dimension((nm+1)*(nm+1),0:lm)           :: wu_I
    real(8), dimension((nm+1)*(nm+1),0:lm)           :: wu_DI

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wu_I = 0.0 ; wu_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
       wu_DI =  - Diffc * dt/2.0 * wu_wr(wr_Lapla_wu(wu_I))
       call wu_Boundary(wu_DI,cond=TempBC)      ! �Ѳ�ʬ�϶�������Ѳ����ʤ�
       DiffLUMatrix(:,:,l) = wu_I + wu_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrix

 !------------------- ���ϥ롼���� ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='wu_cn_diff_D1.nc', &
           title='Diffusion model in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
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
           varname='tempsol', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature(analytic)', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='temperror', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature(error)', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    xyr_Temp = xyr_wu(wu_Temp)
    xyr_TempSol = xyr_TempInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('temp',xyr_Temp)
    call HistoryPut('tempsol',xyr_TempSol)
    call HistoryPut('temperror',xyr_Temp-xyr_TempSol)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program wu_cn_diff_D1
