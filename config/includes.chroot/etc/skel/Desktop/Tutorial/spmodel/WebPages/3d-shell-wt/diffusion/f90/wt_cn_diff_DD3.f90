!----------------------------------------------------------------------
!     Copyright (c) 2007--2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_module
!      �����β��ٳȻ�����(Crank-Nicolson scheme)
!
!      �������ͤ� 0 �Ǥʤ��ͤξ��
!      �����ӥ����ա�����ˡ�ˤ�붭�����Ŭ��
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��3 ��������ΰ�ǤγȻ����������β��ϲ��
!
!      (wt_diffusion.pdf)�򻲾ȤΤ���. 
!
!����  2007/12/19  �ݹ����� 
!      2008/08/13  �ݹ�����  ����ź�����ѹ� im -> 0:im-1
!      
!
program wt_cn_diff_DD3

  use dc_message
  use lumatrix
  use wt_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16, km=16  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=16         ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Temp   ! ����
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp    ! ����
  real(8), dimension((nm+1)**2,0:km) :: wz_Temp    ! ����

  real(8), dimension(0:im-1,jm,0:km) :: xyz_TempBase  ! ���ܾ첹��
  real(8), dimension(0:im-1,jm,0:km) :: xyz_TempInit  ! ����(�����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_TempSol   ! ����(���ϲ�)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_TempError ! ����(����)

  real(8)             :: xy_Tempbndry(0:im-1,jm,2) ! ������
  real(8)             :: w_Tempbndry((nm+1)**2,2)  ! ������

 ! Crank Nicholson ��Ū�׻��ѳȻ�����(����)
  real(8)             :: DifLUMT_Temp((nm+1)*(nm+1),0:lm,0:lm)
  integer             :: kpivot_Temp((nm+1)*(nm+1),0:lm)    ! �ԥܥå�

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100, ndisp=10        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  character(len=2), parameter :: TempBC='DD'    ! ���ٶ������(DD/DN/ND/NN)
  real(8), parameter :: nu=1.0                  ! Ǵ������

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! ư���ȿ�
  real(8) :: sigma                   ! ��ĹΨ

 !---- ����¾ ----
  integer :: it=0
  real(8) :: time=0                  ! ����
  integer :: n,m,l                   ! �ȿ�
  integer :: nn,mm,i,j,k             ! Do loop

 !---------------- ��ɸ�ͤ����� ---------------------
  call wt_Initial(im,jm,km,nm,lm,ri,ro)

 !------------------- ���������� ----------------------
  xy_TempBndry(:,:,1) = cos(xy_Lat)
  xy_TempBndry(:,:,2) =0.0

  w_Tempbndry(:,1) = w_xy(xy_Tempbndry(:,:,1))
  w_Tempbndry(:,2) = w_xy(xy_Tempbndry(:,:,2))

  call CNDiffusionMatrix( nu, dt, DifLUMT_Temp, kpivot_Temp )

 !------------------- ��������� ----------------------
  write(6,*)'n,m,l?'
  read(5,*)n,m,l

  alpha = CalAlpha(n,l)
  sigma = -nu* alpha **2
  write(6,*) alpha, sigma

  do nn=0,nm
     do mm=-nn,nn
        wz_Temp(l_nm(nn,mm),:)=1.0D0/(ro**nn * ri**(-nn-1) - ri**nn * ro**(-nn-1))&
             *(    (ri**(-nn-1)*z_Rad**nn - ri**nn * z_Rad**(-nn-1))  &
                 * w_TempBndry(l_nm(nn,mm),1) &
                 + (-ro**(-nn-1)*z_Rad**nn + ri**nn * z_Rad**(-nn-1)) &
                     * w_TempBndry(l_nm(nn,mm),2) &
               )
     end do
  end do
  xyz_TempBase = xyz_wz(wz_Temp)

  wz_Temp = 0.0
  do k=0,km
     wz_Temp(l_nm(n,m),k) = -sbn(n,alpha*ri)*sbj(n,alpha*z_Rad(k)) &
                            +sbj(n,alpha*ri)*sbn(n,alpha*z_Rad(k))
  enddo
  xyz_Temp = xyz_wz(wz_Temp)
  xyz_TempInit = xyz_Temp

  xyz_Temp = xyz_Temp + xyz_TempBase
  wt_Temp = wt_xyz(xyz_Temp)

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ
     time = it * dt
     wt_Temp = wt_Temp + dt/2 *( nu * wt_Lapla_wt(wt_Temp) )
     call wt_BoundariesGrid(wt_Temp,w_Tempbndry,cond=TempBC)
     wt_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,wt_Temp)

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

  function sbn(n,x)
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbn

    sbn = b(n,x)*sin(x) - a(n,x)*cos(x)
  end function sbn

   !-----------------------------------------
   ! sbj(ri*x)sbn(ro*x)-sbn(ri*x)sbj(ro*x) �β�����
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

      ValS = sbj(l,ri*xs)*sbn(l,ro*xs) - sbn(l,ri*xs)*sbj(l,ro*xs)
      ValL = sbj(l,ri*xl)*sbn(l,ro*xl) - sbn(l,ri*xl)*sbj(l,ro*xl)
      if ( ValS * ValL .GT. 0.0D0 ) &
           call MessageNotify('E','InvXtanX',&
           'Initial values of ValS and ValL are the same sign.')
!!$          write(6,*) 'vals, vall',vals, vall
1000  xm = (xs + xl)/2.0
      ValM = sbj(l,ri*xm)*sbn(l,ro*xm) - sbn(l,ri*xm)*sbj(l,ro*xm)
      
      if ( ValS * ValM .GT. 0.0D0 ) then
         xs = xm ; ValS=sbj(l,ri*xs)*sbn(l,ro*xs) - sbn(l,ri*xs)*sbj(l,ro*xs)
      else
         xl = xm ; ValL=sbj(l,ri*xl)*sbn(l,ro*xl) - sbn(l,ri*xl)*sbj(l,ro*xl)
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
    real(8), dimension((nm+1)*(nm+1),0:lm)           :: wt_I
    real(8), dimension((nm+1)*(nm+1),0:lm)           :: wt_DI
    real(8), dimension((nm+1)*(nm+1),2)              :: w_Bndry

    integer :: l, i

    DiffLUMatrix = 0.0
    w_Bndry  = 0.0                              ! �����Ѳ��ζ������� 0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
       wt_DI =  - Diffc * dt/2.0 * wt_Lapla_wt(wt_I) 
       call wt_Boundaries(wt_DI,w_Bndry,cond=TempBC)
       DiffLUMatrix(:,:,l) = wt_I + wt_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrix

 !------------------- ���ϥ롼���� ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='wt_cn_diff_DD3.nc', &
           title='Diffusion model in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_Lon/pi*180)                       ! �ѿ�����
    call HistoryPut('lat',y_Lat/pi*180)                       ! �ѿ�����
    call HistoryPut('rad',z_Rad)                              ! �ѿ�����

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
    xyz_Temp = xyz_wt(wt_Temp)
    xyz_TempSol = xyz_TempBase + xyz_TempInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('temp',xyz_Temp)
    call HistoryPut('tempsol',xyz_TempSol)

    where ( xyz_TempSol .ne. 0.0d0 )
       xyz_TempError = Abs(xyz_Temp-xyz_TempSol)/Abs(xyz_TempSol)
    elsewhere
       xyz_TempError = Abs(xyz_Temp-xyz_TempSol)
    end where
    call HistoryPut('temperror',xyz_TempError)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program wt_cn_diff_DD3