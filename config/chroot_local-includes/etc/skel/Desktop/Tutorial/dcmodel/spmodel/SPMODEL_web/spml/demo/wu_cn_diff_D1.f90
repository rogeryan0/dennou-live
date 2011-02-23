!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wu_module
!      球内の温度拡散問題(Crank-Nicolson scheme)
!
!      境界の値が 0 の場合
!      チェビシェフータウ法による境界条件適用
!
!      計算結果比較のための解析解については解説文書
!
!         「3 次元球領域での拡散型方程式の解析解」
!
!      (wu_diffusion.pdf)を参照のこと. 
!
!履歴  2008/01/03  竹広真一
!
program wu_cn_diff_D1

  use dc_message
  use lumatrix
  use wu_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)

 !---- 座標変数など ----
  real(8),parameter  :: ra=1.5               ! 球半径

 !---- 変数 ----
  real(8), dimension(im,jm,0:km)     :: xyr_Temp   ! 温度
  real(8), dimension((nm+1)**2,0:lm) :: wu_Temp    ! 温度
  real(8), dimension((nm+1)**2,0:km) :: wr_Temp    ! 温度

  real(8), dimension(im,jm,0:km)     :: xyr_TempInit  ! 温度(初期値)
  real(8), dimension(im,jm,0:km)     :: xyr_TempSol   ! 温度(解析解)

  real(8)             :: xy_Tempbndry(im,jm)       ! 境界値
  real(8)             :: w_Tempbndry((nm+1)**2)    ! 境界値

 ! Crank Nicholson 陰的計算用拡散行列(温度)
  real(8)             :: DifLUMT_Temp((nm+1)*(nm+1),0:lm,0:lm)
  integer             :: kpivot_Temp((nm+1)*(nm+1),0:lm)    ! ピボット

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-3                 ! 時間ステップ間隔
  integer, parameter :: nt=100, ndisp=10        ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  character(len=2), parameter :: TempBC='D'     ! 温度境界条件(D/N)
  real(8), parameter :: kappa=1.0                  ! 拡散係数

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! 動径波数
  real(8) :: sigma                   ! 成長率

 !---- その他 ----
  integer :: it=0
  real(8) :: time=0                  ! 時間
  integer :: k 
  integer :: n,m,l                   ! 波数

 !---------------- 座標値の設定 ---------------------
  call wu_Initial(im,jm,km,nm,lm,ra)

  call CNDiffusionMatrix( kappa, dt, DifLUMT_Temp, kpivot_Temp )

 !------------------- 初期値設定 ----------------------
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

 !------------------- 境界値設定 ----------------------
  xy_Tempbndry(:,:) = 0

  w_Tempbndry = w_xy(xy_Tempbndry)

  call output_gtool4_init
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Euler 法による時間積分
     time = it * dt
     wu_Temp = wu_Temp + dt/2 * wu_wr( kappa * wr_Lapla_wu(wu_Temp) )
     call wu_Boundary(wu_Temp,w_Tempbndry,cond=TempBC)
     wu_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,wu_Temp)

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
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
   ! sbj(ra*x)=0 の解を求める
   !
    function CalAlpha(l,n)
      integer, intent(IN) :: l                 ! 次数
      integer, intent(IN) :: n                 ! 求める解の番号
      real(8)             :: CalAlpha
      real(8), parameter  :: eps = 1.0D-14     ! 解の精度

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

 !------------------- 拡散項 ----------------------
  subroutine CNDiffusionMatrix( Diffc, dt, DiffLUMatrix, kpivot )

    real(8), intent(IN)          :: Diffc    ! 拡散係数
    real(8), intent(IN)          :: dt       ! 時間刻

    ! Crank Nicholson 拡散陰的計算用行列(1-D dt/2▽^2, LU 分解)
    real(8), dimension((nm+1)*(nm+1),0:lm,0:lm), intent(OUT)  :: DiffLUMatrix
    ! ピボット情報
    integer, dimension((nm+1)*(nm+1),0:lm), intent(OUT)       :: kpivot

    ! 作業用変数
    real(8), dimension((nm+1)*(nm+1),0:lm)           :: wu_I
    real(8), dimension((nm+1)*(nm+1),0:lm)           :: wu_DI

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wu_I = 0.0 ; wu_I(:,l) = 1.0             ! 各波数成分独立
       wu_DI =  - Diffc * dt/2.0 * wu_wr(wr_Lapla_wu(wu_I))
       call wu_Boundary(wu_DI,cond=TempBC)      ! 変化分は境界条件変化しない
       DiffLUMatrix(:,:,l) = wu_I + wu_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrix

 !------------------- 出力ルーチン ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! ヒストリー作成
           file='wu_cn_diff_D1.nc', &
           title='Diffusion model in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_Lon/pi*180)                       ! 変数出力
    call HistoryPut('lat',y_Lat/pi*180)                       ! 変数出力
    call HistoryPut('rad',r_Rad)                              ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
           varname='temp', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='tempsol', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature(analytic)', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
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
