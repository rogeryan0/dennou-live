!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_zonal_galerkin_module_sjpack サンプルプログラム
!      球殻内の温度拡散問題(Crank-Nicolson scheme)
!      両側で温度固定条件(T=0)
!
!      計算結果比較のための解析解については解説文書
!
!         「3 次元球殻領域での拡散型方程式の解析解」
!
!      (wt_diffusion.pdf)を参照のこと. 
!
!履歴  2009/10/04  竹広真一 wt_zonal_galerkin_DD_cn_diff.f90 より sjpack 用に改造
!
program wt_zonal_galerkin_module_sjpack_DD_cn_diff
  !
  !  wt_zonal_galerkin_module_sjapck サンプルプログラム
  !      球殻内の温度拡散問題(Crank-Nicolson scheme)
  !      両側で温度固定条件(T=0)
  !
  !      計算結果比較のための解析解については解説文書
  !
  !         「3 次元球殻領域での拡散型方程式の解析解」
  !
  !      (wt_diffusion.pdf)を参照のこと. 
  !
  use dc_message
  use lumatrix
  use wt_zonal_galerkin_module_sjpack
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer,parameter  :: im=1, jm=16, km=32   ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=20         ! 切断波数の設定(水平, 動径)

 !---- 座標変数など ----
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

 !---- 変数 ----
  real(8), dimension(im,jm,0:km)     :: xyz_Temp   ! 温度
  real(8), dimension(nm+1,2:lm)      :: wd_Temp    ! 温度
  real(8), dimension(nm+1,0:km)      :: wz_Temp    ! 温度

  real(8), dimension(im,jm,0:km)     :: xyz_TempInit   ! 温度(初期値)
  real(8), dimension(im,jm,0:km)     :: xyz_TempSol    ! 温度(解析解)

 ! Crank Nicholson 設定
  real(8)             :: DifLUMT_Temp(nm+1,0:lm,0:lm)
                                   ! Crank Nicholson 陰的計算用拡散行列(温度)
  integer             :: kpivot_Temp(nm+1,0:lm)    
                                   ! Crank Nicholson 陰的計算用拡散行列ピボット

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-3                 ! 時間ステップ間隔
  integer, parameter :: nt=200, ndisp=20        ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter :: kappa=1.0                  ! 粘性係数

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! 動径波数
  real(8) :: sigma                   ! 成長率

 !---- その他 ----
  integer :: it=0
  real(8) :: time=0                  ! 時間
  integer :: k 
  integer :: n,l                     ! 波数

 !---------------- 座標値の設定 ---------------------
  call wt_galerkin_Initial(im,jm,km,nm,lm,ri,ro,DD=.true.)

  call CNDiffusionMatrixDD( kappa, dt, DifLUMT_Temp, kpivot_Temp )

 !------------------- 初期値設定 ----------------------
  write(6,*) 'Total horizontal, radial wavenumbers: n,l?'
  read(5,*)n,l

  alpha = CalAlpha(n,l)
  sigma = -kappa* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Growth rate       : ', sigma

  wz_Temp = 0.0
  do k=0,km
     wz_Temp(l_nm(n,0),k) = -sbn(n,alpha*ri)*sbj(n,alpha*z_Rad(k)) &
                            +sbj(n,alpha*ri)*sbn(n,alpha*z_Rad(k))
  enddo
  xyz_Temp = xyz_wz(wz_Temp)
  xyz_TempInit = xyz_Temp
  wd_Temp = wd_wt(wt_xyz(xyz_Temp))

  call output_gtool4_init
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Euler 法による時間積分
     time = it * dt
     wd_Temp = wd_Temp + dt/2 * wd_wt( kappa * wt_Lapla_wt(wt_wd(wd_Temp)) )
     wd_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,wd_Temp)

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
    !
    ! 球ベッセル函数の係数 a_n(x)
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
    ! 球ベッセル函数の係数 b_n(x)
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
    ! 第1種球ベッセル関数
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbj

    sbj = a(n,x)*sin(x) + b(n,x)*cos(x)
  end function sbj

  function sbn(n,x)
    !
    ! 第2種球ベッセル関数
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbn

    sbn = b(n,x)*sin(x) - a(n,x)*cos(x)
  end function sbn

  function sbj1(n,x)
    !
    ! 第 1 種球ベッセル関数の 1 階微分
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbj1

    sbj1 = n*sbj(n,x)/x - sbj(n+1,x)
  end function sbj1

  function sbn1(n,x)
    !
    ! 第 2 種球ベッセル関数の 1 階微分
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbn1

    sbn1 = n*sbn(n,x)/x - sbn(n+1,x)
  end function sbn1

  function det(l,x)
    !
    ! 係数行列式の計算
    ! Δ=sbj(ri*x)sbn(ro*x)-sbn(ri*x)sbj(ro*x)
    !
    integer, intent(IN) :: l                 ! 次数
    real(8), intent(IN) :: x                 ! 
    real(8)             :: det               ! 行列式

    det=sbj(l,ri*x)*sbn(l,ro*x) - sbn(l,ri*x)*sbj(l,ro*x)

  end function det

  function CalAlpha(l,n)
    !
    ! sbj(ri*x)sbn(ro*x)-sbn(ri*x)sbj(ro*x) の解を求める
    !
    integer, intent(IN) :: l                 ! 次数
    integer, intent(IN) :: n                 ! 求める解の番号
    real(8)             :: CalAlpha
    real(8), parameter  :: eps = 1.0D-14     ! 解の精度

    real(8) :: PI
    real(8) :: xs, xl, xm
    real(8) :: ValS, ValL, ValM

    real(8) :: dx
    integer :: nx=20
    integer :: nn

    nn = n
    PI = atan(1.0D0)*4.0D0
    dx = PI/(ro-ri)/nx
    xl = dx ; ValL = det(l,xl)

    !
    ! det = 0 となる点を x=0 から刻 dx の幅で探していき
    ! n 番目を挟む xl, xs を設定する.
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
       xs = xm ; ValS=det(l,xs)
    else
       xl = xm ; ValL=det(l,xl)
    endif

    if ( abs(xl-xs) .lt. eps ) then
       CalAlpha = xm
       goto 99
    endif

    goto 1000

99  continue
  end function CalAlpha


   !------------------- 拡散項 ----------------------
    subroutine CNDiffusionMatrixDD( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson 拡散陰的計算用行列の設定
      ! (1-D dt/2▽^2, LU 分解)
      ! 
      real(8), intent(IN)   :: Diffc    ! 拡散係数
      real(8), intent(IN)   :: dt       ! 時間刻

      real(8), intent(OUT)  :: DiffLUMatrix(nm+1,2:lm,2:lm)
                              ! Crank Nicholson 拡散陰的計算用行列
                              ! (1-D dt/2▽^2, LU 分解)
      integer, intent(OUT)  :: kpivot(nm+1,2:lm)
                              ! Crank Nicholson 拡散陰的計算用行列ピボット情報

      real(8)               :: wd_I(nm+1,2:lm)       ! 作業用変数

      integer :: l

      DiffLUMatrix = 0.0
      do l=2,lm
         wd_I = 0.0 ; wd_I(:,l) = 1.0             ! 各波数成分独立
         DiffLUMatrix(:,:,l) &
              = wd_I - Diffc * dt/2.0 * wd_wt(wt_Lapla_wt(wt_wd(wd_I)))
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixDD

 !------------------- 出力ルーチン ----------------------
  subroutine output_gtool4_init
    !
    ! ヒストリー出力初期化ルーチン
    !
    !   ファイル作成
    !   変数定義
    !
    call HistoryCreate( &                                  ! ヒストリー作成
           file='wt_zonal_galerkin_module_sjpack_DD_cn_diff.nc', &
           title='Diffusion model in a spherical shell (axially symmetric)', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lat','rad','t  '/), dimsizes=(/jm,km+1,0/),&
           longnames=(/'Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lat',y_Lat/pi*180)                       ! 変数出力
    call HistoryPut('rad',z_Rad)                              ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
           varname='temp', dims=(/'lat','rad','t  '/), & 
           longname='temperature', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='tempsol', dims=(/'lat','rad','t  '/), & 
           longname='temperature(analytic)', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='temperror', dims=(/'lat','rad','t  '/), & 
           longname='temperature(error)', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    !
    ! ヒストリー出力
    !
    !   各物理量の gtool4 ファイルへの出力を行う
    !
    write(6,*) 'it = ',it
    xyz_Temp = xyz_wt(wt_wd(wd_Temp))
    xyz_TempSol = xyz_TempInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('temp',xyz_Temp(1,:,:))
    call HistoryPut('tempsol',xyz_TempSol(1,:,:))
    call HistoryPut('temperror',xyz_Temp(1,:,:)-xyz_TempSol(1,:,:))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    !
    ! ヒストリー出力終了処理
    !
    call HistoryClose
  end subroutine output_gtool4_close

end program wt_zonal_galerkin_module_sjpack_DD_cn_diff
