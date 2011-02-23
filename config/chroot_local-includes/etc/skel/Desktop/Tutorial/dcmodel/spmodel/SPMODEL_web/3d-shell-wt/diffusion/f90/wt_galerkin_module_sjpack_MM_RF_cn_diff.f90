!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_galerkin_module_sjpack サンプルプログラム
!      球殻内のトロイダル速度場拡散問題(Crank-Nicolson scheme)
!      外側粘着内側自由すべり条件
!
!      計算結果比較のための解析解については解説文書
!
!         「3 次元球殻領域での拡散型方程式の解析解」
!
!      (wt_diffusion.pdf)を参照のこと. 
!
!履歴  2009/09/18  竹広真一  wt_galerkin_MM_RS_cn_diff.f90 を sjpack 用に改造
!                            境界条件スイッチ名を S -> F に変更
!
program wt_galerkin_module_sjpack_MM_RF_cn_diff
  !
  !  wt_galerkin_module_sjpack サンプルプログラム
  !      球殻内のトロイダル速度場拡散問題(Crank-Nicolson scheme)
  !      外側粘着内側自由すべり条件
  !
  !      計算結果比較のための解析解については解説文書
  !
  !         「3 次元球殻領域での拡散型方程式の解析解」
  !
  !      (wt_diffusion.pdf)を参照のこと. 
  !
  use dc_message
  use lumatrix
  use wt_galerkin_module_sjpack
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=10         ! 切断波数の設定(水平, 動径)

 !---- 座標変数など ----
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

 !---- 変数 ----
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Torvel   ! トロイダル速度場
  real(8), dimension((nm+1)**2,2:lm) :: wf_Torvel    ! トロイダル速度場
  real(8), dimension((nm+1)**2,0:km) :: wz_Torvel    ! トロイダル速度場

  real(8), dimension(0:im-1,jm,0:km) :: xyz_TorvelInit !トロイダル速度場(初期値)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_TorvelSol  !トロイダル速度場(解析解)

 ! Crank Nicholson 設定
  real(8)             :: DifLUMT_Torvel((nm+1)*(nm+1),0:lm,0:lm)
                         ! Crank Nicholson 陰的計算用拡散行列(トロイダル速度場)
  integer             :: kpivot_Torvel((nm+1)*(nm+1),0:lm)
                         ! Crank Nicholson 陰的計算用拡散行列ピボット

 ! 境界条件設定用配列
  real(8)             :: cfdx0_ri           ! 境界条件設定用配列(0階微分@r=ri)
  real(8)             :: cfdx1_ri           ! 境界条件設定用配列(1階微分@r=ri)
  real(8)             :: cfdx0_ro           ! 境界条件設定用配列(0階微分@r=ro)
  real(8)             :: cfdx1_ro           ! 境界条件設定用配列(1階微分@r=ro)

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-3                 ! 時間ステップ間隔
  integer, parameter :: nt=200, ndisp=20        ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter :: nu=1.0                  ! 粘性係数

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! 動径波数
  real(8) :: sigma                   ! 成長率

 !---- その他 ----
  integer :: it=0
  real(8) :: time=0                  ! 時間
  integer :: k 
  integer :: n,m,l                   ! 波数

 !---------------- 座標値の設定 ---------------------
  cfdx1_ri = 1.0D0  ; cfdx0_ri = -1.0D0/ri
  cfdx1_ro = 0.0D0  ; cfdx0_ro =  1.0D0

  call wt_galerkin_Initial(im,jm,km,nm,lm,ri,ro,     &
         MM_cfdx0_ri=cfdx0_ri, MM_cfdx1_ri=cfdx1_ri, &
         MM_cfdx0_ro=cfdx0_ro, MM_cfdx1_ro=cfdx1_ro   )

  call CNDiffusionMatrixMM( nu, dt, DifLUMT_Torvel, kpivot_Torvel )

 !------------------- 初期値設定 ----------------------
  write(6,*) 'Total horizontal, longitudinal, radial wavenumbers: n,m,l?'
  read(5,*)n,m,l

  alpha = CalAlpha(n,l)
  sigma = -nu* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Growth rate       : ', sigma

  wz_Torvel = 0.0
  do k=0,km
     wz_Torvel(l_nm(n,m),k) &
        = - sbn(n,ro*alpha)*sbj(n,alpha*z_Rad(k)) &
          + sbj(n,ro*alpha)*sbn(n,alpha*z_Rad(k))
  enddo
  xyz_Torvel = xyz_wz(wz_Torvel)
  xyz_TorvelInit = xyz_Torvel
  wf_Torvel = wf_wt(wt_xyz(xyz_Torvel))

  write(6,*) wf_Torvel(l_nm(n,m),:)

  call output_gtool4_init
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Euler 法による時間積分
     time = it * dt
     wf_Torvel = wf_Torvel + dt/2 * wf_wt( nu * wt_Lapla_wt(wt_wf(wf_Torvel)) )
     wf_Torvel = LUSolve(DifLUMT_Torvel,kpivot_Torvel,wf_Torvel)

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
    ! 係数行列式Δの計算
    !
    integer, intent(IN) :: l                 ! 次数
    real(8), intent(IN) :: x                 ! 
    real(8)             :: det               ! 行列式

    det=(x*sbj1(l,ri*x)-sbj(l,ri*x)/ro)*sbn(l,ro*x) &
       -(x*sbn1(l,ri*x)-sbn(l,ri*x)/ro)*sbj(l,ro*x)

  end function det

  function CalAlpha(l,n)
    !
    ! Δ=0 の解を求める
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
    
    !------------------- 拡散項 ----------------------
    subroutine CNDiffusionMatrixMM( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson 拡散陰的計算用行列の設定
      ! (1-D dt/2▽^2, LU 分解)
      ! 
      real(8), intent(IN)   :: Diffc    ! 拡散係数
      real(8), intent(IN)   :: dt       ! 時間刻

      ! Crank Nicholson 設定
      real(8), intent(OUT)  :: DiffLUMatrix((nm+1)*(nm+1),2:lm,2:lm)
                              ! Crank Nicholson 拡散陰的計算用行列
                              ! (1-D dt/2▽^2, LU 分解)
      integer, intent(OUT)  :: kpivot((nm+1)*(nm+1),2:lm)     ! ピボット情報
                              ! Crank Nicholson 拡散陰的計算用行列ピボット情報

      real(8)               :: wf_I((nm+1)*(nm+1),2:lm)       ! 作業用変数

      integer :: l

      DiffLUMatrix = 0.0
      do l=2,lm
         wf_I = 0.0 ; wf_I(:,l) = 1.0             ! 各波数成分独立
         DiffLUMatrix(:,:,l) &
              = wf_I - Diffc * dt/2.0 * wf_wt(wt_Lapla_wt(wt_wf(wf_I)))
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixMM

 !------------------- 出力ルーチン ----------------------
  subroutine output_gtool4_init
    !
    ! ヒストリー出力初期化ルーチン
    !
    !   ファイル作成
    !   変数定義
    !
    call HistoryCreate( &                                  ! ヒストリー作成
           file='wt_galerkin_module_sjpack_MM_RF_cn_diff.nc', &
           title='Diffusion model in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_Lon/pi*180)                       ! 変数出力
    call HistoryPut('lat',y_Lat/pi*180)                       ! 変数出力
    call HistoryPut('rad',z_Rad)                              ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
           varname='torvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='Toroidal velocity field', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='torvelsol', dims=(/'lon','lat','rad','t  '/), & 
           longname='Toroidal velocity field(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='torvelerror', dims=(/'lon','lat','rad','t  '/), & 
           longname='Toroidal velocity field(error)', &
           units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    !
    ! ヒストリー出力
    !
    !   各物理量の gtool4 ファイルへの出力を行う
    !
    write(6,*) 'it = ',it
    xyz_Torvel = xyz_wt(wt_wf(wf_Torvel))
    xyz_TorvelSol = xyz_TorvelInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('torvel',xyz_Torvel)
    call HistoryPut('torvelsol',xyz_TorvelSol)
    call HistoryPut('torvelerror',xyz_Torvel-xyz_TorvelSol)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    !
    ! ヒストリー出力終了処理
    !
    call HistoryClose
  end subroutine output_gtool4_close

end program wt_galerkin_module_sjpack_MM_RF_cn_diff
