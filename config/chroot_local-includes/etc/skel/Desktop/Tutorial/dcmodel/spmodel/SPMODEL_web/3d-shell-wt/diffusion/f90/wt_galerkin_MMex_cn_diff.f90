!----------------------------------------------------------------------
!     Copyright (c) 2006--2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_galerkin_module サンプルプログラム
!      球殻内のポロイダル磁場拡散問題(Crank-Nicolson scheme)
!
!      計算結果比較のための解析解については解説文書
!
!         「3 次元球殻領域での拡散型方程式の解析解」
!
!      (wt_diffusion.pdf)を参照のこと. 
!
!履歴  2006/02/19  竹広真一
!      2006/03/02  竹広真一  RDoc 用にコメント修正
!      2008/08/13  竹広真一  配列添え字変更 im -> 0:im-1
!
program wt_galerkin_MMex_cn_diff
  !
  !  wt_galerkin_module サンプルプログラム
  !      球殻内のポロイダル磁場拡散問題(Crank-Nicolson scheme)
  !
  !      計算結果比較のための解析解については解説文書
  !
  !         「3 次元球殻領域での拡散型方程式の解析解」
  !
  !      (wt_diffusion.pdf)を参照のこと. 
  !
  use dc_message
  use lumatrix
  use wt_galerkin_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer,parameter  :: im=32, jm=16, km=32  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=20         ! 切断波数の設定(水平, 動径)

 !---- 座標変数など ----
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

 !---- 変数 ----
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Polmag   ! ポロイダル磁場
  real(8), dimension((nm+1)**2,2:lm) :: wh_Polmag    ! ポロイダル磁場
  real(8), dimension((nm+1)**2,0:km) :: wz_Polmag    ! ポロイダル磁場

  real(8), dimension(0:im-1,jm,0:km) :: xyz_PolmagInit ! ポロイダル磁場(初期値)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_PolmagSol  ! ポロイダル磁場(解析解)

 ! Crank Nicholson 設定
  real(8)             :: DifLUMT_Polmag((nm+1)*(nm+1),0:lm,0:lm)
                         ! Crank Nicholson 陰的計算用拡散行列(ポロイダル磁場)
  integer             :: kpivot_Polmag((nm+1)*(nm+1),0:lm)   
                         ! Crank Nicholson 陰的計算用拡散行列ピボット
 ! 境界条件設定用配列
  real(8)      :: cfdx0_ri((nm+1)*(nm+1))    ! 境界条件設定用配列(0階微分@r=ri)
  real(8)      :: cfdx1_ri((nm+1)*(nm+1))    ! 境界条件設定用配列(1階微分@r=ri)
  real(8)      :: cfdx0_ro((nm+1)*(nm+1))    ! 境界条件設定用配列(0階微分@r=ro)
  real(8)      :: cfdx1_ro((nm+1)*(nm+1))    ! 境界条件設定用配列(1階微分@r=ro)

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
  integer :: n,m,l                   ! 波数

 !---------------- 座標値の設定 ---------------------
  do n=0,nm
     do m=-n,n
        cfdx1_ri(l_nm(n,m))  = 1.0D0
        cfdx0_ri(l_nm(n,m)) = -n/ri
        cfdx1_ro(l_nm(n,m)) = 1.0D0
        cfdx0_ro(l_nm(n,m)) = (n+1)/ro
     enddo
  enddo

  call wt_galerkin_Initial(im,jm,km,nm,lm,ri,ro,             &
         MMex_cfdx0_ri=cfdx0_ri, MMex_cfdx1_ri=cfdx1_ri, &
         MMex_cfdx0_ro=cfdx0_ro, MMex_cfdx1_ro=cfdx1_ro   )

  call CNDiffusionMatrixMMex( kappa, dt, DifLUMT_Polmag, kpivot_Polmag )

 !------------------- 初期値設定 ----------------------
  write(6,*)'n,m,l?'
  read(5,*)n,m,l

  alpha = CalAlpha(n,l)
  sigma = -kappa* alpha **2
  write(6,*) alpha, sigma

  wz_Polmag = 0.0
  do k=0,km
     wz_Polmag(l_nm(n,m),k) = -sbn(n+1,alpha*ri)*sbj(n,alpha*z_Rad(k)) &
                            +sbj(n+1,alpha*ri)*sbn(n,alpha*z_Rad(k))
  enddo
  xyz_Polmag = xyz_wz(wz_Polmag)
  xyz_PolmagInit = xyz_Polmag
  wh_Polmag = wh_wt(wt_xyz(xyz_Polmag))

  call output_gtool4_init
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Euler 法による時間積分
     time = it * dt
     wh_Polmag = wh_Polmag + dt/2 * wh_wt( kappa * wt_Lapla_wt(wt_wh(wh_Polmag)) )
     wh_Polmag = LUSolve(DifLUMT_Polmag,kpivot_Polmag,wh_Polmag)

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

  function det(l,x)
    !
    ! 係数行列式の計算
    ! Δ=sbj(l+1,ri*x)sbn(l-1,ro*x)-sbn(l+1,ri*x)sbj(l-1,ro*x)
    !
    integer, intent(IN) :: l                 ! 次数
    real(8), intent(IN) :: x                 ! 
    real(8)             :: det               ! 行列式

    det=sbj(l+1,ri*x)*sbn(l-1,ro*x) - sbn(l+1,ri*x)*sbj(l-1,ro*x)    
  end function det

  function CalAlpha(l,n)
    !
    ! Δ=sbj(ri*x)sbn(ro*x)-sbn(ri*x)sbj(ro*x) の解を求める
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
    subroutine CNDiffusionMatrixMMex( Diffc, dt, DiffLUMatrix, kpivot )
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

      real(8)               :: wh_I((nm+1)*(nm+1),2:lm)       ! 作業用変数

      integer :: l

      DiffLUMatrix = 0.0
      do l=2,lm
         wh_I = 0.0 ; wh_I(:,l) = 1.0             ! 各波数成分独立
         DiffLUMatrix(:,:,l) &
              = wh_I - Diffc * dt/2.0 * wh_wt(wt_Lapla_wt(wt_wh(wh_I)))
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixMMex

 !------------------- 出力ルーチン ----------------------
  subroutine output_gtool4_init
    !
    ! ヒストリー出力初期化ルーチン
    !
    !   ファイル作成
    !   変数定義
    !
    call HistoryCreate( &                                  ! ヒストリー作成
           file='wt_galerkin_MMex_cn_diff.nc', &
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
           varname='polmag', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal Magnetic field', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='polmagsol', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal Magnetic field(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='polmagerror', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal Magnetic field(error)', &
           units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    !
    ! ヒストリー出力
    !
    !   各物理量の gtool4 ファイルへの出力を行う
    !
    write(6,*) 'it = ',it
    xyz_Polmag = xyz_wt(wt_wh(wh_Polmag))
    xyz_PolmagSol = xyz_PolmagInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('polmag',xyz_Polmag)
    call HistoryPut('polmagsol',xyz_PolmagSol)
    call HistoryPut('polmagerror',xyz_Polmag-xyz_PolmagSol)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    !
    ! ヒストリー出力終了処理
    !
    call HistoryClose
  end subroutine output_gtool4_close

end program wt_galerkin_MMex_cn_diff
