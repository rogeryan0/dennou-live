!----------------------------------------------------------------------
!     Copyright (c) 2008--2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wq_module サンプルプログラム
!      球内のポロイダル速度場拡散問題(Crank-Nicolson scheme)
!      外側すべり条件
!
!      計算結果比較のための解析解については解説文書
!
!         「3 次元球領域での拡散型方程式の解析解」
!
!      (wu_diffusion.pdf)を参照のこと. 
!
!注意  ***** wr_LaplaPol2Pol_wr 未完成のため使用禁止 *****
!
!履歴  2008/04/05  竹広真一
!      2008/07/20  竹広真一  wq_Lapla_wq に変更
!      2008/08/10  竹広真一  配列添え字変更 im -> 0:im-1
!      2009/09/18  竹広真一  球ベッセル係数バグフィックス
!
program wq_diff_cn_polvel_grid_F
  !
  !  wq_module サンプルプログラム
  !      球内のポロイダル速度場拡散問題(Crank-Nicolson scheme)
  !      外側すべり粘着条件
  !
  !      計算結果比較のための解析解については解説文書
  !
  !         「3 次元球領域での拡散型方程式の解析解」
  !
  !      (wu_diffusion.pdf)を参照のこと. 
  !
  use dc_message
  use lumatrix
  use wq_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer,parameter  :: im=32, jm=16, km=8   ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=15         ! 切断波数の設定(水平, 動径)

 !---- 座標変数など ----
  real(8),parameter  :: ra=1.5               ! 球半径

 !---- 変数 ----
  real(8), dimension(0:im-1,jm,km) :: xyr_Polvel     ! ポロイダル速度場
  real(8), dimension((nm+1)**2,km) :: wr_Polvel      ! ポロイダル速度場
  real(8), dimension((nm+1)**2,km) :: wr_LaplaPolvel ! ▽^2ポロイダル

  real(8), dimension(0:im-1,jm,km) :: xyr_PolvelInit  ! ポロイダル速度場(初期値)
  real(8), dimension(0:im-1,jm,km) :: xyr_PolvelSol   ! ポロイダル速度場(解析解)

 ! Crank Nicholson 設定
  real(8)             :: DifLUMT_Polvel((nm+1)*(nm+1),km,km)
                         ! Crank Nicholson 陰的計算用拡散行列(ポロイダル速度場)
  integer             :: kpivot_Polvel((nm+1)*(nm+1),km)
                         ! Crank Nicholson 陰的計算用拡散行列ピボット

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-3                 ! 時間ステップ間隔
  integer, parameter :: nt=100, ndisp=10        ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter :: nu=1.0                  ! 粘性係数

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! 動径波数
  real(8) :: sigma                   ! 成長率
  real(8) :: C(2)                    ! 独立解の係数

 !---- その他 ----
  integer :: it=0
  real(8) :: time=0                  ! 時間
  integer :: k 
  integer :: n,m,l                   ! 波数

 !---------------- 座標値の設定 ---------------------
  call wq_Initial(im,jm,km,nm,lm,ra)

  call CNDiffusionMatrixPolvel( nu, dt, DifLUMT_Polvel, kpivot_Polvel )

 !------------------- 初期値設定 ----------------------
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

 !------------------- 時間積分 ----------------------
  do it=1,nt                               ! Crank-Nicolson 法による時間積分
     time = it * dt

     wr_LaplaPolvel = wr_LaplaPolvel &
          + dt/2 * nu * wr_wq(wq_Lapla_wq(wq_wr(wr_LaplaPolvel)))
     wr_Polvel = wr_LaplaPol2Pol_wr(wr_LaplaPolvel,cond='F')
     wr_Polvel = LuSolve(DifLUMT_Polvel,kpivot_Polvel,wr_Polvel)
     wr_LaplaPolvel = wr_wq(wq_Lapla_wq(wq_wr(wr_Polvel)))

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

  function sbj2(n,x)
    !
    ! 第 1 種球ベッセル関数の 1 階微分
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbj2

    sbj2 = -2*sbj1(n,x)/x - (1-n*(n+1)/x**2)*sbj(n,x)
  end function sbj2

  recursive function detNxN(a) result(determinant)
    !
    ! NxN 行列の行列式の計算
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
    ! 係数行列式Δの計算
    !
    integer, intent(IN) :: l                 ! 次数
    real(8), intent(IN) :: x                 ! 
    real(8)             :: det               ! 行列式
    real(8)             :: a(2,2)            ! 行列

    a(1,1) = ra**l             ; a(1,2) = sbj(l,ra*x)
    a(2,1) = l*(l-1)*ra**(l-2) ; a(2,2) = x**2*sbj2(l,ra*x)

    det=detNxN(a)

  end function det

  function CalcCoeff(l,alpha)
    !
    integer, intent(IN) :: l                 ! 次数
    real(8), intent(IN) :: alpha             ! 動径波数
    real(8)             :: CalcCoeff(2)      ! 係数

    CalcCoeff(1) = sbj(l,ra*alpha) 
    CalcCoeff(2) = -ra**l

  end function CalcCoeff

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
    dx = PI/ra/nx
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
    subroutine CNDiffusionMatrixPolvel( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson 拡散陰的計算用行列の設定
      ! (1-D dt/2▽^2, LU 分解)
      ! 
      real(8), intent(IN)   :: Diffc    ! 拡散係数
      real(8), intent(IN)   :: dt       ! 時間刻

      ! Crank Nicholson 設定
      real(8), intent(OUT)  :: DiffLUMatrix((nm+1)*(nm+1),km,km)
                              ! Crank Nicholson 拡散陰的計算用行列
                              ! (1-D dt/2▽^2, LU 分解)

      integer, intent(OUT)  :: kpivot((nm+1)*(nm+1),km)     ! ピボット情報
                              ! Crank Nicholson 拡散陰的計算用行列ピボット情報

      real(8)               :: wr_I((nm+1)*(nm+1),km)        ! 作業用変数
      real(8)               :: wr_DI((nm+1)*(nm+1),km)       ! 作業用変数

      integer :: k

      DiffLUMatrix = 0.0
      do k=1,km
         wr_I = 0.0 ; wr_I(:,k) = 1.0             ! 各波数成分独立
         wr_DI = - Diffc * dt/2.0 &
                  * wr_wq(wq_Lapla_wq(wq_Lapla_wq(wq_wr(wr_I))))
         wr_DI = wr_LaplaPol2Pol_wr(wr_DI,cond='F')
         DiffLUMatrix(:,:,k) = wr_I + wr_DI
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixPolvel

 !------------------- 出力ルーチン ----------------------
  subroutine output_gtool4_init
    !
    ! ヒストリー出力初期化ルーチン
    !
    !   ファイル作成
    !   変数定義
    !
    call HistoryCreate( &                                  ! ヒストリー作成
           file='wq_diff_cn_polvel_grid_F.nc', &
           title='Diffusion model in a sphere', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_Lon/pi*180)                       ! 変数出力
    call HistoryPut('lat',y_Lat/pi*180)                       ! 変数出力
    call HistoryPut('rad',r_Rad)                              ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
           varname='polvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal velocity field', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='polvelsol', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal velocity field(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='polvelerror', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal velocity field(error)', &
           units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    !
    ! ヒストリー出力
    !
    !   各物理量の gtool4 ファイルへの出力を行う
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
    ! ヒストリー出力終了処理
    !
    call HistoryClose
  end subroutine output_gtool4_close

end program wq_diff_cn_polvel_grid_F
