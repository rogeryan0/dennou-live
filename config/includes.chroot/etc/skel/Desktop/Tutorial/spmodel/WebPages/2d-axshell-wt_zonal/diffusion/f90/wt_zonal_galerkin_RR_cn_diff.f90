!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_zonal_galerkin_module サンプルプログラム
!      球殻内のポロイダル速度場拡散問題(Crank-Nicolson scheme)
!      両側粘着条件
!
!      計算結果比較のための解析解については解説文書
!
!         「3 次元球殻領域での拡散型方程式の解析解」
!
!      (wt_diffusion.pdf)を参照のこと. 
!
!履歴  2009/09/10  竹広真一 wt_galerkin_RR_cn_diff.f90 より軸対称版に改造
!
program wt_zonal_galerkin_RR_cn_diff
  !
  !  wt_zonal_galerkin_module サンプルプログラム
  !      球殻内のポロイダル速度場拡散問題(Crank-Nicolson scheme)
  !      両側粘着条件
  !
  !      計算結果比較のための解析解については解説文書
  !
  !         「3 次元球殻領域での拡散型方程式の解析解」
  !
  !      (wt_diffusion.pdf)を参照のこと. 
  !
  use dc_message
  use lumatrix
  use wt_zonal_galerkin_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer,parameter  :: im=1,  jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)

 !---- 座標変数など ----
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

 !---- 変数 ----
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Polvel     ! ポロイダル速度場
  real(8), dimension(nm+1,4:lm)      :: wq_Polvel      ! ポロイダル速度場
  real(8), dimension(nm+1,0:km)      :: wz_Polvel      ! ポロイダル速度場
  real(8), dimension(nm+1,0:lm)      :: wt_LaplaPolvel ! ▽^2ポロイダル

  real(8), dimension(0:im-1,jm,0:km) :: xyz_PolvelInit  ! ポロイダル速度場(初期値)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_PolvelSol   ! ポロイダル速度場(解析解)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Strfunc     ! 流線関数
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLat        ! 速度緯度成分
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VRad        ! 速度動径成分

 ! Crank Nicholson 設定
  real(8)             :: DifLUMT_Polvel(nm+1,0:lm,0:lm)
                         ! Crank Nicholson 陰的計算用拡散行列(ポロイダル速度場)
  integer             :: kpivot_Polvel(nm+1,0:lm)
                         ! Crank Nicholson 陰的計算用拡散行列ピボット

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-3                 ! 時間ステップ間隔
  integer, parameter :: nt=200, ndisp=20        ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter :: nu=1.0                  ! 粘性係数

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! 動径波数
  real(8) :: sigma                   ! 成長率
  real(8) :: C(4)                    ! 独立解の係数

 !---- その他 ----
  integer :: it=0
  real(8) :: time=0                  ! 時間
  integer :: k 
  integer :: n,m,l                   ! 波数

 !---------------- 座標値の設定 ---------------------
  call wt_galerkin_Initial(im,jm,km,nm,lm,ri,ro,velBC='RR')

  call CNDiffusionMatrixPolvel( nu, dt, DifLUMT_Polvel, kpivot_Polvel )

 !------------------- 初期値設定 ----------------------
  write(6,*) 'Total horizontal, radial wavenumbers: n,l?'
  read(5,*)n,l

  alpha = CalAlpha(n,l)
  sigma = -nu* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Growth rate       : ', sigma

  C = CalcCoeff(n,alpha)

  wz_Polvel = 0.0
  do k=0,km
     wz_Polvel(l_nm(n,0),k) &
        =   C(1)*z_Rad(k)**n           + C(2)*z_Rad(k)**(-(n+1))   &
          + C(3)*sbj(n,alpha*z_Rad(k)) + C(4)*sbn(n,alpha*z_Rad(k))
  enddo

  xyz_Polvel = xyz_wz(wz_Polvel)
  xyz_PolvelInit = xyz_Polvel
  wq_Polvel = wq_wt(wt_xyz(xyz_Polvel))
  wt_LaplaPolvel = wt_Lapla_wt(wt_wq(wq_Polvel))

  call output_gtool4_init
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Euler 法による時間積分
     time = it * dt

     wt_LaplaPolvel = wt_LaplaPolvel &
          + dt/2 *( nu * wt_Lapla_wt(wt_LaplaPolvel)  )
     wq_Polvel = wq_LaplaPol2Pol_wt(wt_LaplaPolvel)
     wq_Polvel = LuSolve(DifLUMT_Polvel,kpivot_Polvel,wq_Polvel)
     wt_LaplaPolvel = wt_Lapla_wt(wt_wq(wq_Polvel))

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
    real(8)             :: a(4,4)            ! 行列

    a(1,1) = ri**l       ; a(1,2) = ri**(-(l+1))
    a(1,3) = sbj(l,ri*x) ; a(1,4) = sbn(l,ri*x)

    a(2,1) = ro**l       ; a(2,2) = ro**(-(l+1))
    a(2,3) = sbj(l,ro*x) ; a(2,4) = sbn(l,ro*x)

    a(3,1) = l*ri**(l-1)    ; a(3,2) = -(l+1)*ri**(-(l+2))
    a(3,3) = x*sbj1(l,ri*x) ; a(3,4) = x*sbn1(l,ri*x)

    a(4,1) = l*ro**(l-1)    ; a(4,2) = -(l+1)*ro**(-(l+2))
    a(4,3) = x*sbj1(l,ro*x) ; a(4,4) = x*sbn1(l,ro*x)

    det=detNxN(a)

  end function det

  function CalcCoeff(l,alpha)
    !
    ! alpha を決めた後の係数の計算  Ax=b を解く
    !
    integer, intent(IN) :: l                 ! 次数
    real(8), intent(IN) :: alpha             ! 動径波数
    real(8)             :: CalcCoeff(4)      ! 係数

    real(8)             :: a(3,3)            ! 連立方程式左辺
    real(8)             :: b(3)              ! 連立方程式右辺
    integer             :: kp(3)             ! ピボット

    a(1,1) = ri**l           ; a(1,2) = ri**(-(l+1))
    a(1,3) = sbj(l,ri*alpha) ; b(1)   = sbn(l,ri*alpha)

    a(2,1) = ro**l           ; a(2,2) = ro**(-(l+1))
    a(2,3) = sbj(l,ro*alpha) ; b(2)   = sbn(l,ro*alpha)

    a(3,1) = l*ri**(l-1)            ; a(3,2) = -(l+1)*ri**(-(l+2))
    a(3,3) = alpha*sbj1(l,ri*alpha) ; b(3) = alpha*sbn1(l,ri*alpha)

    call LUDecomp(a,kp)
    b =  LUSolve(a,kp,b)
    CalcCoeff(1:3) = b ; CalcCoeff(4) = -1

!!$    write(6,*) b(1)*l*ro**(l-1) - b(2)*(l+1)*ro**(-(l+2)) &
!!$             + b(3) * alpha*sbj1(l,ro*alpha) - alpha*sbn1(l,ro*alpha)

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
    subroutine CNDiffusionMatrixPolvel( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson 拡散陰的計算用行列の設定
      ! (1-D dt/2▽^2, LU 分解)
      ! 
      real(8), intent(IN)   :: Diffc    ! 拡散係数
      real(8), intent(IN)   :: dt       ! 時間刻

      ! Crank Nicholson 設定
      real(8), intent(OUT)  :: DiffLUMatrix(nm+1,4:lm,4:lm)
                              ! Crank Nicholson 拡散陰的計算用行列
                              ! (1-D dt/2▽^2, LU 分解)

      integer, intent(OUT)  :: kpivot(nm+1,4:lm)     ! ピボット情報
                              ! Crank Nicholson 拡散陰的計算用行列ピボット情報

      real(8)               :: wq_I(nm+1,4:lm)       ! 作業用変数
      real(8)               :: wt_I(nm+1,0:lm)       ! 作業用変数

      integer :: l

      DiffLUMatrix = 0.0
      do l=4,lm
         wq_I = 0.0 ; wq_I(:,l) = 1.0             ! 各波数成分独立
         wt_I = wt_wq(wq_I)
         wt_I = wt_Lapla_wt(wt_I)
         wt_I = wt_I - Diffc * dt/2.0 * wt_Lapla_wt(wt_I)
         DiffLUMatrix(:,:,l) = wq_LaplaPol2Pol_wt(wt_I)
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
           file='wt_zonal_galerkin_RR_cn_diff.nc', &
           title='Diffusion model in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lat','rad','t  '/), dimsizes=(/jm,km+1,0/),&
           longnames=(/'Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lat',y_Lat/pi*180)                       ! 変数出力
    call HistoryPut('rad',z_Rad)                              ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
           varname='polvel', dims=(/'lat','rad','t  '/), & 
           longname='Poloidal velocity field', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='polvelsol', dims=(/'lat','rad','t  '/), & 
           longname='Poloidal velocity field(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='polvelerror', dims=(/'lat','rad','t  '/), & 
           longname='Poloidal velocity field(error)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='strfunc', dims=(/'lat','rad','t  '/), & 
           longname='Stream function', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vlat', dims=(/'lat','rad','t  '/), & 
           longname='Lat-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vrad', dims=(/'lat','rad','t  '/), & 
           longname='Rad-velocity', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    !
    ! ヒストリー出力
    !
    !   各物理量の gtool4 ファイルへの出力を行う
    !
    write(6,*) 'it = ',it
    xyz_Polvel = xyz_wt(wt_wq(wq_Polvel))
    xyz_PolvelSol = xyz_PolvelInit* exp( sigma * time )

    xyz_Strfunc = xyz_GradLat_wt(wt_wq(wq_Polvel))*xyz_Rad**2*cos(xyz_Lat)
    xyz_VLat = xyz_GradLat_wt(wt_RotRad_wt(wt_wq(wq_Polvel)))*xyz_Rad
    xyz_VRad = xyz_wt(wt_L2_wt(wt_wq(wq_Polvel)))/xyz_Rad

    call HistoryPut('t',real(it*dt))
    call HistoryPut('polvel',xyz_Polvel(0,:,:))
    call HistoryPut('polvelsol',xyz_PolvelSol(0,:,:))
    call HistoryPut('polvelerror',xyz_Polvel(0,:,:)-xyz_PolvelSol(0,:,:))
    call HistoryPut('strfunc',xyz_Strfunc(0,:,:))
    call HistoryPut('vlat',xyz_VLat(0,:,:))
    call HistoryPut('vrad',xyz_VRad(0,:,:))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    !
    ! ヒストリー出力終了処理
    !
    call HistoryClose
  end subroutine output_gtool4_close

end program wt_zonal_galerkin_RR_cn_diff
