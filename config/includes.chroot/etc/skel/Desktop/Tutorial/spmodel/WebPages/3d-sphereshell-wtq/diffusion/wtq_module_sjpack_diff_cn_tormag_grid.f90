!----------------------------------------------------------------------
!     Copyright (c) 2010 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wtq_module_sjpack
!      球内のトロイダル磁場拡散問題(Crank-Nicolson scheme)
!
!      チェビシェフータウ法による境界条件適用
!
!      計算結果比較のための解析解については解説文書
!
!         「3 次元球領域での拡散型方程式の解析解」
!
!      (wu_diffusion.pdf)を参照のこと. 
!
!履歴  2010/04/18  竹広真一  wtq_diff_cn_tormag_grid.f90 より変更
!
program wtq_di_module_sjpackff_cn_tormag_grid

  use dc_message
  use dc_string, only : StoA
  use lumatrix
  use wtq_module_sjpack
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer,parameter  :: im=32, jm=16         ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: kmo=16, kmi=8        ! 格子点の設定(球殻動径, 球動径)
  integer,parameter  :: nm=10                ! 切断波数の設定(水平)
  integer,parameter  :: lmo=16, lmi=15       ! 切断波数の設定(球殻動径, 球動径)

 !---- 座標変数など ----
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

 !---- 変数 ----
  real(8), dimension(0:im-1,jm,0:kmo) :: xyz_Tormag   ! トロイダル磁場
  real(8), dimension((nm+1)**2,0:lmo) :: wt_Tormag    ! トロイダル磁場
  real(8), dimension((nm+1)**2,0:kmo) :: wz_Tormag    ! トロイダル磁場

  real(8), dimension(0:im-1,jm,0:kmo) :: xyz_TormagInit ! トロイダル磁場(初期値)
  real(8), dimension(0:im-1,jm,0:kmo) :: xyz_TormagSol  ! トロイダル磁場(解析解)

  real(8), dimension(0:im-1,jm,kmi)   :: xyr_Tormag   ! トロイダル磁場
  real(8), dimension((nm+1)**2,0:lmi) :: wq_Tormag    ! トロイダル磁場
  real(8), dimension((nm+1)**2,kmi)   :: wr_Tormag    ! トロイダル磁場

  real(8), dimension(0:im-1,jm,kmi)   :: xyr_TormagInit ! トロイダル磁場(初期値)
  real(8), dimension(0:im-1,jm,kmi)   :: xyr_TormagSol  ! トロイダル磁場(解析解)

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-4                 ! 時間ステップ間隔
  integer, parameter :: nt=1000, ndisp=100        ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
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
  call wtq_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

 !------------------- 初期値設定 ----------------------
  write(6,*)'n,m,l?'
  read(5,*)n,m,l

  alpha = CalAlpha(n,l)
  sigma = -kappa* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Growth rate       : ', sigma

  wz_Tormag = 0.0 ; wr_Tormag = 0.0
  do k=0,kmo
     wz_Tormag(l_nm(n,m),k) = sbj(n,alpha*z_Rad(k))
  enddo

  do k=1,kmi
     wr_Tormag(l_nm(n,m),k) = sbj(n,alpha*r_Rad(k))
  enddo

  xyz_Tormag = xyz_wz(wz_Tormag)
  xyz_TormagInit = xyz_Tormag
  wz_Tormag = wz_xyz(xyz_Tormag)

  xyr_Tormag = xyr_wr(wr_Tormag)
  xyr_TormagInit = xyr_Tormag
  wr_Tormag = wr_xyr(xyr_Tormag)

  call output_gtool4_init
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                               ! Crank-Nicolson 法による時間積分
     time = it * dt
     wz_Tormag = wz_Tormag + dt/2 * kappa * wz_wt(wt_Lapla_wt(wt_wz(wz_Tormag)))
     wr_Tormag = wr_Tormag + dt/2 * kappa * wr_wq(wq_Lapla_wq(wq_wr(wr_Tormag)))
     call wtq_TormagBoundariesGrid(wz_Tormag,wr_Tormag,kappa,kappa)
     call CNImplicitTormag(wz_Tormag,wr_Tormag,kappa,kappa,dt)

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
       b = -(945-105*x**2+x**4)/x**4
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

      real(8) :: dx
      integer :: nx=20
      integer :: nn

      nn = n
      PI = atan(1.0D0)*4.0D0
      dx = PI/ro/nx
      xl = dx ; ValL = sbj(l,ro*xl)

      !
      ! det = 0 となる点を x=0 から刻 dx の幅で探していき
      ! n 番目を挟む xl, xs を設定する.
      !
100   continue
      xs =xl ; ValS = ValL
      xl = xl+dx
      ValL = sbj(l,ro*xl)
      if( ValL*ValS .le. 0.0 ) nn=nn-1
      if( nn .lt. 0 ) goto 199
      goto 100
199 continue

1000  xm = (xs + xl)/2.0
      ValM = sbj(l,ro*xm)
      
      if ( ValS * ValM .GT. 0.0D0 ) then
         xs = xm ; ValS=sbj(l,ro*xs)
      else
         xl = xm ; ValL=sbj(l,ro*xl)
      endif

      if ( abs(xl-xs) .lt. eps ) then
         CalAlpha = xm
         goto 99
      endif

      goto 1000

99    continue
    end function CalAlpha

 !------------------- 拡散項 ----------------------
  subroutine CNImplicitTormag(wz_Tormag,wr_Tormag,Diffc_o,Diffc_i,dt)

    ! トロイダル磁場(球殻)
    real(8), dimension((nm+1)*(nm+1),0:kmo), intent(INOUT) :: wz_Tormag
    ! トロイダル磁場(球)
    real(8), dimension((nm+1)*(nm+1),kmi), intent(INOUT)   :: wr_Tormag
    real(8), intent(IN)          :: Diffc_o    ! 拡散係数(球殻)
    real(8), intent(IN)          :: Diffc_i    ! 拡散係数(球)
    real(8), intent(IN)          :: dt         ! 時間刻

    ! Crank Nicholson 拡散陰的計算用行列(1-D dt/2▽^2, LU 分解)
    real(8), dimension((nm+1)*(nm+1),0:kmo+kmi,0:kmo+kmi) :: DiffLUMatrix
    ! ピボット情報
    integer, dimension((nm+1)*(nm+1),0:kmo+kmi)           :: kpivot

    ! 作業用変数
    real(8), dimension((nm+1)*(nm+1),0:kmo)     :: wz_I
    real(8), dimension((nm+1)*(nm+1),0:kmo)     :: wz_DI
    real(8), dimension((nm+1)*(nm+1),kmi)       :: wr_I
    real(8), dimension((nm+1)*(nm+1),kmi)       :: wr_DI
    real(8), dimension((nm+1)**2,0:kmo+kmi)     :: wzr_Tormag  ! トロイダル磁場

    integer :: k
    logical :: first=.true.

    save DiffLUMatrix, kpivot, first

    if ( first ) then
       first = .false. 

       DiffLUMatrix = 0.0
       do k=0,kmo
          wz_I = 0.0 ; wr_DI = 0.0
          wz_I = 0.0 ; wz_I(:,k) = 1.0             ! 各波数成分独立
          wz_DI =  - Diffc_o * dt/2.0 * wz_wt(wt_Lapla_wt(wt_wz(wz_I)))
          call wtq_TormagBoundariesGrid(wz_DI,wr_DI,Diffc_o,DIffc_i)
          DiffLUMatrix(:,0:kmo,k) = wz_I + wz_DI
          DiffLUMatrix(:,kmo+1:kmo+kmi,k) = wr_DI
       enddo

       do k=1,kmi
          wz_I = 0.0 ; wz_DI = 0.0
          wr_I = 0.0 ; wr_I(:,k) = 1.0             ! 各波数成分独立
          wr_DI =  - Diffc_i * dt/2.0 * wr_wq(wq_Lapla_wq(wq_wr(wr_I)))
          call wtq_TormagBoundariesGrid(wz_DI,wr_DI,Diffc_o,DIffc_i)
          DiffLUMatrix(:,0:kmo,kmo+k) = wz_DI
          DiffLUMatrix(:,kmo+1:kmo+kmi,kmo+k) = wr_I + wr_DI
       enddo

       call ludecomp(DiffLUMatrix,kpivot)
    endif
    
    wzr_Tormag(:,0:kmo) = wz_Tormag
    wzr_Tormag(:,kmo+1:kmo+kmi) = wr_Tormag
    wzr_Tormag = LUSolve(DiffLUMatrix,kpivot,wzr_Tormag)
    wz_Tormag = wzr_Tormag(:,0:kmo) 
    wr_Tormag = wzr_Tormag(:,kmo+1:kmo+kmi)

  end subroutine CNImplicitTormag

 !------------------- 出力ルーチン ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! ヒストリー作成
           file='wtq_module_sjpack_diff_cn_tormag_grid.nc', &
           title='Diffusion model in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=StoA('lon','lat','rad','crad','t'), &
           dimsizes=(/im,jm,kmo+1,kmi,0/),&
           longnames=StoA('Longitude','Latitude','Radius','Radius','time'),&
           units=(/'1','1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_Lon/pi*180)                       ! 変数出力
    call HistoryPut('lat',y_Lat/pi*180)                       ! 変数出力
    call HistoryPut('rad',z_Rad)                              ! 変数出力
    call HistoryPut('crad',r_Rad)                             ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
           varname='tormag_o', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal magnetic potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='tormagsol_o', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal magnetic potential(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='tormagerror_o', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal magnetic potential(error)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='tormag_i', dims=(/'lon ','lat ','crad','t   '/), & 
           longname='Poloidal magnetic potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='tormagsol_i', dims=(/'lon ','lat ','crad','t   '/), & 
           longname='Poloidal magnetic potential(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='tormagerror_i', dims=(/'lon ','lat ','crad','t   '/), & 
           longname='Poloidal magnetic potential(error)', &
           units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    xyz_Tormag = xyz_wz(wz_Tormag)
    xyz_TormagSol = xyz_TormagInit* exp( sigma * time )

    xyr_Tormag = xyr_wr(wr_Tormag)
    xyr_TormagSol = xyr_TormagInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('tormag_o',xyz_Tormag)
    call HistoryPut('tormagsol_o',xyz_TormagSol)
    call HistoryPut('tormagerror_o',xyz_Tormag-xyz_TormagSol)
    call HistoryPut('tormag_i',xyr_Tormag)
    call HistoryPut('tormagsol_i',xyr_TormagSol)
    call HistoryPut('tormagerror_i',xyr_Tormag-xyr_TormagSol)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program wtq_di_module_sjpackff_cn_tormag_grid

