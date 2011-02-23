!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_module
!      速度ポロイダルポテンシャル拡散問題
!
!履歴  2002/02/13  竹広真一
!      2004/02/15  佐々木 洋平 gt4f90io の使用に伴うライブラリ名変更
!      2005/06/03  佐々木 洋平 it=0 を明示的に宣言
!
program wt_diff3

  use lumatrix
  use wt_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)

 !---- 座標変数など ----
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

 !---- 変数 ----
  real(8), dimension(im,jm,0:km)     :: xyz_PVel   ! ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_PVel    ! ポロイダル速度
  real(8), dimension(im,jm,0:km)     :: xyz_LaplaPVel   ! ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaPVel    ! ▽^2ポロイダル速度

  real(8)             :: xy_PmagBndry(im,jm,2)     ! 境界値
  real(8)             :: w_PmagBndry((nm+1)**2,2)  ! 境界値

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=5e-5                 ! 時間ステップ間隔
  integer, parameter :: nt=10000, ndisp=1000    ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  character(len=2), parameter :: VelBC='RF'     ! 速度境界条件(RR/RF/FR/FF)
  real(8), parameter :: nu=1.0                  ! 粘性係数

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: Lon1=pi                 ! 初期分布緯度座標
  real(8), parameter :: Lat1=0.0                ! 初期分布経度座標
  real(8), parameter :: Rad1=(ri+ro)/2.0        ! 初期分布動径座標
  real(8), parameter :: Sigma=0.2               ! 初期分布の大きさ

 !---- その他 ----
  integer :: it=0

 !---------------- 座標値の設定 ---------------------
  call wt_initial(im,jm,km,nm,lm,ri,ro)

 !------------------- 初期値設定 ----------------------
  xyz_PVel =  exp(-((xyz_Lon-Lon1)**2+(xyz_Lat-Lat1)**2+(xyz_Rad-Rad1)**2)&
                 /(2*Sigma**2))
  wt_PVel = wt_xyz(xyz_PVel)
  wt_LaplaPVel = wt_Lapla_wt(wt_PVel)
  xyz_LaplaPVel = xyz_wt(wt_LaplaPVel)

  call output_gtool4_init
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Euler 法による時間積分
     wt_LaplaPVel = wt_LaplaPVel + dt *( nu * wt_Lapla_wt(wt_LaplaPVel))
     wt_PVel = wt_wz(wz_Laplapol2pol_wz(wz_wt(wt_LaplaPVel),cond=VelBC))
     wt_LaplaPVel = wt_Lapla_wt(wt_PVel)

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        xyz_PVel = xyz_wt(wt_PVel)
        xyz_LaplaPVel = xyz_wt(wt_LaplaPVel)
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

 !------------------- 出力 ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! ヒストリー作成
           file='wt_diff3.nc', title='Diffusion model in a spherical shell', &
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
           varname='pvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='poloidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='laplapvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='Laplacian poloidal velocity potential', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('pvel',xyz_PVel)
    call HistoryPut('laplapvel',xyz_LaplaPVel)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program wt_diff3
