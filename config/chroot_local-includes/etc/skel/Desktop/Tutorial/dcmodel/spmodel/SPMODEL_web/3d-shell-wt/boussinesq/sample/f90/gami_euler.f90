!----------------------------------------------------------------------
!  Copyright (C) 2002--2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  回転球殻ブシネスク流体モデル : Euler scheme
!
!履歴  2002/05/14  竹広真一
!      2004/02/16  佐々木 洋平 gt4f90io の使用に伴うライブラリ名変更
!      2008/08/13  竹広真一  配列添え字変更 im -> 0:im-1
!
program gami_euler

  use lumatrix
  use wt_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer,parameter  :: im=32, jm=16, km=8  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=8         ! 切断波数の設定(水平, 動径)

 !---- 座標変数など ----
  !real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径 \eta=0.3
  real(8),parameter  :: ri=1.0, ro=2.0       ! 内外半径 \eta=0.5

 !---- 変数 ----
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Torvel      ! トロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_Torvel       ! トロイダル速度
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Polvel      ! ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_Polvel       ! ポロイダル速度
  real(8), dimension(0:im-1,jm,0:km) :: xyz_LaplaPolvel ! ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaPolvel  ! ▽^2ポロイダル速度

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Temp   ! 温度
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp    ! 温度

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Q      ! 熱源
  real(8), dimension((nm+1)**2,0:lm) :: wt_Q       ! 熱源

  real(8), dimension(0:im-1,jm,0:km) :: xyz_U      ! 速度(経度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_V      ! 速度(緯度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_W      ! 速度(動径)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lon   ! v・▽v (経度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lat   ! v・▽v(緯度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Rad   ! v・▽v(動径)

  real(8)             :: xy_TempBndry(0:im-1,jm,2) ! 境界値
  real(8)             :: w_TempBndry((nm+1)**2,2)  ! 境界値

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=5e-5                    ! 時間ステップ間隔
  !integer, parameter :: nt=100000, ndisp=2000      ! 時間積分数, 表示ステップ
  integer, parameter :: nt=1000, ndisp=200      ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  character(len=2), parameter :: VelBC='FF'     ! 速度境界条件(RR/RF/FR/FF)
  character(len=2), parameter :: TempBC='DD'    ! 温度境界条件(DD/DN/ND/NN)

  real(8), parameter :: Ra=1e3                  ! レイリー数
  real(8), parameter :: Pr=1.0                  ! プランドル数
  real(8), parameter :: Ta=1.0d4                ! テイラー数

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: Lon1=pi                 ! 初期分布緯度座標
  real(8), parameter :: Lat1=0.0                ! 初期分布経度座標
  real(8), parameter :: Rad1=(ri+ro)/2.0        ! 初期分布動径座標
  real(8), parameter :: Sigma=0.05               ! 初期分布の大きさ

 !---- その他 ----
  integer :: it
  real(8) :: Tau                                ! √テイラー数

  Tau = sqrt(Ta)

 !---------------- 座標値の設定 ---------------------
  call wt_Initial(im,jm,km,nm,lm,ri,ro)

 !------------------- 初期値設定 ----------------------
  xyz_Temp =  exp(-((xyz_Lon-Lon1)**2+(xyz_Lat-Lat1)**2+(xyz_Rad-Rad1)**2)&
                 /(2*Sigma**2))
  wt_Temp = wt_xyz(xyz_Temp)

  xyz_Q = 0.0
  wt_Q = wt_xyz(xyz_Q)

  xyz_Torvel =  0
  xyz_Polvel =  0
  wt_Torvel = wt_xyz(xyz_Torvel)
  wt_Polvel = wt_xyz(xyz_Polvel)
  wt_LaplaPolvel = wt_Lapla_wt(wt_Polvel)
  xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)

  call wt_Potential2Vector(xyz_U,xyz_V,xyz_W,wt_Torvel,wt_Polvel)
  call wt_VGradV(xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad, &
                 xyz_U,xyz_V,xyz_W )

 !------------------- 境界値設定 ----------------------
  xy_TempBndry(:,:,1) = 0
  xy_TempBndry(:,:,2) = 0

  w_TempBndry(:,1) = w_xy(xy_TempBndry(:,:,1))
  w_TempBndry(:,2) = w_xy(xy_TempBndry(:,:,2))

 !------------------- 時間積分(単純 Euler 法) --------------------
  call output_gtool4_init
  call output_gtool4

  do it=1,nt                                         ! Euler 法による時間積分

     ! トロイダル速度場
     wt_Torvel = wt_Torvel + dt *( &
                    Pr * wt_Lapla_wt(wt_Torvel) &
                  + wt_L2Inv_wt(  Tau * wt_KxRGrad_wt(wt_Torvel)   &
                                - Tau * wt_Qoperator_wt(wt_Polvel) ) &
                  - wt_L2Inv_wt(wt_RadRot_xyz_xyz(      &
                                   xyz_VGradV_Lon,xyz_VGradV_Lat)) &
                  )
          

     ! ポロイダル速度場
     wt_LaplaPolvel = wt_LaplaPolvel + dt *(    &
               - Ra*Pr*wt_Temp                  &
               + Pr * wt_Lapla_wt(wt_LaplaPolvel) &
               + wt_L2Inv_wt(  Tau * wt_KxRGrad_wt(wt_LaplaPolvel) &
                             + Tau * wt_Qoperator_wt(wt_Torvel) )  &
               + wt_L2Inv_wt(wt_RadRotRot_xyz_xyz_xyz(      &
                     xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad )) &
             )

     ! 温度場
     wt_Temp = wt_Temp + dt *(       &
              - wt_Div_xyz_xyz_xyz(  &
                    xyz_U*xyz_Temp,xyz_V*xyz_Temp,xyz_W*xyz_Temp) &
              + wt_L2_wt(wt_Polvel)  &
              + wt_Lapla_wt(wt_Temp) &
              + wt_Q                 &
            )

     ! 境界条件
     call wt_TorBoundaries(wt_Torvel,cond=VelBC)
     wt_Polvel = wt_wz(wz_Laplapol2Pol_wz(wz_wt(wt_LaplaPolvel),cond=VelBC))
     wt_LaplaPolvel = wt_Lapla_wt(wt_Polvel)
     call wt_Boundaries(wt_Temp,w_TempBndry,cond=TempBC)

     ! グリッド値計算
     xyz_Torvel = xyz_wt(wt_Torvel)
     xyz_Polvel = xyz_wt(wt_Polvel)
     xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)
     xyz_Temp = xyz_wt(wt_Temp)

     call wt_Potential2Vector(xyz_U,xyz_V,xyz_W,wt_Torvel,wt_Polvel)
     call wt_VGradV(xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad, &
                    xyz_U,xyz_V,xyz_W )

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

 !------------------- 出力 ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! ヒストリー作成
           file='gami_euler.nc', title='convection in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_Lon/pi*180)                       ! 変数出力
    call HistoryAddattr('lon','topology','circular')          ! 周期属性
    call HistoryAddattr('lon','modulo',360.0)                 ! 周期属性
    call HistoryPut('lat',y_Lat/pi*180)                       ! 変数出力
    call HistoryPut('rad',z_Rad)                              ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
           varname='torvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='toroidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='polvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='poloidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='laplapolvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='laplacian poloidal velocity potential', &
           units='1', xtype='double')
   call HistoryAddVariable( &                                ! 変数定義
           varname='temp', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature', units='1', xtype='double')
   call HistoryAddVariable( &                                ! 変数定義
           varname='u', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-velocity', units='1', xtype='double')
   call HistoryAddVariable( &                                ! 変数定義
           varname='v', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-velocity', units='1', xtype='double')
   call HistoryAddVariable( &                                ! 変数定義
           varname='w', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-velocity', units='1', xtype='double')

   call HistoryAddVariable( &                                ! 変数定義
           varname='ke', dims=(/'t  '/), & 
           longname='kinetic energy', units='1', xtype='double')
   call HistoryAddVariable( &                                ! 変数定義
           varname='um', dims=(/'lat','rad','t  '/), & 
           longname='zonal flow', units='1', xtype='double')
   end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('torvel',xyz_Torvel)
    call HistoryPut('polvel',xyz_Polvel)
    call HistoryPut('laplapolvel',xyz_LaplaPolvel)
    call HistoryPut('temp',xyz_Temp)
    call HistoryPut('u',xyz_U)
    call HistoryPut('v',xyz_V)
    call HistoryPut('w',xyz_W)
    call HistoryPut('ke',IntLonLatRad_xyz((xyz_U**2+xyz_V**2+xyz_W**2)/2))
    call HistoryPut('um',yz_AvrLon_xyz(xyz_U))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program gami_euler
