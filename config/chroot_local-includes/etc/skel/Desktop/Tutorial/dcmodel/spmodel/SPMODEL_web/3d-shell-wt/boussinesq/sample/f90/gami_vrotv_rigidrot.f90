!----------------------------------------------------------------------
!  Copyright (C) 2002--2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  回転球殻ブシネスク流体モデル : 剛体回転初期値
!
!履歴  2002/05/25  竹広真一
!      2002/11/27  竹広真一
!      2002/11/28  竹広真一 非線形項変更
!      2004/02/16  佐々木 洋平 gt4f90io の使用に伴うライブラリ名変更
!      2008/08/13  竹広真一  配列添え字変更 im -> 0:im-1
!
program gami_vrotv_rigidrot

  use lumatrix
  use wt_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer,parameter  :: im=32, jm=16, km=8  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=8         ! 切断波数の設定(水平, 動径)

 !---- 座標変数など ----
  real(8),parameter  :: Ri=0.5, Ro=1.5       ! 内外半径

 !---- 変数 ----
  real(8), dimension((nm+1)**2,0:lm) :: wt_VTor       ! トロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_VPol       ! ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaVPol  ! ▽^2ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp    ! 温度

  real(8), dimension(0:im-1,jm,0:km) :: xyz_VTor      ! トロイダル速度
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VPol      ! ポロイダル速度
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Temp   ! 温度

  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLon      ! 速度(経度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLat      ! 速度(緯度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VRad      ! 速度(動径)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_ZLon      ! 渦度(経度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_ZLat      ! 渦度(緯度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_ZRad      ! 渦度(動径)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_ZxVLon   ! (▽xv)xv (経度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_ZxVLat   ! (▽xv)xv(緯度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_ZxVRad   ! (▽xv)xv(動径)

  real(8)             :: xy_TempBndry(0:im-1,jm,2) ! 境界値
  real(8)             :: w_TempBndry((nm+1)**2,2)  ! 境界値

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=2e-4                 ! 時間ステップ間隔
  integer, parameter :: nt=1000, ndisp=100      ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  character(len=2), parameter :: VelBC='FF'     ! 速度境界条件(RR/RF/FR/FF)
  character(len=2), parameter :: TempBC='DD'    ! 温度境界条件(DD/DN/ND/NN)

  real(8), parameter :: Ra=0                    ! レイリー数
  real(8), parameter :: Pr=1.0                  ! プランドル数
  !real(8), parameter :: Ta=1.0d2                ! テイラー数
  real(8), parameter :: Ta=0.0                  ! テイラー数

  real(8), parameter :: pi=3.1415926535897932385D0

 !---- その他 ----
  integer :: it
  real(8) :: Tau                                ! √テイラー数

  Tau = sqrt(Ta)

 !---------------- 座標値の設定 ---------------------
  call wt_initial(im,jm,km,nm,lm,Ri,Ro)

 !------------------- 初期値設定 ----------------------
  xyz_Temp =  0.0
  wt_Temp = wt_xyz(xyz_Temp)

  ! 東西流剛体回転(系の回転非 0)
  !xyz_VTor =  xyz_Rad*sin(xyz_Lat) ; xyz_VPol =  0 

  ! 南北流剛体回転(系の回転 0)
  xyz_VTor =  xyz_Rad*cos(xyz_Lat)*sin(xyz_Lon) ; xyz_VPol =  0 

  wt_VTor = wt_xyz(xyz_VTor) ; wt_VPol = wt_xyz(xyz_VPol)
  wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

  call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)
  call wt_Potential2Rotation(xyz_ZLon,xyz_ZLat,xyz_ZRad,wt_VTor,wt_VPol)

  xyz_ZxVLon = xyz_VRad * xyz_ZLat - xyz_VLat * xyz_ZRad
  xyz_ZxVLat = xyz_VLon * xyz_ZRad - xyz_VRad * xyz_ZLon
  xyz_ZxVRad = xyz_VLat * xyz_ZLon - xyz_VLon * xyz_ZLat

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
     wt_VTor = wt_VTor + dt *( &
                    Pr * wt_Lapla_wt(wt_VTor) &
                  + wt_L2Inv_wt(  Tau * wt_KxRGrad_wt(wt_VTor)   &
                                - Tau * wt_QOperator_wt(wt_VPol) ) &
                  - wt_L2Inv_wt(wt_RadRot_xyz_xyz(      &
                                   xyz_ZxVLon,xyz_ZxVLat)) &
                  )
          

     ! ポロイダル速度場
     wt_LaplaVPol = wt_LaplaVPol + dt *(    &
               - Ra*Pr*wt_Temp                  &
               + Pr * wt_Lapla_wt(wt_LaplaVPol) &
               + wt_L2Inv_wt(  Tau * wt_KxRGrad_wt(wt_LaplaVPol) &
                             + Tau * wt_QOperator_wt(wt_VTor) )  &
               + wt_L2Inv_wt(wt_RadRotRot_xyz_xyz_xyz(      &
                     xyz_ZxVLon,xyz_ZxVLat,xyz_ZxVRad )) &
             )

     ! 温度場
     wt_Temp = wt_Temp + dt *(       &
              - wt_Div_xyz_xyz_xyz(  &
                    xyz_VLon*xyz_Temp,xyz_VLat*xyz_Temp,xyz_VRad*xyz_Temp) &
              + wt_L2_wt(wt_VPol)    &
              + wt_Lapla_wt(wt_Temp) &
            )

     ! 境界条件
     call wt_TorBoundariesGrid(wt_VTor,cond=VelBC)
     wt_VPol = wt_LaplaPol2PolGrid_wt(wt_LaplaVPol,cond=VelBC)
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)

     ! グリッド値計算
     xyz_Temp = xyz_wt(wt_Temp)
     call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)
     call wt_Potential2Rotation(xyz_ZLon,xyz_ZLat,xyz_ZRad,wt_VTor,wt_VPol)



     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

 !------------------- 出力 ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! ヒストリー作成
           file='gami_vrotv_rigidrot.nc', title='convection in a spherical shell', &
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
           varname='vtor', dims=(/'lon','lat','rad','t  '/), & 
           longname='toroidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vpol', dims=(/'lon','lat','rad','t  '/), & 
           longname='poloidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='temp', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vlon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vlat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vrad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='ek', dims=(/'t  '/), & 
           longname='kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='am', dims=(/'t  '/), & 
           longname='relative angular momentum', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('vtor',xyz_wt(wt_VTor))
    call HistoryPut('vpol',xyz_wt(wt_VPol))
    call HistoryPut('temp',xyz_Temp)
    call HistoryPut('vlon',xyz_VLon)
    call HistoryPut('vlat',xyz_VLat)
    call HistoryPut('vrad',xyz_VRad)
    call HistoryPut('ek',AvrLonLatRad_xyz((xyz_VLon**2+xyz_VLat**2+xyz_VRad**2)/2))
    call HistoryPut('am',IntLonLatRad_xyz(xyz_VLon*xyz_Rad*cos(xyz_Lat)))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program gami_vrotv_rigidrot
