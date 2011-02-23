!----------------------------------------------------------------------
!  Copyright (C) 2002--2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  GAMIRAS モデル : Euler scheme
!      ポロイダル速度場は鉛直格子点空間での選点法による境界条件適用
!      その他は Chebyshev-Tau method
!
!履歴  2002/08/06  竹広真一  ベンチマーク Case 0
!      2002/11/19  竹広真一  プログラム名変更
!      2002/11/26  竹広真一  配列を減らし変数名変更
!      2004/02/16  佐々木 洋平 gt4f90io の使用に伴うライブラリ名変更
!      2008/08/13  竹広真一  配列添え字変更 im -> 0:im-1
!
program gami_euler_tau_bench

  use lumatrix
  use wt_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer,parameter  :: im=32, jm=16, km=8  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=8         ! 切断波数の設定(水平, 動径)
  !integer,parameter  :: im=64, jm=32, km=16  ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter  :: nm=21, lm=16         ! 切断波数の設定(水平, 動径)
  !integer,parameter  :: im=128, jm=64, km=32 ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter  :: nm=42, lm=32         ! 切断波数の設定(水平, 動径)

 !---- 座標変数など ----
  real(8),parameter  :: Ri=7.0D0/13.0D0     ! 内外半径 \eta=0.35
  real(8),parameter  :: Ro=20.0D0/13.0D0    ! 内外半径 \eta=0.35

 !---- 変数 ----
  real(8), dimension((nm+1)**2,0:lm) :: wt_VTor       ! トロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_VPol       ! ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaVPol  ! ▽^2ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp       ! 温度

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Temp      ! 温度
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLon      ! 速度(経度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLat      ! 速度(緯度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VRad      ! 速度(動径)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lon   ! v・▽v (経度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lat   ! v・▽v(緯度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Rad   ! v・▽v(動径)

  real(8)             :: xy_TempBndry(0:im-1,jm,2) ! 境界値
  real(8)             :: w_TempBndry((nm+1)**2,2)  ! 境界値

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-4                    ! 時間ステップ間隔
  integer, parameter :: nt=100, ndisp=20        ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  character(len=2), parameter :: VelBC='RR'     ! 速度境界条件(RR/RF/FR/FF)
  character(len=2), parameter :: TempBC='DD'    ! 温度境界条件(DD/DN/ND/NN)

  real(8), parameter :: Ra=100.0D0              ! レイリー数
  real(8), parameter :: Pr=1.0D0                ! プランドル数
  real(8), parameter :: Ekman=1.0d-3            ! エクマン数

  real(8), parameter :: A=0.1D0                 ! 初期分布振幅
  real(8), parameter :: Temptop=0.0D0           ! 境界温度(上端)
  real(8), parameter :: Tempbottom=1.0D0        ! 境界温度(下端)

 !---- その他 ----
  real(8), parameter :: pi=3.1415926535897932385D0
  integer :: it
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_x   ! x=2r-r_i-r_o

 !---------------- 座標値の設定 ---------------------
  call wt_Initial(im,jm,km,nm,lm,Ri,Ro)

 !------------------- 初期値設定 ----------------------
  xyz_x = 2*xyz_Rad - Ri - Ro

  xyz_Temp =  Ro*Ri/xyz_Rad - Ri &
       + 210*A/sqrt(17920*pi) * (1 - 3*xyz_x**2 + 3*xyz_x**4 -xyz_x**6) &
           * sin(pi/2-xyz_Lat)**4 * cos(4*xyz_Lon)

  wt_Temp = wt_xyz(xyz_Temp)

  wt_VTor = 0     ! 静止状態
  wt_VPol = 0
  wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

  call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)
  call wt_VGradV(xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad, &
                 xyz_VLon,xyz_VLat,xyz_VRad )

 !------------------- 境界値設定 ----------------------
  xy_TempBndry(:,:,1) = Temptop
  xy_TempBndry(:,:,2) = Tempbottom

  w_TempBndry(:,1) = w_xy(xy_TempBndry(:,:,1))
  w_TempBndry(:,2) = w_xy(xy_TempBndry(:,:,2))

 !------------------- 時間積分(単純 Euler 法) --------------------
  call output_gtool4_init
  call output_gtool4

  do it=1,nt                                         ! Euler 法による時間積分

     ! トロイダル速度場
     wt_VTor = wt_VTor + dt *( &
                    wt_Lapla_wt(wt_VTor) &
                  + wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_VTor)   &
                                - 2/Ekman * wt_Qoperator_wt(wt_VPol) ) &
                  - wt_L2Inv_wt(wt_RadRot_xyz_xyz(      &
                                   xyz_VGradV_Lon,xyz_VGradV_Lat)) &
                  )
          

     ! ポロイダル速度場
     wt_LaplaVPol = wt_LaplaVPol + dt *(    &
               - Ra/(Ekman*Ro)*wt_Temp          &
               + wt_Lapla_wt(wt_LaplaVPol) &
               + wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_LaplaVPol) &
                             + 2/Ekman * wt_Qoperator_wt(wt_VTor) )  &
               + wt_L2Inv_wt(wt_RadRotRot_xyz_xyz_xyz(      &
                     xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad )) &
             )

     ! 温度場
     wt_Temp = wt_Temp + dt *(       &
              - wt_Div_xyz_xyz_xyz(  &
                    xyz_VLon*xyz_Temp,xyz_VLat*xyz_Temp,xyz_VRad*xyz_Temp) &
              + 1/Pr * wt_Lapla_wt(wt_Temp) &
            )

     ! 境界条件
     call wt_TorBoundariesTau(wt_VTor,cond=VelBC)
     wt_VPol = wt_wz(wz_Laplapol2pol_wz(wz_wt(wt_LaplaVPol),cond=VelBC))
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)
     call wt_BoundariesTau(wt_Temp,w_TempBndry,cond=TempBC)

     ! グリッド値計算
     xyz_Temp = xyz_wt(wt_Temp)
     call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)
     call wt_VGradV(xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad, &
                    xyz_VLon,xyz_VLat,xyz_VRad )

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

 !------------------- 出力 ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! ヒストリー作成
           file='gami_euler_tau_bench0.nc', &
           title='Benchmark of dynamo model (case0)', &
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
           longname='mean kinetic energy', units='1', xtype='double')
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
    call HistoryPut('ek',avrlonlatrad_xyz((xyz_VLon**2+xyz_VLat**2+xyz_VRad**2)/2))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program gami_euler_tau_bench

