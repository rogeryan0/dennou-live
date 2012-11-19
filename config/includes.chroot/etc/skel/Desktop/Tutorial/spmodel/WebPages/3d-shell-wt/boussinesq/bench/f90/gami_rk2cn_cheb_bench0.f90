!----------------------------------------------------------------------
!  Copyright (C) 2002--2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  回転球殻ブシネスク流体モデル
!      2nd Order Rung-Kutta + Crank Nicholson scheme
!
!履歴  2002/11/03  竹広真一  ベンチマーク Case 0
!      2002/11/20  竹広真一  境界条件計算変更
!      2002/11/26  竹広真一  配列を減らし変数名変更
!      2004/02/16  佐々木 洋平 gt4f90io の使用に伴うライブラリ名変更
!      2008/08/13  竹広真一  配列添え字変更 im -> 0:im-1
!
program gami_rk2cn_cheb_bench0

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
  real(8), dimension((nm+1)**2,0:lm) :: wt_VTor1      ! トロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_VPol       ! ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_VPol1      ! ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaVPol  ! ▽^2ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaVPol1 ! ▽^2ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp       ! 温度
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp1      ! 温度

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Temp      ! 温度
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLon      ! 速度(経度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLat      ! 速度(緯度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VRad      ! 速度(動径)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Q      ! 熱源
  real(8), dimension((nm+1)**2,0:lm) :: wt_Q       ! 熱源

  real(8)             :: xy_TempBndry(0:im-1,jm,2) ! 境界値
  real(8)             :: w_TempBndry((nm+1)**2,2)  ! 境界値

 !---- 時間積分用変数 ----

 ! トロイダル速度時間変化
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDVTor
 ! ▽^2ポロイダル速度時間変化
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDLaplaVPol
 ! 温度時間変化
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDTemp

 ! Crank Nicholson 陰的計算用拡散行列(速度場)
  real(8), dimension((nm+1)*(nm+1),0:lm,0:lm)   :: DifLUMT_Vel
  integer, dimension((nm+1)*(nm+1),0:lm)        :: kpivot_Vel     ! ピボット
 ! Crank Nicholson 陰的計算用拡散行列(温度)
  real(8), dimension((nm+1)*(nm+1),0:lm,0:lm)   :: DifLUMT_Temp
  integer, dimension((nm+1)*(nm+1),0:lm)        :: kpivot_Temp    ! ピボット

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=2e-4                 ! 時間ステップ間隔
  integer, parameter :: nt=250, ndisp=50        ! 時間積分数, 表示ステップ

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
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_X   ! x=2r-r_i-r_o

 !---------------- 座標値の設定 ---------------------
  call wt_Initial(im,jm,km,nm,lm,Ri,Ro)

 !------------------- 初期値設定 ----------------------
  xyz_X = 2*xyz_Rad - Ri - Ro

  xyz_Temp =  Ro*Ri/xyz_Rad - Ri &
       + 210*A/sqrt(17920*pi) * (1 - 3*xyz_X**2 + 3*xyz_X**4 -xyz_X**6) &
           * sin(pi/2-xyz_Lat)**4 * cos(4*xyz_Lon)

  wt_Temp = wt_xyz(xyz_Temp)

  wt_VTor = 0     ! 静止状態
  wt_VPol = 0
  wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

  call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)

  call CNDiffusionMatrix( 1.0D0, dt, DifLUMT_Vel, kpivot_Vel )
  call CNDiffusionMatrix( 1/Pr, dt, DifLUMT_Temp, kpivot_Temp )

  xyz_Q = 0.0 ; wt_Q = wt_xyz(xyz_Q)    ! 熱源なし

 !------------------- 境界値設定 ----------------------
  xy_Tempbndry(:,:,1) = Temptop
  xy_Tempbndry(:,:,2) = Tempbottom

  w_TempBndry(:,1) = w_xy(xy_TempBndry(:,:,1))
  w_TempBndry(:,2) = w_xy(xy_TempBndry(:,:,2))

 !------------------- 時間積分(Adams-Bashforth 法) --------------------
  call output_gtool4_init
  call output_gtool4

  do it=1,nt
    !---- Runge-Kutta 1 段目
     call TimeDerivetives_noDiffusion( &
         wt_VTor, wt_VPol, wt_LaplaVPol, wt_Temp, wt_Q, &
         xyz_VLon, xyz_VLat, xyz_VRad, xyz_Temp, &
         wt_DtDVTor, wt_DtDLaplaVPol, wt_DtDTemp )

     ! トロイダル速度場
     wt_VTor1 = wt_VTor + 1/2.0 * dt * wt_DtDVTor
     call wt_TorBoundariesGrid(wt_VTor1,cond=velBC)

     ! ポロイダル速度場
     wt_LaplaVPol1 = wt_LaplaVPol + 1/2.0 * dt * wt_DtDLaplaVPol
     wt_VPol1 = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaVPol1),cond=velBC))
     wt_LaplaVPol1 = wt_Lapla_wt(wt_VPol1)

     ! 温度場
     wt_Temp1 = wt_Temp + 1/2.0 * dt * wt_DtDTemp
     call wt_BoundariesGrid(wt_Temp1,w_TempBndry,cond=TempBC)

     ! グリッド値計算
     xyz_Temp = xyz_wt(wt_Temp1)
     call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor1,wt_VPol1)

    !---- Runge-Kutta 2 段目
     call TimeDerivetives_noDiffusion( &
         wt_VTor1, wt_VPol1, wt_LaplaVPol1, wt_Temp1, wt_Q, &
         xyz_VLon, xyz_VLat, xyz_VRad, xyz_Temp, &
         wt_DtDVTor, wt_DtDLaplaVPol, wt_DtDTemp )

     ! トロイダル速度場
     wt_VTor = wt_VTor + dt * wt_DtDVTor
     call wt_TorBoundariesGrid(wt_VTor,cond=velBC)

     ! ポロイダル速度場
     wt_LaplaVPol = wt_LaplaVPol + dt * wt_DtDLaplaVPol
     wt_VPol = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaVPol),cond=velBC))
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

     ! 温度場
     wt_Temp = wt_Temp + dt * wt_DtDTemp
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)

    !---- Crank-Nicolson 
     wt_VTor = wt_VTor+dt/2.0 * wt_Lapla_wt(wt_VTor)
     call wt_TorBoundariesGrid(wt_VTor,cond=velBC)
     wt_VTor = LUSolve(DifLUMT_Vel,kpivot_Vel,wt_VTor)
     call wt_TorBoundariesGrid(wt_VTor,cond=velBC)

     wt_LaplaVPol = wt_LaplaVPol + dt/2.0 * wt_Lapla_wt(wt_LaplaVPol)
     wt_VPol = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaVPol),cond=velBC))
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

     wt_LaplaVPol = LUSolve(DifLUMT_Vel,kpivot_Vel,wt_LaplaVPol)
     wt_VPol = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaVPol),cond=velBC))
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

     wt_Temp = wt_Temp+ dt/2.0/Pr * wt_Lapla_wt(wt_Temp)
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)
     wt_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,wt_Temp)
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)

     ! グリッド値計算
     xyz_Temp = xyz_wt(wt_Temp)
     call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

 !------------------- 時間変化項 ----------------------
  subroutine TimeDerivetives_noDiffusion( &
       wt_VTor, wt_VPol, wt_LaplaVPol, wt_Temp, wt_Q, &
       xyz_VLon, xyz_VLat, xyz_VRad, xyz_Temp, &
       wt_DtDVTor, wt_DtDLaplaVPol, wt_DtDTemp )

   ! トロイダル速度
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_VTor
   ! ポロイダル速度
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_VPol
   ! ▽^2 ポロイダル速度
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_LaplaVPol
   ! 温度
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Temp
   ! 熱源
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Q

    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_VLon   ! 速度(経度)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_VLat   ! 速度(緯度)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_VRad   ! 速度(動径)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_Temp   ! 温度

   ! トロイダル速度時間変化項
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDVTor
   ! ポロイダル速度時間変化項
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDLaplaVPol
   ! 温度時間変化項
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDTemp

    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lon   ! v・▽v(経度)
    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Lat   ! v・▽v(緯度)
    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradV_Rad   ! v・▽v(動径)


   ! v・▽v 非線形項計算
    call wt_VGradV(xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad, &
                   xyz_VLon,xyz_VLat,xyz_VRad )

  ! トロイダル時間変化
    wt_DtDVTor = &
           wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_VTor)   &
                       - 2/Ekman * wt_Qoperator_wt(wt_VPol) ) &
         - wt_L2Inv_wt(wt_RadRot_xyz_xyz(xyz_VGradV_Lon,xyz_VGradV_Lat)) 

  ! ポロイダル時間変化
    wt_DtDLaplaVPol = &
         - Ra/(Ekman*RO)*wt_Temp                    &
         + wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_LaplaVPol) &
                       + 2/Ekman * wt_Qoperator_wt(wt_VTor) )  &
         + wt_L2inv_wt(wt_RadRotRot_xyz_xyz_xyz(      &
               xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad ))

  ! 温度時間変化
    wt_DtDTemp = &
         - wt_Div_xyz_xyz_xyz(  &
               xyz_VLon*xyz_Temp,xyz_VLat*xyz_Temp,xyz_VRad*xyz_Temp)

  end subroutine TimeDerivetives_noDiffusion

 !------------------- 拡散項 ----------------------
  subroutine CNDiffusionMatrix( Diffc, dt, DiffLUMatrix, kpivot )

    real(8), intent(IN)          :: Diffc    ! 拡散係数
    real(8), intent(IN)          :: dt       ! 時間刻

    ! Crank Nicholson 拡散陰的計算用行列(1-D dt/2▽^2, LU 分解)
    real(8), dimension((nm+1)*(nm+1),0:lm,0:lm), intent(OUT)  :: DiffLUMatrix
    ! ピボット情報
    integer, dimension((nm+1)*(nm+1),0:lm), intent(OUT)       :: kpivot

    ! 作業用変数
    real(8), dimension((nm+1)*(nm+1),0:lm)           :: wt_I

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! 各波数成分独立
       DiffLUMatrix(:,:,l) = wt_I - Diffc * dt/2.0 * wt_Lapla_wt(wt_I) 
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrix

 !------------------- 出力 ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! ヒストリー作成
           file='gami_rk2cn_cheb_bench0.nc', &
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
           varname='laplapolvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='laplacian poloidal velocity potential', &
           units='1', xtype='double')
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
           varname='um', dims=(/'lat','rad','t  '/), & 
           longname='zonal flow', units='1', xtype='double')
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
    call HistoryPut('um',yz_avrLon_xyz(xyz_VLon))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program gami_rk2cn_cheb_bench0



