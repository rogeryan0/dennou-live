!----------------------------------------------------------------------
!  Copyright (C) 2004--2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  回転球殻ブシネスク流体モデル
!      Adams-Bashforth + Crank Nicholson scheme
!      鉛直格子点空間での選点法による境界条件適用
!
!履歴  2002/09/04  竹広 真一   ベンチマーク Case 0
!      2002/10/14  竹広 真一   リスタート可能バージョン
!      2002/11/19  竹広 真一   境界条件をタウ法から変更
!      2002/11/24  竹広 真一   鉛直格子空間での Crank Nicholson 行列. 
!                              行列計算で境界値を変更しない. 
!      2002/11/26  竹広 真一   配列を減らし変数名変更
!      2004/02/16  佐々木 洋平 gt4f90io の使用に伴うライブラリ名変更
!      2004/10/27  竹広 真一   Adams-Bashforth scheme 用の各時間変化量を
!                              リスタート変数に追加
!      2004/12/30  竹広 真一   HistoryCreate に xtype を追加
!                              (Alpha machine での出力時エラーへの暫定対処).
!      2005/01/03  竹広 真一   解像度追加
!      2005/01/04  竹広 真一   HistoryCreate のオプション xtypes typo 修正
!      2008/08/13  竹広 真一   配列添え字変更 im -> 0:im-1
!
program gami_abcn_bench0

  use lumatrix
  use wt_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  ! T10 N8
  integer,parameter :: im=32, jm=16, km=8  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter :: nm=10, lm=8         ! 切断波数の設定(水平, 動径)
  !  T21 N16
  !integer,parameter :: im=64, jm=32, km=16  ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter :: nm=21, lm=16         ! 切断波数の設定(水平, 動径)
  !! T21 N32
  !integer,parameter  :: im=64, jm=32, km=32  ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter  :: nm=21, lm=32         ! 切断波数の設定(水平, 動径)
  !  T42 N32
  !integer,parameter :: im=128, jm=64, km=32 ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter :: nm=42, lm=32         ! 切断波数の設定(水平, 動径)
  !  T85 N32
  !integer,parameter :: im=256, jm=128, km=48 ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter :: nm=85, lm=48          ! 切断波数の設定(水平, 動径)
  !  T170 N48
  !integer,parameter :: im=512, jm=256, km=64 ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter :: nm=170, lm=64         ! 切断波数の設定(水平, 動径)
  !  T341 N80
  !integer,parameter :: im=1024, jm=512, km=80 ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter :: nm=341, lm=80          ! 切断波数の設定(水平, 動径)

 !---- 座標変数など ----
  real(8),parameter  :: Ri=7.0D0/13.0D0     ! 内外半径 \eta=0.35
  real(8),parameter  :: Ro=20.0D0/13.0D0    ! 内外半径 \eta=0.35

 !---- 変数 ----
  real(8), dimension((nm+1)**2,0:lm) :: wt_VTor       ! トロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_VPol       ! ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaVPol  ! ▽^2ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp       ! 温度

  real(8), dimension(0:im-1,jm,0:km) :: xyz_VTor      ! トロイダル速度
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VPol      ! ポロイダル速度
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
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDVTor1, wt_DtDVTor0
 ! ▽^2ポロイダル速度時間変化
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDLaplaVPol1, wt_DtDLaplaVPol0
 ! 温度時間変化
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDTemp1, wt_DtDTemp0

 ! Crank Nicholson 陰的計算用拡散行列(速度場)
  real(8), dimension((nm+1)*(nm+1),0:km,0:lm)   :: DifLUMT_Vel
  integer, dimension((nm+1)*(nm+1),0:km)        :: kpivot_Vel     ! ピボット
 ! Crank Nicholson 陰的計算用拡散行列(温度)
  real(8), dimension((nm+1)*(nm+1),0:km,0:lm)   :: DifLUMT_Temp
  integer, dimension((nm+1)*(nm+1),0:km)        :: kpivot_Temp    ! ピボット

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
  integer :: it=0
  real(8) :: time

 !---- NAMELIST 変数 ----
  character(len=100) :: initial_file=''          ! 初期値データファイル名
  real(8)            :: initial_time=0.0         ! 初期時刻
  namelist /initial/ initial_file, initial_time  ! 初期値設定パラメター

  real(8) :: delta_t=2.0d-4                      ! 時間積分刻み
  integer :: nstep=250                           ! 時間積分ステップ数
  namelist /tint/    delta_t, nstep              ! 時間積分パラメター

  character(len=100) :: output_file='gami_abcn_bench0.nc'  ! 出力ファイル名
  integer :: ndisp=50                            ! 出力間隔ステップ数
  namelist /output/    output_file, ndisp        ! 出力設定

 !---------------- 座標値の設定 ---------------------
  call wt_Initial(im,jm,km,nm,lm,Ri,Ro)

 !------------------- 時間積分パラメター --------------------
  write(6,nml=tint)   ; read(5,nml=tint)   ; write(6,nml=tint)

 !------------------- 初期値設定 ----------------------
  write(6,nml=initial) ; read(5,nml=initial) ; write(6,nml=initial)
  time = initial_time

  if ( initial_file /= '' ) then
     call initial_read_gtool4(initial_file, initial_time, &
                      xyz_VTor, xyz_VPol, xyz_Temp, &
                      wt_DtDVTor1, wt_DtDLaplaVPol1, wt_DtDTemp1 )

  else
     call set_initial_values(xyz_VTor, xyz_VPol, xyz_Temp)
  endif

  wt_Temp = wt_xyz(xyz_Temp)
  wt_VTor = wt_xyz(xyz_VTor) ; wt_VPol = wt_xyz(xyz_VPol)
  
  wt_LaplaVPol = wt_Lapla_wt(wt_VPol)
  call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)  

  if ( initial_file == '' ) then
     call TimeDerivetives_noDiffusion( &
          wt_VTor, wt_VPol, wt_LaplaVPol, wt_Temp, wt_Q, &
          xyz_VLon, xyz_VLat, xyz_VRad, xyz_Temp, &
          wt_DtDVTor1, wt_DtDLaplaVPol1, wt_DtDTemp1 )
  endif

  call CNDiffusionMatrixGrid( 1.0D0, delta_t, DifLUMT_Vel, kpivot_Vel )
  call CNDiffusionMatrixGrid( 1/Pr, delta_t, DifLUMT_Temp, kpivot_Temp )

  xyz_Q = 0.0 ; wt_Q = wt_xyz(xyz_Q)    ! 熱源なし

 !------------------- 境界値設定 ----------------------
  xy_Tempbndry(:,:,1) = Temptop
  xy_Tempbndry(:,:,2) = Tempbottom

  w_TempBndry(:,1) = w_xy(xy_TempBndry(:,:,1))
  w_TempBndry(:,2) = w_xy(xy_TempBndry(:,:,2))

 !------------------- 出力設定 ----------------------
  write(6,nml=output) ; read(5,nml=output) ; write(6,nml=output)

  call output_gtool4_init
  if ( initial_file == '' ) call output_gtool4    ! 内部で与えた初期値は出力

 !------------------- 時間積分(Adams-Bashforth 法) --------------------
  do it=1,nstep
     time = initial_time + it * delta_t

     ! 昔の時間変化項を保存
     wt_DtDVTor0 = wt_DtDVTor1
     wt_DtDLaplaVPol0 = wt_DtDLaplaVPol1
     wt_DtDTemp0   = wt_DtDTemp1

     call TimeDerivetives_noDiffusion( &
         wt_VTor, wt_VPol, wt_LaplaVPol, wt_Temp, wt_Q, &
         xyz_VLon, xyz_VLat, xyz_VRad, xyz_Temp, &
         wt_DtDVTor1, wt_DtDLaplaVPol1, wt_DtDTemp1 )

     ! トロイダル速度場
     wt_VTor = wt_VTor + delta_t *( 3/2.0*wt_DtDVTor1 - 1/2.0*wt_DtDVTor0 ) &
                           + delta_t/2.0 * wt_Lapla_wt(wt_VTor) 
     call wt_TorBoundariesGrid(wt_VTor,cond=velBC)
     wt_VTor = LUSolve(DifLUMT_Vel,kpivot_Vel,wz_wt(wt_VTor))
     call wt_TorBoundariesGrid(wt_VTor,cond=velBC)

     ! ポロイダル速度場
     wt_LaplaVPol = wt_LaplaVPol &
           + delta_t *(3/2.0*wt_DtDLaplaVPol1 - 1/2.0*wt_DtDLaplaVPol0 ) &
           + delta_t/2.0 * wt_Lapla_wt(wt_LaplaVPol) 
     wt_VPol = wt_LaplaPol2PolGrid_wt(wt_LaplaVPol,cond=velBC)
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

     wt_LaplaVPol = LUSolve(DifLUMT_Vel,kpivot_Vel,wz_wt(wt_LaplaVPol))
     wt_VPol = wt_LaplaPol2PolGrid_wt(wt_LaplaVPol,cond=velBC)
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

     ! 温度場
     wt_Temp = wt_Temp + delta_t *( 3/2.0*wt_DtDTemp1 - 1/2.0*wt_DtDTemp0 ) &
                       + delta_t/2.0/Pr * wt_Lapla_wt(wt_Temp) 
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)
     wt_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,wz_wt(wt_Temp))
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

    real(8), dimension(0:im-1,jm,0:km)     :: xyz_VGradV_Lon   ! v・▽v(経度)
    real(8), dimension(0:im-1,jm,0:km)     :: xyz_VGradV_Lat   ! v・▽v(緯度)
    real(8), dimension(0:im-1,jm,0:km)     :: xyz_VGradV_Rad   ! v・▽v(動径)


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
  subroutine CNDiffusionMatrixGrid( Diffc, dt, DiffLUMatrix, kpivot )

    real(8), intent(IN)          :: Diffc    ! 拡散係数
    real(8), intent(IN)          :: dt       ! 時間刻

    ! Crank Nicholson 拡散陰的計算用行列(1-D dt/2▽^2, LU 分解)
    real(8), dimension((nm+1)*(nm+1),0:km,0:lm), intent(OUT)  :: DiffLUMatrix
    ! ピボット情報
    integer, dimension((nm+1)*(nm+1),0:km), intent(OUT)       :: kpivot

    ! 作業用変数
    real(8), dimension((nm+1)*(nm+1),0:lm)           :: wt_I

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! 各波数成分独立
       DiffLUMatrix(:,:,l) = - Diffc * dt/2.0 * wz_wt(wt_Lapla_wt(wt_I))
    enddo

    DiffLUMatrix(:,0,:) = 0.0D0 ; DiffLUMatrix(:,km,:) = 0.0D0 ! 境界値は保存

    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! 各波数成分独立
       DiffLUMatrix(:,:,l) = wz_wt(wt_I) + DiffLUMatrix(:,:,l)
    enddo
    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrixGrid

 !------------------- 初期値設定(ファイルなし) ----------------------
  subroutine set_initial_values(xyz_VTor, xyz_VPol, xyz_Temp)

    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_VTor !トロイダル速度
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_VPol !ポロイダル速度
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_Temp !温度

    real(8), dimension(0:im-1,jm,0:km)     :: xyz_X             ! x=2r-r_i-r_o

    xyz_X = 2*xyz_Rad - Ri - Ro

    xyz_Temp =  Ro*Ri/xyz_Rad - Ri &
       + 210*A/sqrt(17920*pi) * (1 - 3*xyz_X**2 + 3*xyz_X**4 -xyz_X**6) &
           * sin(pi/2-xyz_Lat)**4 * cos(4*xyz_Lon)

    xyz_VTor =  0     ! 静止状態
    xyz_VPol =  0     ! 静止状態

  end subroutine set_initial_values

 !------------------- 初期値設定(ファイルから) ----------------------  
  subroutine initial_read_gtool4( filename, time, &
                      xyz_VTor, xyz_VPol, xyz_Temp, &
                      wt_DtDVTor, wt_DtDLaplaVPol, wt_DtDTemp )

    character(len=*), intent(in)   :: filename
    real(8), intent(in)            :: time
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_VTor !トロイダル速度
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_VPol !ポロイダル速度
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_Temp ! 温度

 ! トロイダル速度時間変化 
    real(8), dimension((nm+1)**2,0:lm), intent(out) :: wt_DtDVTor
 ! ▽^2ポロイダル速度時間変化
    real(8), dimension((nm+1)**2,0:lm), intent(out) :: wt_DtDLaplaVPol
 ! 温度時間変化
    real(8), dimension((nm+1)**2,0:lm), intent(out) :: wt_DtDTemp

 ! 読み込み用配列
    real(8), dimension(0:im-1,jm,0:km) :: xyz_DtDVar

    character(len=100) :: varname, ctime

    write(ctime,*) time

    varname = trim(filename)//'@vtor,t='//trim(adjustl(ctime))
    call LoadVariable(varname, xyz_VTor, im*jm*(km+1))

    varname = trim(filename)//'@vpol,t='//trim(adjustl(ctime))
    call LoadVariable(varname, xyz_VPol, im*jm*(km+1))

    varname = trim(filename)//'@temp,t='//trim(adjustl(ctime))
    call LoadVariable(varname, xyz_Temp,   im*jm*(km+1))

    varname = trim(filename)//'@DtDVTor,t='//trim(adjustl(ctime))
    call LoadVariable(varname, xyz_DtDVar, im*jm*(km+1))
    wt_DtDVTor      = wt_xyz(xyz_DtDVar)

    varname = trim(filename)//'@DtDLaplaVPol,t='//trim(adjustl(ctime))
    call LoadVariable(varname, xyz_DtDVar, im*jm*(km+1))
    wt_DtDLaplaVPol = wt_xyz(xyz_DtDVar)

    varname = trim(filename)//'@DtDTemp,t='//trim(adjustl(ctime))
    call LoadVariable(varname, xyz_DtDVar, im*jm*(km+1))
    wt_DtDTemp      = wt_xyz(xyz_DtDVar)

  end subroutine initial_read_gtool4

 !------------------- 出力 ----------------------
  subroutine output_gtool4_init

    call HistoryCreate( &                                  ! ヒストリー作成
           file=trim(output_file), &
           title='Benchmark of dynamo model (case0)', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=real(time), interval=real(ndisp*delta_t), &
           xtypes=(/'real'/))

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
           varname='DtDVTor', dims=(/'lon','lat','rad','t  '/), & 
           longname='time variation of velocity toroidal potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='DtDLaplaVPol', dims=(/'lon','lat','rad','t  '/), & 
           longname='time variation of laplacian of velocity poloidal potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='DtDTemp', dims=(/'lon','lat','rad','t  '/), & 
           longname='time variation of temperature', &
           units='1', xtype='double')

   call HistoryAddVariable( &                                ! 変数定義
           varname='ek', dims=(/'t  '/), & 
           longname='kinetic energy', units='1', xtype='double')
   call HistoryAddVariable( &                                ! 変数定義
           varname='um', dims=(/'lat','rad','t  '/), & 
           longname='zonal flow', units='1', xtype='double')
   end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it, '  time = ', time
    call HistoryPut('t',real(time))
    call HistoryPut('vtor',xyz_wt(wt_VTor))
    call HistoryPut('vpol',xyz_wt(wt_VPol))
    call HistoryPut('temp',xyz_Temp)
    call HistoryPut('vlon',xyz_VLon)
    call HistoryPut('vlat',xyz_VLat)
    call HistoryPut('vrad',xyz_VRad)
    call HistoryPut('DtDVTor',xyz_wt(wt_DtDVTor1))
    call HistoryPut('DtDLaplaVPol',xyz_wt(wt_DtDLaplaVPol1))
    call HistoryPut('DtDTemp',xyz_wt(wt_DtDTemp1))

    call HistoryPut('ek',AvrLonLatRad_xyz((xyz_VLon**2+xyz_VLat**2+xyz_VRad**2)/2))
    call HistoryPut('um',yz_avrLon_xyz(xyz_VLon))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

  subroutine LoadVariable(var_uri, array, length)
    use gt4f90io
    ! 変数を指示する URI
    character(len = *), intent(in):: var_uri
    ! 配列とその長さ
    integer, intent(in):: length
    real(8), intent(out):: array(length)
    type(GT_VARIABLE):: var
    continue

    call open(var, var_uri)
    call get(var, array, length)
    call close(var)
  end subroutine LoadVariable

end program gami_abcn_bench0
