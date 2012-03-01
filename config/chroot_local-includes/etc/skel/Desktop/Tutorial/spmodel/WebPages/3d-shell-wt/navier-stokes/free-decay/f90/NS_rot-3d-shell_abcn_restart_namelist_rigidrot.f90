!--------------------------------------------------------------------------
!  Copyright (C) 2002--2008 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
!
!表題  回転球殻ナビエーストークス流体モデル : 
!      2nd order Adams-Bashforth + Crank Nicholson scheme
!      剛体回転初期値問題
!
!履歴  2005/02/11  竹広 真一   mhd-boussinesq モデル元に簡略化
!      2005/02/19  竹広 真一   剛体回転初期値用に改造
!      2008/08/10  竹広 真一   配列添え字変更 im -> 0:im-1
!
program NS_rot_3d_shell_abcn_restart_namelist

  use lumatrix
  use wt_module
  use dc_message, only : MessageNotify
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  ! T10 N8
  integer,parameter  :: im=32, jm=16, km=8  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=8         ! 切断波数の設定(水平, 動径)
  ! T10 N16
  !integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  !! T21 N16
  !integer,parameter  :: im=64, jm=32, km=16  ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter  :: nm=21, lm=16         ! 切断波数の設定(水平, 動径)
  !! T21 N24
  !integer,parameter  :: im=64, jm=32, km=24  ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter  :: nm=21, lm=24         ! 切断波数の設定(水平, 動径)
  !! T21 N32
  !integer,parameter  :: im=64, jm=32, km=32  ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter  :: nm=21, lm=32         ! 切断波数の設定(水平, 動径)
  !! T42 N32
  !integer,parameter  :: im=128, jm=64, km=32 ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter  :: nm=42, lm=32         ! 切断波数の設定(水平, 動径)
  !! T42 N48
  !integer,parameter  :: im=128, jm=64, km=48 ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter  :: nm=42, lm=48         ! 切断波数の設定(水平, 動径)
  !  T85 N32
  !integer,parameter :: im=256, jm=128, km=32 ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter :: nm=85, lm=32          ! 切断波数の設定(水平, 動径)
  !  T85 N48
  !integer,parameter :: im=256, jm=128, km=48 ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter :: nm=85, lm=48          ! 切断波数の設定(水平, 動径)
  !  T85 N64
  !integer,parameter :: im=256, jm=128, km=64 ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter :: nm=85, lm=64          ! 切断波数の設定(水平, 動径)
  !  T170 N48
  !integer,parameter :: im=512, jm=256, km=48 ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter :: nm=170, lm=48         ! 切断波数の設定(水平, 動径)
  !  T170 N64
  !integer,parameter :: im=512, jm=256, km=64 ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter :: nm=170, lm=64         ! 切断波数の設定(水平, 動径)
  !  T341 N80
  !integer,parameter :: im=1024, jm=512, km=80 ! 格子点の設定(経度, 緯度, 動径)
  !integer,parameter :: nm=341, lm=80          ! 切断波数の設定(水平, 動径)

 !---- 変数 ----
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Torvel      ! トロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_Torvel       ! トロイダル速度
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Polvel      ! ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_Polvel       ! ポロイダル速度
  real(8), dimension(0:im-1,jm,0:km) :: xyz_LaplaPolvel ! ポロイダル速度
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaPolvel  ! ▽^2ポロイダル速度

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Vlon       ! 速度(経度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Vlat       ! 速度(緯度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Vrad       ! 速度(動径)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Zlon       ! 渦度(経度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Zlat       ! 渦度(緯度)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Zrad       ! 渦度(動径)

 !---- 時間積分用変数 ----

 ! トロイダル速度時間変化
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDTorvel1, wt_DtDTorvel0
 ! ▽^2ポロイダル速度時間変化
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDLaplaPolvel1, wt_DtDLaplaPolvel0

 ! Crank Nicholson 陰的計算用拡散行列(速度場)
  real(8), dimension((nm+1)*(nm+1),0:lm,0:lm)   :: DifLUMT_Vel
  integer, dimension((nm+1)*(nm+1),0:lm)        :: kpivot_Vel     ! ピボット

 !---- その他 ----
  real(8), parameter :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time

 !---- NAMELIST 変数 ----
  logical            :: verbose=.false.          ! 出力メッセージレベル
  namelist /message/  verbose                    !
                                                 !-- 座標変数など --
  real(8)            :: ri=0.0D0                 ! 内半径
  real(8)            :: ro=0.0D00                ! 外半径
  real(8)            :: eta=0.0D0                ! 内外半径比
  namelist /radius/  ri, ro, eta
                                                 ! -- 初期値 --
  character(len=100) :: initial_file=''          ! 初期値データファイル名
  real               :: initial_time=0.0         ! 初期時刻
  namelist /initial/ initial_file, initial_time  ! 
                                                 ! -- 時間積分 --
  real(8) :: delta_t=1.0e-7                      ! 時間積分刻み
  integer :: nstep=2000                          ! 時間積分ステップ数
  namelist /tint/    delta_t, nstep              ! 
                                                 ! -- 出力 --
  character(len=100) :: output_file=''           ! 出力ファイル名
  character(len=100) :: title = &                ! タイトル
            'Navier-Stokes equation in a 3-dim spherical shell'!
  integer :: ndisp=200                           ! 出力間隔ステップ数
  namelist /output/    output_file, title, ndisp ! 
                                                 ! -- 物理パラメター --
  real(8)            :: Rs=0.0D0                 ! ロスビー数
  real(8)            :: Re=0.0D0                 ! レイノルズ数
  real(8)            :: RsInv=0.0D0              ! 1/ロスビー数
  real(8)            :: ReInv=0.0D0              ! 1/レイノルズ数
  namelist /physics/  Rs, Re                     ! 
                                                 ! -- 境界条件 --
  character(len=2)   :: VelBC='FF'               ! 速度境界条件(RR/RF/FR/FF)
  namelist /boundary/ VelBC                      !
                                                 ! -- 初期条件 --
  real(8)            :: Omega=1.0D0              ! 回転角速度
  real(8)            :: LonOmega=0.0D0           ! 回転軸経度
  real(8)            :: LatOmega=90.0D0          ! 回転軸緯度
  namelist /rotation/ Omega, LonOmega, LatOmega  !

 !---------------- NAMELIST 読み込み ---------------------
  write(6,nml=message)  ; read(5,nml=message) ; write(6,nml=message)

  if (verbose) write(6,nml=radius) ; read(5,nml=radius)
   !
   ! ri,ro か eta のどちらかを NAMELIST radius で与える. eta の設定が優先される
   !
  if ( eta /= 0.0D0 ) then
     ri=eta/(1.0D0-eta) ; ro=1.0D0/(1.0D0-eta)
     call MessageNotify('M','main', &
             'The inner/outer radii were calculatied with their ratio, eta.')
  else if ( ro /= 0.0D0 .and. ro /= 0.0D0 ) then
     eta = ri/ro
     call MessageNotify('M','main', &
             'The ratio of inner/outer radii were calculatied with ro and ri.')
  else
     call MessageNotify('E','main', &
                 'The inner/outer radii or their ratio should be given')
  endif
  write(6,nml=radius)

  if(verbose)write(6,nml=initial) ; read(5,nml=initial) ;write(6,nml=initial)
  if(verbose)write(6,nml=tint)    ; read(5,nml=tint)    ;write(6,nml=tint)
  if(verbose)write(6,nml=output)  ; read(5,nml=output)  ;write(6,nml=output)

  if(verbose)write(6,nml=physics) ; read(5,nml=physics)
  if ( Rs == 0.0D0 ) then
     RsInv = 0.0D0
     call MessageNotify('M','main', &
             'Inverse of Rossby Number is set to zero.')
  else
     RsInv = 1.0D0/Rs
  endif
  if ( Re == 0.0D0 ) then
     ReInv = 0.0D0
     call MessageNotify('M','main', &
             'Inverse of Reynolds Number is set to zero.')
  else
     ReInv = 1.0D0/Re
  endif
  write(6,nml=physics)

  if(verbose)write(6,nml=boundary); read(5,nml=boundary);write(6,nml=boundary)
  if(verbose)write(6,nml=rotation); read(5,nml=rotation);write(6,nml=rotation)

 !---------------- 座標値の設定 ---------------------
  call wt_Initial(im,jm,km,nm,lm,ri,ro)

 !------------------- 初期値設定 ----------------------
  if ( initial_file == "") then
     call set_initial_values( xyz_Torvel, xyz_Polvel )

  else
     call initial_read_gtool4(initial_file, initial_time, &
          xyz_Torvel, xyz_Polvel, wt_DtDTorvel1, wt_DtDLaplaPolvel1 )
  endif

  time = initial_time
  
  wt_Torvel = wt_xyz(xyz_Torvel) 
  wt_Polvel = wt_xyz(xyz_Polvel)
  wt_LaplaPolvel = wt_Lapla_wt(wt_Polvel)
  xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)

  call wt_Potential2Vector(xyz_Vlon, xyz_Vlat, xyz_Vrad, wt_Torvel, wt_Polvel)

  if ( initial_file == "") then
     call TimeDerivetives_noDiffusion( &
          wt_Torvel, wt_Polvel, wt_LaplaPolvel, &
          xyz_Vlon, xyz_Vlat, xyz_Vrad, &
          wt_DtDTorvel1, wt_DtDLaplaPolvel1 )
  endif

  call CNDiffusionMatrix( ReInv, delta_t, DifLUMT_Vel, kpivot_Vel )

 !------------------- 時間積分(Adams-Bashforth 法) --------------------
  call output_gtool4_init
  if ( initial_file == '' ) call output_gtool4    ! 内部で与えた初期値は出力

  do it=1,nstep
     time = initial_time + it * delta_t

     ! 昔の時間変化項を保存
     wt_DtDTorvel0 = wt_DtDTorvel1
     wt_DtDLaplaPolvel0 = wt_DtDLaplaPolvel1

     call TimeDerivetives_noDiffusion( &
       wt_Torvel, wt_Polvel, wt_LaplaPolvel, &
       xyz_Vlon, xyz_Vlat, xyz_Vrad, &
       wt_DtDTorvel1, wt_DtDLaplaPolvel1 )

     ! トロイダル速度場
     wt_Torvel = wt_Torvel + delta_t *( 3/2.0*wt_DtDTorvel1 - 1/2.0*wt_DtDTorvel0 ) &
                           + delta_t/2.0 * ReInv * wt_Lapla_wt(wt_Torvel) 
     wt_Torvel = LUSolve(DifLUMT_Vel,kpivot_Vel,wt_Torvel)

     ! ポロイダル速度場
     wt_LaplaPolvel = wt_LaplaPolvel &
           + delta_t *(3/2.0*wt_DtDLaplaPolvel1 - 1/2.0*wt_DtDLaplaPolvel0 ) &
           + delta_t/2.0 * ReInv * wt_Lapla_wt(wt_LaplaPolvel) 
     wt_LaplaPolvel = LUSolve(DifLUMT_Vel,kpivot_Vel,wt_LaplaPolvel)

     ! 境界条件
     call wt_TorBoundariesGrid(wt_Torvel,cond=velBC)
     wt_Polvel = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaPolvel),cond=velBC))
     wt_LaplaPolvel = wt_Lapla_wt(wt_Polvel)

     ! グリッド値計算
     xyz_Torvel = xyz_wt(wt_Torvel)
     xyz_Polvel = xyz_wt(wt_Polvel)
     xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)

     call wt_Potential2Vector(xyz_Vlon,xyz_Vlat,xyz_Vrad,wt_Torvel,wt_Polvel)

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

! 以上 メインプログラム 
!--------------------------------------------------------------------------------
! 以下 サブルーチン
  !------------------- 時間変化項(拡散項以外) ----------------------
  subroutine TimeDerivetives_noDiffusion( &
       wt_Torvel, wt_Polvel, wt_LaplaPolvel, &
       xyz_Vlon, xyz_Vlat, xyz_Vrad, &
       wt_DtDTorvel, wt_DtDLaplaPolvel )

   ! トロイダル速度
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Torvel
   ! ポロイダル速度
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Polvel
   ! ▽^2 ポロイダル速度
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_LaplaPolvel
   ! 温度

    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_Vlon   ! 速度(経度)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_Vlat   ! 速度(緯度)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_Vrad   ! 速度(動径)

   ! トロイダル速度時間変化項
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDTorvel
   ! ▽^2 ポロイダル速度時間変化項
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDLaplaPolvel

    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradxyz_Vlon   ! v・▽v(経度)
    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradxyz_Vlat   ! v・▽v(緯度)
    real(8), dimension(0:im-1,jm,0:km) :: xyz_VGradxyz_Vrad   ! v・▽v(動径)

   ! v・▽v 非線形項計算
    call wt_VGradV(xyz_VGradxyz_Vlon,xyz_VGradxyz_Vlat,xyz_VGradxyz_Vrad, &
                   xyz_Vlon, xyz_Vlat, xyz_Vrad )

  ! トロイダル時間変化
    wt_DtDTorvel = &
           wt_L2Inv_wt(  RsInv * wt_KxRGrad_wt(wt_Torvel)   &
                       - RsInv * wt_Qoperator_wt(wt_Polvel) ) &
         - wt_L2Inv_wt(wt_RadRot_xyz_xyz(xyz_VGradxyz_Vlon,xyz_VGradxyz_Vlat )) 
  ! ポロイダル時間変化
    wt_DtDLaplaPolvel = &
           wt_L2Inv_wt(  RsInv * wt_KxRGrad_wt(wt_LaplaPolvel) &
                       + RsInv * wt_Qoperator_wt(wt_Torvel) )  &
         + wt_L2Inv_wt(wt_RadRotRot_xyz_xyz_xyz(                 &
               xyz_VGradxyz_Vlon, xyz_VGradxyz_Vlat, xyz_VGradxyz_Vrad ))

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

 !------------------- 初期値設定(ファイルなし) ----------------------
  subroutine set_initial_values(xyz_Torvel, xyz_Polvel )

    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_Torvel ! トロイダル速度
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_Polvel ! ポロイダル速度

    real(8), parameter  :: pi=3.1415926535897932385D0

    ! 速度場

   ! 剛体回転場
    xyz_Torvel =  Omega * xyz_Rad                                &
                   * ( cos(LatOmega/180.0*pi)*cos(xyz_Lat)       &
                         *cos(LonOmega/180.0D0*pi-xyz_Lon)       &
                       + sin(LatOmega/180.0D0*pi)*sin(xyz_Lat) )
    xyz_Polvel =  0.0D0

  end subroutine set_initial_values

 !------------------- 初期値設定(ファイルから) ----------------------  
  subroutine initial_read_gtool4( filename, time, &
       xyz_Torvel, xyz_Polvel, wt_DtDTorvel, wt_DtDLaplaPolvel )


    character(len=*), intent(in)   :: filename
    real, intent(in)               :: time
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_Torvel  ! トロイダル速度
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_Polvel  ! ポロイダル速度
 ! トロイダル速度時間変化 
    real(8), dimension((nm+1)**2,0:lm), intent(out) :: wt_DtDTorvel
 ! ▽^2ポロイダル速度時間変化
    real(8), dimension((nm+1)**2,0:lm), intent(out) :: wt_DtDLaplaPolvel

 ! 読み込み用配列
    real(8), dimension(0:im-1,jm,0:km) :: xyz_DtDVar

    call HistoryGet( trim(filename), 'torvel', xyz_Torvel, time )
    call HistoryGet( trim(filename), 'polvel', xyz_Polvel, time )

    call HistoryGet( trim(filename), 'DtDTorvel', xyz_DtDVar, time )
      wt_DtDTorvel = wt_xyz(xyz_DtDVar)
    call HistoryGet( trim(filename), 'DtDLaplaPolvel', xyz_DtDVar, time )
      wt_DtDLaplaPolvel = wt_xyz(xyz_DtDVar)

  end subroutine initial_read_gtool4

 !------------------- 出力 ----------------------
  subroutine output_gtool4_init

   !---- ヒストリーファイル作成 ----
    call HistoryCreate( &
           file=trim(output_file), &
           title=trim(title), &
           source='NS_rot-3d-shell_abcn_restart_namelist.f90 (2008/08/10)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=real(time), interval=real(ndisp*delta_t), &
           xtypes=(/'double','double','double','double'/))

   !---- 座標変数定義, 出力 ----
    call HistoryPut('lon',x_Lon/pi*180)                       ! 変数出力
    call HistoryAddattr('lon','topology','circular')          ! 周期属性
    call HistoryAddattr('lon','modulo',360.0)                 ! 周期属性
    call HistoryPut('lat',y_Lat/pi*180)                       ! 変数出力
    call HistoryPut('rad',z_Rad)                              ! 変数出力

   !---- 物理変数定義 ----
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
           varname='vlon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vlat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vrad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-velocity', units='1', xtype='double')

   !---- 時間変化項定義 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='DtDTorvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='time variation of velocity toroidal potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='DtDLaplaPolvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='time variation of laplacian of velocity poloidal potential', &
           units='1', xtype='double')

   !---- その他診断量定義 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='zlon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-vorticity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='zlat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-vorticity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='zrad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-voticity', units='1', xtype='double')

    call HistoryAddVariable( &                                ! 変数定義
           varname='ek', dims=(/'t  '/), & 
           longname='mean kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='um', dims=(/'lat','rad','t  '/), & 
           longname='zonal flow', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='hel', dims=(/'lon','lat','rad','t  '/), & 
           longname='helicity', units='1', xtype='double')

   !---- 実験パラメターを属性として定義, 出力(全て Global 属性) ----
    call HistoryAddAttr('lon','+Radius', (/ri,ro,eta/) )
    call HistoryAddAttr('lon','+delta_t', delta_t  )

    call HistoryAddAttr('lon', '+Rossby',   Rs    )
    call HistoryAddAttr('lon', '+Reynolds', Re    )

    call HistoryAddAttr('lon', '+VelBoundary',  VelBC  )

   end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it, '  time = ', time
    call HistoryPut('t',real(time))

   !---- 物理変数出力 ----
    call HistoryPut('torvel',xyz_Torvel)
    call HistoryPut('polvel',xyz_Polvel)
    call HistoryPut('laplapolvel',xyz_LaplaPolvel)
    call HistoryPut('vlon',xyz_Vlon)
    call HistoryPut('vlat',xyz_Vlat)
    call HistoryPut('vrad',xyz_Vrad)

   !---- 時間変化項出力 ----
    call HistoryPut('DtDTorvel',xyz_wt(wt_DtDTorvel1))
    call HistoryPut('DtDLaplaPolvel',xyz_wt(wt_DtDLaplaPolvel1))

   !---- 診断量出力 ----
    xyz_Zlon = xyz_RotLon_wt_wt(wt_xyz(xyz_Vrad),wt_xyz(xyz_Vlat))
    xyz_Zlat = xyz_RotLat_wt_wt(wt_xyz(xyz_Vlon),wt_xyz(xyz_Vrad))
    xyz_Zrad = xyz_wt(wt_RotRad_xyz_xyz(xyz_Vlat,xyz_Vlon))

    call HistoryPut('zlon',xyz_Zlon)
    call HistoryPut('zlat',xyz_Zlat)
    call HistoryPut('zrad',xyz_Zrad)

    call HistoryPut('ek',AvrLonLatRad_xyz((xyz_Vlon**2+xyz_Vlat**2+xyz_Vrad**2)/2))
    call HistoryPut('um',yz_avrLon_xyz(xyz_Vlon))
    call HistoryPut('hel',xyz_Vlon*xyz_Zlon + xyz_Vlat*xyz_Zlat + xyz_Vrad*xyz_Zrad)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program NS_rot_3d_shell_abcn_restart_namelist
