!--------------------------------------------------------------------------
!  Copyright (C) 2002--2008 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
!
!表題  回転球殻ブシネスク磁気流体モデル
!
!      * 球面調和函数変換 + チェビシェフ選点法
!      * 時間積分 Adams-Bashforth + Crank Nicholson スキーム
!      * ダイナモベンチマーク用設定
!
!履歴  2002/11/01  佐々木洋平  ベンチマーク Case 1 リスタート可能バージョン
!      2002/11/02  佐々木洋平  spmodel 書法に従う修正, 読み込み初期値のポテンシャル化
!      2002/11/19  佐々木洋平  初期時刻の読み込みの修正
!      2002/11/19  佐々木洋平  境界条件をタウ法から変更 
!      2004/02/16  佐々木 洋平 gt4f90io の使用に伴うライブラリ名変更
!      2004/11/26  竹広 真一   Adams-Bashforth scheme 用の各時間変化量を
!                              リスタート変数に追加
!      2004/11/27  竹広 真一   内部初期条件の設定ルーチン追加.
!      2004/12/30  竹広 真一   HistoryCreate に xtype を追加
!                              (Alpha machine での出力時エラーへの暫定対処).
!                              結果を出力した時間を表示. 
!      2005/01/03  竹広 真一   解像度追加, 外部ファイルで与えた初期値は出力せず
!      2005/01/04  竹広 真一   HistoryCreate のオプション xtypes typo 修正
!
!      2005/01/06  竹広 真一   ベンチマークプログラムより一般化
!                              球殻半径, 物理パラメター, 境界条件を namelist 化
!                              変数読み込みを HistoryGet で行う. 
!      2005/01/07  竹広 真一   Namelist 変数等を属性として出力. 
!      2005/01/19  竹広 真一   半径比で球殻内外半径を設定可能にする. 
!      2005/01/20  竹広 真一   渦度, 電流, ヘリシティーの計算追加. 
!      2005/01/25  竹広 真一   タイトルを Namelist で設定可能にする. 
!      2005/02/04  竹広 真一   磁場拡散項の係数 1/Pm が抜けていたのを修正. 
!      2005/02/11  竹広 真一   係数 1/Pm の修正再度. 
!      2005/04/17  竹広 真一   解像度を Namelist で指定
!                              エネルギースペクトルの出力追加
!      2005/04/29  竹広 真一   非線形項を V・▽V から (▽xV)xV に変更
!                              渦度の計算をポテンシャルから直接行なう. 
!      2005/05/09  竹広 真一   初期値のタイプ(case0/case1)のスイッチ導入
!      2005/05/19  竹広 真一   Clank Nicolson 第 1 段後に境界条件計算を追加
!      2006/02/19  竹広 真一   Crank-Nicolson 行列・計算変更
!      2006/02/21  竹広 真一   ソース名(日付)をログに出力
!      2006/02/28  竹広 真一   RDoc 用にコメント変更, ファイル改名
!      2007/06/26  竹広 真一   lon_weight, lat_weight 出力
!      2007/09/13  竹広 真一   時間積分前処理を変更
!      2007/12/19  竹広 真一   温度 Crank-Nicolson 行列を修正
!      2007/12/20  竹広真一    コマンドライン引数を解析し, 
!                              NAMELIST ファイル名を取得
!      2008/01/25  竹広 真一   磁場エネルギー計算 bug fix
!      2008/06/11  竹広 真一   OPENMP スイッチ導入
!      2008/08/09  竹広 真一   経度添え字を im->0:im-1 に変更
!
program MHD_bsnsq_rot_3d_shell_wt_abcn_bench
  !
  !  回転球殻ブシネスク磁気流体モデル
  !
  !   * 球面調和函数変換 + チェビシェフガラーキン法
  !   * 時間積分 Adams-Bashforth + Crank Nicholson スキーム
  !   * ダイナモベンチマーク用設定
  !
  use lumatrix
  use at_module,  only : a_Int_ag
  use wt_module
  use dc_trace,   only : SetDebug, BeginSub, EndSub, DbgMessage
  use dc_message, only : MessageNotify
  use dc_types
  use dc_string,   only : StoA
  use dc_args
  use gt4_history
  implicit none

!== 宣言部 ============================================================

 !---- 変数 ----
  real(8), allocatable :: xyz_Torvel(:,:,:)      ! トロイダル速度
  real(8), allocatable :: wt_Torvel(:,:)         ! トロイダル速度
  real(8), allocatable :: xyz_Polvel(:,:,:)      ! ポロイダル速度
  real(8), allocatable :: wt_Polvel(:,:)         ! ポロイダル速度
  real(8), allocatable :: xyz_LaplaPolvel(:,:,:) ! ポロイダル速度
  real(8), allocatable :: wt_LaplaPolvel(:,:)    ! ▽^2ポロイダル速度

  real(8), allocatable :: xyz_Tormag(:,:,:)      ! トロイダル磁場
  real(8), allocatable :: wt_Tormag(:,:)         ! トロイダル磁場
  real(8), allocatable :: xyz_Polmag(:,:,:)      ! ポロイダル磁場
  real(8), allocatable :: wt_Polmag(:,:)         ! ポロイダル磁場

  real(8), allocatable :: xyz_Temp(:,:,:)       ! 温度
  real(8), allocatable :: wt_Temp(:,:)          ! 温度

  real(8), allocatable :: xyz_Vlon(:,:,:)       ! 速度(経度)
  real(8), allocatable :: xyz_Vlat(:,:,:)       ! 速度(緯度)
  real(8), allocatable :: xyz_Vrad(:,:,:)       ! 速度(動径)

  real(8), allocatable :: xyz_Blon(:,:,:)       ! 磁場(経度)
  real(8), allocatable :: xyz_Blat(:,:,:)       ! 磁場(緯度)
  real(8), allocatable :: xyz_Brad(:,:,:)       ! 磁場(動径)

  real(8), allocatable :: xyz_Zlon(:,:,:)       ! 渦度(経度)
  real(8), allocatable :: xyz_Zlat(:,:,:)       ! 渦度(緯度)
  real(8), allocatable :: xyz_Zrad(:,:,:)       ! 渦度(動径)

  real(8), allocatable :: xyz_Jlon(:,:,:)       ! 電流(経度)
  real(8), allocatable :: xyz_Jlat(:,:,:)       ! 電流(緯度)
  real(8), allocatable :: xyz_Jrad(:,:,:)       ! 電流(動径)

  real(8), allocatable :: xy_TempBndry(:,:,:)   ! 温度境界値
  real(8), allocatable :: w_TempBndry(:,:)      ! 温度境界値スペクトル

  real(8), allocatable :: nz_EkTor(:,:)   ! 運動エネルギー(トロイダル成分)
  real(8), allocatable :: nz_EkPol(:,:)   ! 運動エネルギー(ポロイダル成分)

  real(8), allocatable :: nz_EmTor(:,:)   ! 磁場エネルギー(トロイダル成分)
  real(8), allocatable :: nz_EmPol(:,:)   ! 磁場エネルギー(ポロイダル成分)

  real(8), allocatable :: nz_EnsTor(:,:)  ! エンストロフィー(トロイダル成分)
  real(8), allocatable :: nz_EnsPol(:,:)  ! エンストロフィー(ポロイダル成分)

 !---- 時間積分用変数 ----

  real(8), allocatable :: wt_DtDTorvel1(:,:), wt_DtDTorvel0(:,:)
                          ! トロイダル速度時間変化
  real(8), allocatable :: wt_DtDLaplaPolvel1(:,:), wt_DtDLaplaPolvel0(:,:)
                          ! ▽^2ポロイダル速度時間変化
  real(8), allocatable :: wt_DtDTormag1(:,:), wt_DtDTormag0(:,:)
                          ! トロイダル磁場時間変化
  real(8), allocatable :: wt_DtDPolmag1(:,:), wt_DtDPolmag0(:,:)
                          ! ポロイダル磁場時間変化
  real(8), allocatable :: wt_DtDTemp1(:,:), wt_DtDTemp0(:,:)
                          ! 温度時間変化

  real(8), allocatable :: DifLUMT_Torvel(:,:,:)
                          ! Crank Nicholson 陰的計算行列(トロイダル速度場)
  integer, allocatable :: kpivot_Torvel(:,:)
                          ! Crank Nicholson 行列ピボット(トロイダル速度場)

  real(8), allocatable :: DifLUMT_Polvel(:,:,:)
                          ! Crank Nicholson 陰的計算行列
                          ! (ポロイダル速度場)
  integer, allocatable :: kpivot_Polvel(:,:)
                          ! Crank Nicholson 行列ピボット(ポロイダル速度場)

  real(8), allocatable :: DifLUMT_Tormag(:,:,:)
                          ! Crank Nicholson 陰的計算行列(トロイダル磁場)
  integer, allocatable :: kpivot_Tormag(:,:)
                          ! Crank Nicholson 行列ピボット(トロイダル磁場)

  real(8), allocatable :: DifLUMT_Polmag(:,:,:)
                          ! Crank Nicholson 陰的計算行列(ポロイダル磁場)
  integer, allocatable :: kpivot_Polmag(:,:)
                          ! Crank Nicholson 行列ピボット(ポロイダル磁場)

  real(8), allocatable :: DifLUMT_Temp(:,:,:)
                          ! Crank Nicholson 陰的計算行列(温度場)
  integer, allocatable :: kpivot_Temp(:,:)    ! ピボット
                          ! Crank Nicholson 陰的計算行列ピボット(温度場)

 !---- その他 ----
  real(8), parameter :: pi=3.1415926535897932385D0
  character(len=20)  :: DbgMessageFmt='*** DbgMESSAGE ***  '
  real(8), parameter  :: vmiss = -999.0       ! 欠損値
  integer :: it=0
  real(8) :: time
  integer :: n, m, l

 !---- NAMELIST 変数 ----
  ! コマンドライン引数用変数
  type(ARGS) :: arg
  character(STRING), pointer :: argv(:) => null()

  ! NAMELIST 入力用デフォルトファイル名
  character(STRING)  :: nmlfile='MHD-bsnsq_rot-3d-shell_wt_abcn_bench.nml'

  logical            :: verbose=.false.          ! 出力メッセージレベル
  logical            :: DebugOn=.false.          ! デバッグ出力コントロール
  namelist /message/  Verbose, DebugOn           !

  integer  :: im=64, jm=32, km=16                ! 格子点(経度, 緯度, 動径)
  integer  :: nm=21, lm=16                       ! 切断波数(水平, 動径)
  integer  :: np=1                               ! OPENMP プロセス数
  namelist /gridset/ im, jm, km, nm, lm, np      !-- 格子点・スペクトル --

  real(8)            :: ri=0.0D0                 ! 内半径
  real(8)            :: ro=0.0D00                ! 外半径
  real(8)            :: eta=0.0D0                ! 内外半径比
  namelist /radius/  ri, ro, eta                 !-- 座標変数など --

  character(STRING)  :: initial_file=''          ! 初期値データファイル名
  real               :: initial_time=0.0         ! 初期時刻
  namelist /initial/ initial_file, initial_time  ! -- 初期値 --

  real(8) :: delta_t=1.0e-7                      ! 時間積分刻み
  integer :: nstep=2000                          ! 時間積分ステップ数
  namelist /tint/    delta_t, nstep              ! -- 時間積分 --

  character(STRING)  :: output_file=''           ! 出力ファイル名
  character(STRING)  :: title = &                ! タイトル
            'MHD Dynamo in 3-dim spherical shell'!
  character(STRING)  :: source = &               ! ソース名
            'MHD-bsnsq_rot-3d-shell_bench_wt_abcn.f90 (2008/08/09)'!
  integer :: ndisp=200                           ! 出力間隔ステップ数
  namelist /output/    output_file, title, ndisp ! -- 出力 --

  real(8)            :: Ra=100.0D0               ! レイリー数
  real(8)            :: Pr=1.0D0                 ! プランドル数
  real(8)            :: Ekman=1.0d-3             ! エクマン数
  real(8)            :: Pm=5.0D0                 ! 磁気プランドル数
  namelist /physics/  Ra, Pr, Ekman, Pm          ! -- 物理パラメター --

  character(len=2)   :: VelBC='RR'               ! 速度境界条件(RR/RF/FR/FF)
  character(len=2)   :: TempBC='DD'              ! 温度境界条件(DD/DN/ND/NN)
  real(8)            :: Temptop=0.0D0            ! 境界温度(上端)
  real(8)            :: Tempbottom=1.0D0         ! 境界温度(下端)
  namelist /boundary/ VelBC, TempBC, Temptop, Tempbottom
                                                 ! -- 境界条件 --

 !---------------- NAMELIST 読み込み ---------------------
  call Open(arg)
  call Debug(arg) ; call Help(arg) ; call Strict(arg)
  call Get(arg, argv)
  if ( size(argv) .le. 0 ) then
     call MessageNotify('W','main',&
          'Usage: MHD_bsnsq_rot_3d_shell_wt_abcn_bench.out [nmlfilename]')
     call MessageNotify('E','main','There is no argument. ')
  else
     nmlfile=argv(1)
     call MessageNotify('M','main','Namelist file is '//trim(nmlfile))
  endif
  deallocate(argv)
  call Close(arg)

  call MessageNotify('M','main', trim(source))

  write(6,nml=message) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=message) ; write(6,nml=message) ; close(10)

  if(verbose) write(6,nml=gridset)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=gridset)  ; write(6,nml=gridset) ; close(10)

  if (verbose) write(6,nml=radius)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=radius)
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
  write(6,nml=radius)  ; close(10)

  if(verbose)write(6,nml=initial)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=initial) ; write(6,nml=initial) ; close(10)

  if(verbose)write(6,nml=tint)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=tint)     ; write(6,nml=tint)    ; close(10)

  if(verbose)write(6,nml=output)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=output)   ; write(6,nml=output)  ; close(10)

  if(verbose)write(6,nml=physics)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=physics)  ; write(6,nml=physics) ; close(10)

  if(verbose)write(6,nml=boundary)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=boundary) ; write(6,nml=boundary); close(10)

 !---------------- デバッグ出力制御設定 -----------------
  if (DebugOn) then
    call SetDebug
  end if

 !------------------ 変数の割り付け ---------------------
  allocate(xyz_Torvel(0:im-1,jm,0:km),wt_Torvel((nm+1)**2,0:lm))
  allocate(xyz_Polvel(0:im-1,jm,0:km),wt_Polvel((nm+1)**2,0:lm))
  allocate(xyz_LaplaPolvel(0:im-1,jm,0:km),wt_LaplaPolvel((nm+1)**2,0:lm))

  allocate(xyz_Tormag(0:im-1,jm,0:km),wt_Tormag((nm+1)**2,0:lm))
  allocate(xyz_Polmag(0:im-1,jm,0:km),wt_Polmag((nm+1)**2,0:lm))

  allocate(xyz_Temp(0:im-1,jm,0:km),wt_Temp((nm+1)**2,0:lm))

  allocate(xyz_Vlon(0:im-1,jm,0:km))        ! 速度(経度)
  allocate(xyz_Vlat(0:im-1,jm,0:km))        ! 速度(緯度)
  allocate(xyz_Vrad(0:im-1,jm,0:km))        ! 速度(動径)

  allocate(xyz_Zlon(0:im-1,jm,0:km))        ! 渦度(経度)
  allocate(xyz_Zlat(0:im-1,jm,0:km))        ! 渦度(緯度)
  allocate(xyz_Zrad(0:im-1,jm,0:km))        ! 渦度(動径)

  allocate(xyz_Blon(0:im-1,jm,0:km))        ! 磁場(経度)
  allocate(xyz_Blat(0:im-1,jm,0:km))        ! 磁場(緯度)
  allocate(xyz_Brad(0:im-1,jm,0:km))        ! 磁場(動径)

  allocate(xyz_Jlon(0:im-1,jm,0:km))        ! 電流(経度)
  allocate(xyz_Jlat(0:im-1,jm,0:km))        ! 電流(緯度)
  allocate(xyz_Jrad(0:im-1,jm,0:km))        ! 電流(動径)

  allocate(xy_TempBndry(0:im-1,jm,2))       ! 温度境界値
  allocate(w_TempBndry((nm+1)**2,2))    ! 温度境界値スペクトル

  allocate(nz_EkTor(0:nm,0:km))         ! 運動エネルギー(トロイダル成分)
  allocate(nz_EkPol(0:nm,0:km))         ! 運動エネルギー(ポロイダル成分)

  allocate(nz_EmTor(0:nm,0:km))         ! 磁場エネルギー(トロイダル成分)
  allocate(nz_EmPol(0:nm,0:km))         ! 磁場エネルギー(ポロイダル成分)

  allocate(nz_EnsTor(0:nm,0:km))        ! エンストロフィー(トロイダル成分)
  allocate(nz_EnsPol(0:nm,0:km))        ! エンストロフィー(ポロイダル成分)

 ! トロイダル速度時間変化
  allocate(wt_DtDTorvel1((nm+1)**2,0:lm),wt_DtDTorvel0((nm+1)**2,0:lm))

 ! ▽^2ポロイダル速度時間変化
  allocate(wt_DtDLaplaPolvel1((nm+1)**2,0:lm),wt_DtDLaplaPolvel0((nm+1)**2,0:lm))
 ! トロイダル磁場時間変化
  allocate(wt_DtDTormag1((nm+1)**2,0:lm),wt_DtDTormag0((nm+1)**2,0:lm))

 ! ポロイダル磁場時間変化
  allocate(wt_DtDPolmag1((nm+1)**2,0:lm),wt_DtDPolmag0((nm+1)**2,0:lm))

 ! 温度時間変化
  allocate(wt_DtDTemp1((nm+1)**2,0:lm),wt_DtDTemp0((nm+1)**2,0:lm))

 ! Crank Nicholson 陰的計算用拡散行列(トロイダル速度場)
  allocate(DifLUMT_Torvel((nm+1)*(nm+1),0:lm,0:lm))
  allocate(kpivot_Torvel((nm+1)*(nm+1),0:lm))          ! ピボット

 ! Crank Nicholson 陰的計算用拡散行列(ポロイダル速度場)
  allocate(DifLUMT_Polvel((nm+1)*(nm+1),0:lm,0:lm))
  allocate(kpivot_Polvel((nm+1)*(nm+1),0:lm))          ! ピボット

 ! Crank Nicholson 陰的計算用拡散行列(トロイダル磁場)
  allocate(DifLUMT_Tormag((nm+1)*(nm+1),0:lm,0:lm))
  allocate(kpivot_Tormag((nm+1)*(nm+1),0:lm))          ! ピボット

 ! Crank Nicholson 陰的計算用拡散行列(ポロイダル磁場)
  allocate(DifLUMT_Polmag((nm+1)*(nm+1),0:lm,0:lm))
  allocate(kpivot_Polmag((nm+1)*(nm+1),0:lm))          ! ピボット

 ! Crank Nicholson 陰的計算用拡散行列(温度場)
  allocate(DifLUMT_Temp((nm+1)*(nm+1),0:lm,0:lm))
  allocate(kpivot_Temp((nm+1)*(nm+1),0:lm))          ! ピボット

 !---------------- 座標値の設定 ---------------------
  call DbgMessage(fmt='call %c', c1='wt_Initial') 

  if ( np .gt. 1 ) then
     call wt_Initial(im,jm,km,nm,lm,ri,ro,np=np)
  else
     call wt_Initial(im,jm,km,nm,lm,ri,ro)
  endif

  wt_VMiss = vmiss

 !------------------- 初期値設定 ----------------------
  if ( initial_file == "") then
     call set_initial_values 
       ! xyz_Torvel,xyz_Polvel,xyz_Tormag,xyz_Polmag,xyz_Temp を設定する
  else
     call initial_read_gtool4 
       ! xyz_Torvel,xyz_Polvel,xyz_Tormag,xyz_Polmag,xyz_Temp,
       ! wt_DtDTorvel1, wt_DtDLaplaPolvel1, 
       ! wt_DtDTormag1, wt_DtDPolmag1, wt_DtDTemp1 を読み込む
  endif

 !------------------- 境界値設定 ----------------------
  xy_Tempbndry(:,:,1) = Temptop
  xy_Tempbndry(:,:,2) = Tempbottom

  w_TempBndry(:,1) = w_xy(xy_TempBndry(:,:,1))
  w_TempBndry(:,2) = w_xy(xy_TempBndry(:,:,2))

 !------------------- Crank-Nicoslon 用行列設定 ----------------------
  call CNDiffusionMatrixTorvel( 1.0D0, delta_t, DifLUMT_Torvel, kpivot_Torvel )
  call CNDiffusionMatrixPolvel( 1.0D0, delta_t, DifLUMT_Polvel, kpivot_Polvel )
  call CNDiffusionMatrixTemp( 1/Pr, delta_t, DifLUMT_Temp, kpivot_Temp )
  call CNDiffusionMatrixTormag( 1/Pm, delta_t, DifLUMT_Tormag, kpivot_Tormag )
  call CNDiffusionMatrixPolmag( 1/Pm, delta_t, DifLUMT_Polmag, kpivot_Polmag )


 !------------------- 時間積分開始の前処理 --------------------
  time = initial_time
  
  wt_Temp = wt_xyz(xyz_Temp)
  wt_Torvel = wt_xyz(xyz_Torvel) 
  wt_Polvel = wt_xyz(xyz_Polvel)
  wt_LaplaPolvel = wt_Lapla_wt(wt_Polvel)
  xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)
  wt_Tormag = wt_xyz(xyz_Tormag)
  wt_Polmag = wt_xyz(xyz_Polmag)

  if ( initial_file == "") then
     call wt_TorBoundariesGrid(wt_Torvel,cond=velBC)
     wt_Polvel = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaPolvel),cond=velBC))
     wt_LaplaPolvel = wt_Lapla_wt(wt_Polvel)
     call wt_TormagBoundariesGrid(wt_Tormag)
     call wt_PolmagBoundariesGrid(wt_Polmag)
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)

     xyz_Temp = xyz_wt(wt_Temp)
     xyz_Torvel = xyz_wt(wt_Torvel) 
     xyz_Polvel = xyz_wt(wt_Polvel)
     xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)
     xyz_Tormag = xyz_wt(wt_Tormag)
     xyz_Polmag = xyz_wt(wt_Polmag)
  endif

  call wt_Potential2Vector(xyz_Vlon,xyz_Vlat,xyz_Vrad,wt_Torvel,wt_Polvel)
  call wt_Potential2Vector &
       (xyz_ZLon,xyz_ZLat,xyz_ZRad,-wt_LaplaPolvel,wt_Torvel)

  call wt_Potential2Vector(xyz_Blon,xyz_Blat,xyz_Brad,wt_Tormag,wt_Polmag)
  call wt_Potential2Rotation(xyz_JLon,xyz_JLat,xyz_JRad,wt_Tormag,wt_Polmag)

  if ( initial_file == "") then
     call TimeDerivetives_noDiffusion( &
          wt_Torvel, wt_Polvel, wt_LaplaPolvel, wt_Temp, &
          xyz_Vlon, xyz_Vlat, xyz_Vrad, &
          xyz_Zlon, xyz_Zlat, xyz_Zrad, &
          xyz_Blon, xyz_Blat, xyz_Brad, &
          xyz_Jlon, xyz_Jlat, xyz_Jrad, &
          xyz_Temp, &
          wt_DtDTorvel1, wt_DtDLaplaPolvel1, &
          wt_DtDTormag1, wt_DtDPolmag1, wt_DtDTemp1 )
  endif

  call output_gtool4_init
  if ( initial_file == '' ) call output_gtool4    ! 内部で与えた初期値は出力

 !------------------- 時間積分(Adams-Bashforth 法) --------------------
  do it=1,nstep
     time = initial_time + it * delta_t

     ! 昔の時間変化項を保存
     wt_DtDTorvel0 = wt_DtDTorvel1
     wt_DtDLaplaPolvel0 = wt_DtDLaplaPolvel1
     wt_DtDTormag0 = wt_DtDTormag1
     wt_DtDPolmag0 = wt_DtDPolmag1
     wt_DtDTemp0   = wt_DtDTemp1

     call TimeDerivetives_noDiffusion( &
          wt_Torvel, wt_Polvel, wt_LaplaPolvel, wt_Temp, &
          xyz_Vlon, xyz_Vlat, xyz_Vrad, &
          xyz_Zlon, xyz_Zlat, xyz_Zrad, &
          xyz_Blon, xyz_Blat, xyz_Brad, &
          xyz_Jlon, xyz_Jlat, xyz_Jrad, &
          xyz_Temp, &
          wt_DtDTorvel1, wt_DtDLaplaPolvel1, &
          wt_DtDTormag1, wt_DtDPolmag1, wt_DtDTemp1 )

     ! トロイダル速度場
     wt_Torvel = wt_Torvel &
          + delta_t *( 3/2.0*wt_DtDTorvel1 - 1/2.0*wt_DtDTorvel0 ) &
          + delta_t/2.0 * wt_Lapla_wt(wt_Torvel) 
     call wt_TorBoundariesGrid(wt_Torvel,cond=velBC)
     wt_Torvel = LUSolve(DifLUMT_Torvel,kpivot_Torvel,wt_Torvel)

     ! ポロイダル速度場
     wt_LaplaPolvel = wt_LaplaPolvel &
           + delta_t *(3/2.0*wt_DtDLaplaPolvel1 - 1/2.0*wt_DtDLaplaPolvel0 ) &
           + delta_t/2.0 * wt_Lapla_wt(wt_LaplaPolvel) 
     wt_Polvel = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaPolvel),cond=velBC))
     wt_Polvel = LUSolve(DifLUMT_Polvel,kpivot_Polvel,wt_Polvel)
     wt_LaplaPolvel = wt_Lapla_wt(wt_Polvel)

     ! トロイダル磁場
     wt_Tormag = wt_Tormag &
          + delta_t *( 3/2.0*wt_DtDTormag1 - 1/2.0*wt_DtDTormag0 ) &
          + delta_t/2.0 * (1/Pm) * wt_Lapla_wt(wt_Tormag) 
     call wt_TormagBoundariesGrid(wt_Tormag)
     wt_Tormag = LUSolve(DifLUMT_Tormag,kpivot_Tormag,wt_Tormag)

     ! ポロイダル磁場
     wt_Polmag = wt_Polmag &
           + delta_t *(3/2.0*wt_DtDPolmag1 - 1/2.0*wt_DtDPolmag0 ) &
           + delta_t/2.0* (1/Pm) * wt_Lapla_wt(wt_Polmag) 
     call wt_PolmagBoundariesGrid(wt_Polmag)
     wt_Polmag = LUSolve(DifLUMT_Polmag,kpivot_Polmag,wt_Polmag)

     ! 温度場
     wt_Temp = wt_Temp &
          + delta_t *( 3/2.0*wt_DtDTemp1 - 1/2.0*wt_DtDTemp0 ) &
          + delta_t/2.0 * (1/Pr) * wt_Lapla_wt(wt_Temp) 
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)
     wt_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,wt_Temp)

     ! グリッド値計算
     xyz_Torvel = xyz_wt(wt_Torvel)
     xyz_Polvel = xyz_wt(wt_Polvel)
     xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)
     xyz_Tormag = xyz_wt(wt_Tormag)
     xyz_Polmag = xyz_wt(wt_Polmag)
     xyz_Temp = xyz_wt(wt_Temp)

     call wt_Potential2Vector(xyz_Vlon,xyz_Vlat,xyz_Vrad,wt_Torvel,wt_Polvel)
     call wt_Potential2Vector &
          (xyz_ZLon,xyz_ZLat,xyz_ZRad,-wt_LaplaPolvel,wt_Torvel)

     call wt_Potential2Vector(xyz_Blon,xyz_Blat,xyz_Brad,wt_Tormag,wt_Polmag)
     call wt_Potential2Rotation(xyz_JLon,xyz_JLat,xyz_JRad,wt_Tormag,wt_Polmag)

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

! 以上 メインプログラム 
!-----------------------------------------------------------------------------
! 以下 サブルーチン
 !------------------- 時間変化項(拡散項以外) ----------------------
  subroutine TimeDerivetives_noDiffusion( &
          wt_Torvel, wt_Polvel, wt_LaplaPolvel, wt_Temp, &
          xyz_Vlon, xyz_Vlat, xyz_Vrad, &
          xyz_Zlon, xyz_Zlat, xyz_Zrad, &
          xyz_Blon, xyz_Blat, xyz_Brad, &
          xyz_Jlon, xyz_Jlat, xyz_Jrad, &
          xyz_Temp, &
          wt_DtDTorvel, wt_DtDLaplaPolvel, &
          wt_DtDTormag, wt_DtDPolmag, wt_DtDTemp )
    !
    ! 時間変化項(拡散項以外)の計算を行う. 
    !
    real(8), intent(IN) :: wt_Torvel(:,:)        ! トロイダル速度
    real(8), intent(IN) :: wt_Polvel(:,:)        ! ポロイダル速度
    real(8), intent(IN) :: wt_LaplaPolvel(:,:)   ! ▽^2 ポロイダル速度
    real(8), intent(IN) :: wt_Temp(:,:)          ! 温度

    real(8), intent(IN)     :: xyz_Vlon(:,:,:)   ! 速度(経度)
    real(8), intent(IN)     :: xyz_Vlat(:,:,:)   ! 速度(緯度)
    real(8), intent(IN)     :: xyz_Vrad(:,:,:)   ! 速度(動径)

    real(8), intent(IN)     :: xyz_Zlon(:,:,:)   ! 渦度(経度)
    real(8), intent(IN)     :: xyz_Zlat(:,:,:)   ! 渦度(緯度)
    real(8), intent(IN)     :: xyz_Zrad(:,:,:)   ! 渦度(動径)

    real(8), intent(IN)     :: xyz_Blon(:,:,:)   ! 磁場(経度)
    real(8), intent(IN)     :: xyz_Blat(:,:,:)   ! 磁場(緯度)
    real(8), intent(IN)     :: xyz_Brad(:,:,:)   ! 磁場(動径)

    real(8), intent(IN)     :: xyz_Jlon(:,:,:)   ! 電流(経度)
    real(8), intent(IN)     :: xyz_Jlat(:,:,:)   ! 電流(緯度)
    real(8), intent(IN)     :: xyz_Jrad(:,:,:)   ! 電流(動径)

    real(8), intent(IN)     :: xyz_Temp(:,:,:)   ! 温度

    real(8), intent(OUT) :: wt_DtDTorvel(:,:)   ! トロイダル速度時間変化項
    real(8), intent(OUT) :: wt_DtDLaplaPolvel(:,:)
                                                ! ▽^2 ポロイダル速度時間変化項
    real(8), intent(OUT) :: wt_DtDTormag(:,:)   ! トロイダル磁場時間変化項
    real(8), intent(OUT) :: wt_DtDPolmag(:,:)   ! ポロイダル磁場時間変化項
    real(8), intent(OUT) :: wt_DtDTemp(:,:)     ! 温度時間変化項

    !--- 作業変数 ---
    real(8), dimension(size(xyz_Vlon,1),size(xyz_Vlon,2),size(xyz_Vlon,3)) &
         :: xyz_ZxVLon   ! (▽xV)xV (経度)
    real(8), dimension(size(xyz_Vlat,1),size(xyz_Vlat,2),size(xyz_Vlat,3)) &
         :: xyz_ZxVLat   ! (▽xV)xV(緯度)
    real(8), dimension(size(xyz_Vrad,1),size(xyz_Vrad,2),size(xyz_Vrad,3)) &
         :: xyz_ZxVRad   ! (▽xV)xV(動径)

    real(8), dimension(size(xyz_Blon,1),size(xyz_Blon,2),size(xyz_Blon,3)) &
         :: xyz_JxBLon   ! (▽xB)xB (経度)
    real(8), dimension(size(xyz_Blat,1),size(xyz_Blat,2),size(xyz_Blat,3)) &
         :: xyz_JxBLat   ! (▽xB)xB(緯度)
    real(8), dimension(size(xyz_Brad,1),size(xyz_Brad,2),size(xyz_Brad,3)) &
         :: xyz_JxBRad   ! (▽xB)xB(動径)

    ! (▽xv)xv 非線形項計算
    xyz_ZxVLon = xyz_ZLat * xyz_VRad - xyz_ZRad * xyz_VLat 
    xyz_ZxVLat = xyz_ZRad * xyz_VLon - xyz_ZLon * xyz_VRad 
    xyz_ZxVRad = xyz_ZLon * xyz_VLat - xyz_ZLat * xyz_VLon 

   ! (▽xB)xv 非線形項計算
    xyz_JxBLon = xyz_JLat * xyz_BRad - xyz_JRad * xyz_BLat 
    xyz_JxBLat = xyz_JRad * xyz_BLon - xyz_JLon * xyz_BRad 
    xyz_JxBRad = xyz_JLon * xyz_BLat - xyz_JLat * xyz_BLon 

  ! トロイダル時間変化
    wt_DtDTorvel = &
           wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_Torvel)   &
                       - 2/Ekman * wt_Qoperator_wt(wt_Polvel) ) &
         - wt_L2Inv_wt(wt_RadRot_xyz_xyz(      &
                       xyz_ZxVLon - xyz_JxBLon/(Ekman*Pm), &
                       xyz_ZxVLat - xyz_JxBLat/(Ekman*Pm)))

  ! ポロイダル時間変化
    wt_DtDLaplaPolvel = &
         - Ra/(Ekman*RO)*wt_Temp                    &
         + wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_LaplaPolvel) &
                       + 2/Ekman * wt_Qoperator_wt(wt_Torvel) )  &
         + wt_L2Inv_wt(wt_RadRotRot_xyz_xyz_xyz(          &
               xyz_ZxVLon - xyz_JxBLon/(Ekman*Pm),&
               xyz_ZxVLat - xyz_JxBLat/(Ekman*Pm),&
               xyz_ZxVRad - xyz_JxBRad/(Ekman*Pm) ))

  ! 温度時間変化
    wt_DtDTemp = &
         - wt_Div_xyz_xyz_xyz(  &
               xyz_Vlon*xyz_Temp, xyz_Vlat*xyz_Temp, xyz_Vrad*xyz_Temp)

  ! トロイダル磁場
    wt_DtDTormag = &
           wt_L2Inv_wt(wt_RadRotRot_xyz_xyz_xyz(         &
               xyz_Vlat * xyz_Brad - xyz_Vrad * xyz_Blat,&
               xyz_Vrad * xyz_Blon - xyz_Vlon * xyz_Brad,&
               xyz_Vlon * xyz_Blat - xyz_Vlat * xyz_Blon))

  ! ポロイダル磁場
    wt_DtDPolmag = &
           wt_L2Inv_wt( wt_RadRot_xyz_xyz(               &
               xyz_Vlat * xyz_Brad - xyz_Vrad * xyz_Blat,&
               xyz_Vrad * xyz_Blon - xyz_Vlon * xyz_Brad))

  end subroutine TimeDerivetives_noDiffusion

 !------------------- 拡散項(トロイダル速度場) ----------------------
  subroutine CNDiffusionMatrixTorvel( Diffc, dt, DiffLUMatrix, kpivot )
    !
    ! Crank Nicolson 用拡散項陰的行列設定(トロイダル速度場)
    !
    real(8), intent(IN)          :: Diffc    ! 拡散係数
    real(8), intent(IN)          :: dt       ! 時間刻

    real(8), intent(OUT)   :: DiffLUMatrix(:,0:,0:)
                                ! Crank Nicholson 用行列(1-D dt/2▽^2, LU 分解)
    integer, intent(OUT)   :: kpivot(:,0:)        ! ピボット
                                ! Crank Nicholson 行列ピボット情報

    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_I
    ! 作業用変数
    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_DI
    ! 作業用変数

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! 各波数成分独立
       wt_DI = - Diffc * dt/2.0 * wt_Lapla_wt(wt_I) 
       call wt_TorBoundariesGrid(wt_DI,cond=velBC)
       DiffLUMatrix(:,:,l) = wt_I + wt_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrixTorvel

 !------------------- 拡散項(ポロイダル速度場) ----------------------
  subroutine CNDiffusionMatrixPolvel( Diffc, dt, DiffLUMatrix, kpivot )
    !
    ! Crank Nicolson 用拡散項陰的行列設定(ポロイダル速度場)
    !
    real(8), intent(IN)          :: Diffc    ! 拡散係数
    real(8), intent(IN)          :: dt       ! 時間刻

    real(8), intent(OUT)   :: DiffLUMatrix(:,0:,0:)
                                ! Crank Nicholson 用行列(1-D dt/2▽^2, LU 分解)
    integer, intent(OUT)   :: kpivot(:,0:)        ! ピボット
                                ! Crank Nicholson 行列ピボット情報

    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_I
    ! 作業用変数
    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_DI
    ! 作業用変数

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! 各波数成分独立
       wt_DI = - Diffc * dt/2.0 * wt_Lapla_wt(wt_Lapla_wt(wt_I))
       wt_DI = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_DI),cond=velBC))
       DiffLUMatrix(:,:,l) = wt_I + wt_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrixPolvel

 !------------------- 拡散項(温度場) ----------------------
  subroutine CNDiffusionMatrixTemp( Diffc, dt, DiffLUMatrix, kpivot )
    !
    ! Crank Nicolson 用拡散項陰的行列設定(温度場)
    !
    real(8), intent(IN)          :: Diffc    ! 拡散係数
    real(8), intent(IN)          :: dt       ! 時間刻

    real(8), intent(OUT)   :: DiffLUMatrix(:,0:,0:)
                                ! Crank Nicholson 用行列(1-D dt/2▽^2, LU 分解)
    integer, intent(OUT)   :: kpivot(:,0:)        ! ピボット
                                ! Crank Nicholson 行列ピボット情報

    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_I
    ! 作業用変数
    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_DI
    ! 作業用変数
    real(8), dimension(size(DiffLUMatrix,1),2) :: w_Bndry
    ! 作業用変数

    integer :: l

    DiffLUMatrix = 0.0
    w_Bndry=0.0D0                               ! 境界の時間変化は 0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! 各波数成分独立
       wt_DI = - Diffc * dt/2.0 * wt_Lapla_wt(wt_I) 
       call wt_BoundariesGrid(wt_DI,w_Bndry,cond=TempBC)
       DiffLUMatrix(:,:,l) = wt_I + wt_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrixTemp

 !------------------- 拡散項(トロイダル磁場) ----------------------
  subroutine CNDiffusionMatrixTormag( Diffc, dt, DiffLUMatrix, kpivot )
    !
    ! Crank Nicolson 用拡散項陰的行列設定(トロイダル磁場)
    !
    real(8), intent(IN)          :: Diffc    ! 拡散係数
    real(8), intent(IN)          :: dt       ! 時間刻

    real(8), intent(OUT)   :: DiffLUMatrix(:,0:,0:)
                                ! Crank Nicholson 用行列(1-D dt/2▽^2, LU 分解)
    integer, intent(OUT)   :: kpivot(:,0:)        ! ピボット
                                ! Crank Nicholson 行列ピボット情報

    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_I
    ! 作業用変数
    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_DI
    ! 作業用変数

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! 各波数成分独立
       wt_DI = - Diffc * dt/2.0 * wt_Lapla_wt(wt_I) 
       call wt_TormagBoundariesGrid(wt_DI)
       DiffLUMatrix(:,:,l) = wt_I + wt_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrixTormag

 !------------------- 拡散項(ポロイダル磁場) ----------------------
  subroutine CNDiffusionMatrixPolmag( Diffc, dt, DiffLUMatrix, kpivot )
    !
    ! Crank Nicolson 用拡散項陰的行列設定(ポロイダル磁場)
    !
    real(8), intent(IN)          :: Diffc    ! 拡散係数
    real(8), intent(IN)          :: dt       ! 時間刻


    real(8), intent(OUT)   :: DiffLUMatrix(:,0:,0:)
                                ! Crank Nicholson 用行列(1-D dt/2▽^2, LU 分解)
    integer, intent(OUT)   :: kpivot(:,0:)        ! ピボット
                                ! Crank Nicholson 行列ピボット情報

    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_I
    ! 作業用変数
    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_DI
    ! 作業用変数

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! 各波数成分独立
       wt_DI = - Diffc * dt/2.0 * wt_Lapla_wt(wt_I) 
       call wt_PolmagBoundariesGrid(wt_DI)
       DiffLUMatrix(:,:,l) = wt_I + wt_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrixPolmag

 !------------------- 初期値設定(ファイルなし) ----------------------
  subroutine set_initial_values         ! xyz_* を設定する
    !
    ! 初期値設定ルーチン
    !
    !     初期値ファイルが指定されなかった場合に内部で初期値設定する.
    !     xyz_Torvel,xyz_Polvel,xyz_Tormag,xyz_Polmag,xyz_Temp を設定する
    !
    integer :: case=1                       ! 初期値のタイプ(0/1)
    namelist /inittype/ case                ! 初期値のタイプ設定

    real(8), dimension(size(xyz_Vlon,1),size(xyz_Vlon,2),size(xyz_Vlon,3)) &
         :: xyz_X         ! x=2r-r_i-r_o
    real(8), dimension(size(xyz_Vlon,1),size(xyz_Vlon,2),size(xyz_Vlon,3)) &
         :: xyz_theta     ! 余緯度

    real(8), parameter  :: A=0.1D0                     ! 初期温度擾乱振幅
    real(8), parameter  :: pi=3.1415926535897932385D0

    if(verbose) write(6,nml=inittype)
    open(10,file=nmlfile,status='OLD')
    read(10,nml=inittype); write(6,nml=inittype) ; close(10)

    xyz_theta = pi/2.0D0 - xyz_Lat
    xyz_X = 2*xyz_Rad - Ri - Ro

    ! 温度場
    xyz_Temp =  Ro*Ri/xyz_Rad - Ri &
       + 210*A/sqrt(17920*pi) * (1 - 3*xyz_X**2 + 3*xyz_X**4 -xyz_X**6) &
           * sin(pi/2-xyz_Lat)**4 * cos(4*xyz_Lon)

    ! 速度場 ; 静止状態
    xyz_Torvel =  0 ; xyz_Polvel =  0

    ! 磁場 
    if ( case == 0 )then
       xyz_Tormag = 0.0D0 ;  xyz_Polmag = 0.0D0    ! 磁場なし

       call MessageNotify('M','set_initial_values',&
            'Selected initial data type is  "CASE0". ')

    elseif ( case == 1 )then
       xyz_BLon = 5.0D0 * sin(pi*(xyz_Rad-ri)) * sin(2.0D0*xyz_theta)
       xyz_BLat = 5.0D0/8.0D0 &
            * ( 9.0D0*xyz_Rad - 8.0D0*ro - ri**4.0D0/xyz_Rad**3.0D0) &
            * sin(xyz_theta)
       xyz_BRad = 5.0D0/8.0D0 * ( 8.0D0*ro - 6.0D0*xyz_Rad &
            - 2.0D0*ri**4.0D0/xyz_Rad**3.0D0 )* cos(xyz_theta)

       ! 磁場からポテンシャルへの変換
       xyz_Tormag = xyz_wt(wt_L2Inv_wt(wt_RadRot_xyz_xyz(xyz_BLon, xyz_BLat)))
       xyz_Polmag = xyz_wt(wt_L2Inv_wt(wt_xyz(xyz_Rad * xyz_BRad)))

       call MessageNotify('M','set_initial_values',&
            'Selected initial data type is  "CASE1". ')
    else
       call MessageNotify('E','set_initial_values',&
            'The parameter "case" should be 0 or 1.')
    endif

  end subroutine set_initial_values

 !------------------- 初期値設定(ファイルから) ----------------------  
  subroutine initial_read_gtool4
    !
    ! 初期値設定ルーチン
    !
    !     初期値ファイルから変数
    !     xyz_Torvel,xyz_Polvel,xyz_Tormag,xyz_Polmag,xyz_Temp,
    !     wt_DtDTorvel1, wt_DtDLaplaPolvel1, 
    !     wt_DtDTormag1, wt_DtDPolmag1, wt_DtDTemp1 を読み込む

    call HistoryGet( trim(initial_file), 'torvel', xyz_Torvel, initial_time )
    call HistoryGet( trim(initial_file), 'polvel', xyz_Polvel, initial_time )
    call HistoryGet( trim(initial_file), 'tormag', xyz_Tormag, initial_time )
    call HistoryGet( trim(initial_file), 'polmag', xyz_Polmag, initial_time )
    call HistoryGet( trim(initial_file), 'temp',   xyz_Temp,   initial_time )

    call HistoryGet( trim(initial_file), 'DtDTorvel', wt_DtDTorvel1, initial_time )
    call HistoryGet( trim(initial_file), 'DtDLaplaPolvel', wt_DtDLaplaPolvel1, initial_time )
      
    call HistoryGet( trim(initial_file), 'DtDTormag', wt_DtDTormag1, initial_time )
      
    call HistoryGet( trim(initial_file), 'DtDPolmag', wt_DtDPolmag1, initial_time )

    call HistoryGet( trim(initial_file), 'DtDTemp', wt_DtDTemp1, initial_time )
      
  end subroutine initial_read_gtool4

 !------------------- 出力 ----------------------
  subroutine output_gtool4_init
    !
    ! ヒストリー出力初期化ルーチン
    !
    !   ファイル作成
    !   変数定義
    !

    !---- ヒストリーファイル作成 ----
    call HistoryCreate( &
           file=trim(output_file), &
           title=trim(title), source=trim(source), &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','nm ','n  ','m  ','l  ','t  '/), &
           dimsizes=(/im,jm,km+1,(nm+1)**2,nm+1,2*nm+1,lm+1,0/),&
           longnames=(/'Longitude             ','Latitude              ',&
                       'Radius                ', &
                       'Hor.wave number index ','Hor.total wave number ',&
                       'zonal wave number     ','Radial chebyshev order',&
                       'time                  '/),&
           units=(/'1','1','1','1','1','1','1','1'/),   &
           origin=real(time), interval=real(ndisp*delta_t))

   !---- 座標変数定義, 出力 ----
    call HistoryPut('lon',x_Lon/pi*180)                       ! 変数出力
    call HistoryAddattr('lon','topology','circular')          ! 周期属性
    call HistoryAddattr('lon','modulo',360.0)                 ! 周期属性
    call HistoryPut('lat',y_Lat/pi*180)                       ! 変数出力
    call HistoryPut('rad',z_Rad)                              ! 変数出力
    call HistoryPut('nm',(/(dble(n),n=0,(nm+1)**2)/))         ! 変数出力
    call HistoryPut('n',(/(dble(n),n=0,nm)/))                 ! 変数出力
    call HistoryPut('m',(/(dble(m),m=-nm,nm)/))               ! 変数出力
    call HistoryPut('l',(/(dble(l),l=0,lm)/))                 ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
           varname='lon_weight', dims=(/'lon'/), & 
           longname='weight function in longitude', &
           units='1', xtype='double')
    call HistoryPut('lon_weight',x_Lon_weight)                ! 変数出力
    call HistoryAddVariable( &                                ! 変数定義
           varname='lat_weight', dims=(/'lat'/), & 
           longname='weight function in latitude', &
           units='1', xtype='double')
    call HistoryPut('lat_weight',y_Lat_weight)                ! 変数出力
    call HistoryAddVariable( &                                ! 変数定義
           varname='rad_weight', dims=(/'rad'/), & 
           longname='weight function in radial', &
           units='1', xtype='double')
    call HistoryPut('rad_weight',z_Rad_weight)                ! 変数出力

   !---- 物理変数定義 ----
    !-- 速度ポテンシャル --
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

    !-- 磁場ポテンシャル --
    call HistoryAddVariable( &                                ! 変数定義
           varname='tormag', dims=(/'lon','lat','rad','t  '/), & 
           longname='toroidal magnetic field potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='polmag', dims=(/'lon','lat','rad','t  '/), & 
           longname='poloidal magnetic field potential', units='1', xtype='double')
    !-- 温度 --
    call HistoryAddVariable( &                                ! 変数定義
           varname='temp', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature', units='1', xtype='double')
    !-- 速度 --
    call HistoryAddVariable( &                                ! 変数定義
           varname='vlon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vlat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vrad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-velocity', units='1', xtype='double')
    !-- 渦度 --
    call HistoryAddVariable( &                                ! 変数定義
           varname='zlon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-vorticity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='zlat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-vorticity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='zrad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-voticity', units='1', xtype='double')
    !-- 磁場 --
    call HistoryAddVariable( &                                ! 変数定義
           varname='blon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-mag.field', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='blat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-mag.field', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='brad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-mag.field', units='1', xtype='double')
    !-- 電流 --
    call HistoryAddVariable( &                                ! 変数定義
           varname='jlon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-current', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='jlat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-current', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='jrad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-current', units='1', xtype='double')

   !---- 時間変化項定義 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='DtDTorvel', dims=(/'nm ','l  ','t  '/), & 
           longname='time variation of velocity toroidal potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='DtDLaplaPolvel', dims=(/'nm ','l  ','t  '/), & 
           longname='time variation of laplacian of velocity poloidal potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='DtDTemp', dims=(/'nm ','l  ','t  '/), & 
           longname='time variation of temperature', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='DtDTormag', dims=(/'nm ','l  ','t  '/), & 
           longname='time variation of magnetic field toroidal potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='DtDPolmag', dims=(/'nm ','l  ','t  '/), & 
           longname='time variation of magnetic field poloidal potential', &
           units='1', xtype='double')

   !---- その他診断量定義 ----
    !-- 全エネルギー --
    call HistoryAddVariable( &                                ! 変数定義
           varname='ek', dims=(/'t  '/), & 
           longname='mean kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='em', dims=(/'t  '/), & 
           longname='mean magnetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='ektot', dims=(/'t  '/), & 
           longname='total kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='emtot', dims=(/'t  '/), & 
           longname='total magnetic energy', units='1', xtype='double')


    !-- 平均帯状流 --
    call HistoryAddVariable( &                                ! 変数定義
           varname='um', dims=(/'lat','rad','t  '/), & 
           longname='zonal flow', units='1', xtype='double')
    !-- ヘリシティー --
    call HistoryAddVariable( &                                ! 変数定義
           varname='hel', dims=(/'lon','lat','rad','t  '/), & 
           longname='helicity', units='1', xtype='double')

    !-- 運動エネルギースペクトル診断量定義 --
    call HistoryAddVariable( &                                ! 変数定義
           varname='ektor_nmz', dims=(/'n  ','m  ','rad','t  '/), & 
           longname='Toroidal kinetic energy spectrum', units='1', xtype='double')
    call HistoryAddAttr('ektor_nmz','missing_value', vmiss  )

    call HistoryAddVariable( &                                ! 変数定義
           varname='ekpol_nmz', dims=(/'n  ','m  ','rad','t  '/), & 
           longname='Poloidal kinetic energy spectrum', units='1', xtype='double')
    call HistoryAddAttr('ekpol_nmz','missing_value', vmiss  )

    call HistoryAddVariable( &                                ! 変数定義
           varname='ektor_nz', dims=(/'n  ','rad','t  '/), & 
           longname='Toroidal kinetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='ekpol_nz', dims=(/'n  ','rad','t  '/), & 
           longname='Poloidal kinetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='ektor_n', dims=(/'n  ','t  '/), & 
           longname='Toroidal kinetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='ekpol_n', dims=(/'n  ','t  '/), & 
           longname='Poloidal kinetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='ektor', dims=(/'t  '/), & 
           longname='Toroidal kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='ekpol', dims=(/'t  '/), & 
           longname='Poloidal kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='ekpot', dims=(/'t'/), & 
           longname='Kinetic energy (by potential)', units='1', xtype='double')

    !-- 磁場エネルギースペクトル診断量定義 --
    call HistoryAddVariable( &                                ! 変数定義
           varname='emtor_nmz', dims=(/'n  ','m  ','rad','t  '/), & 
           longname='Toroidal magnetic energy spectrum', units='1', xtype='double')
    call HistoryAddAttr('emtor_nmz','missing_value', vmiss  )

    call HistoryAddVariable( &                                ! 変数定義
           varname='empol_nmz', dims=(/'n  ','m  ','rad','t  '/), & 
           longname='Poloidal magnetic energy spectrum', units='1', xtype='double')
    call HistoryAddAttr('empol_nmz','missing_value', vmiss  )

    call HistoryAddVariable( &                                ! 変数定義
           varname='emtor_nz', dims=(/'n  ','rad','t  '/), & 
           longname='Toroidal magnetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='empol_nz', dims=(/'n  ','rad','t  '/), & 
           longname='Poloidal magnetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='emtor_n', dims=(/'n  ','t  '/), & 
           longname='Toroidal magnetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='empol_n', dims=(/'n  ','t  '/), & 
           longname='Poloidal magnetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='emtor', dims=(/'t  '/), & 
           longname='Toroidal magnetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='empol', dims=(/'t  '/), & 
           longname='Poloidal magnetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='empot', dims=(/'t'/), & 
           longname='Magnetic energy (by potential)', units='1', xtype='double')
   !---- エンストロフィースペクトル診断量定義 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='enstor_nmz', dims=(/'n  ','m  ','rad','t  '/), & 
           longname='Toroidal enstrophy spectrum', units='1', xtype='double')
    call HistoryAddAttr('enstor_nmz','missing_value', vmiss  )

    call HistoryAddVariable( &                                ! 変数定義
           varname='enspol_nmz', dims=(/'n  ','m  ','rad','t  '/), & 
           longname='Poloidal enstrophy spectrum', units='1', xtype='double')
    call HistoryAddAttr('enspol_nmz','missing_value', vmiss  )

    call HistoryAddVariable( &                                ! 変数定義
           varname='enstor_nz', dims=(/'n  ','rad','t  '/), & 
           longname='Toroidal enstrophy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='enspol_nz', dims=(/'n  ','rad','t  '/), & 
           longname='Poloidal enstrophy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='enstor_n', dims=(/'n  ','t  '/), & 
           longname='Toroidal enstrophy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='enspol_n', dims=(/'n  ','t  '/), & 
           longname='Poloidal enstrophy spectrum', units='1', xtype='double')

    call HistoryAddVariable( &                                ! 変数定義
           varname='enstor', dims=(/'t  '/), & 
           longname='Toroidal enstrophy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='enspol', dims=(/'t  '/), & 
           longname='Poloidal enstrophy', units='1', xtype='double')

    call HistoryAddVariable( &                                ! 変数定義
           varname='enspot', dims=(/'t'/), & 
           longname='enstrophy (by potential)', units='1', xtype='double')


   !---- 実験パラメターを属性として定義, 出力(全て Global 属性) ----
    call HistoryAddAttr('lon','+Radius', (/ri,ro,eta/) )
    call HistoryAddAttr('lon','+delta_t', delta_t  )

    call HistoryAddAttr('lon', '+Rayleigh',   Ra    )
    call HistoryAddAttr('lon', '+Prandtl',    Pr    )
    call HistoryAddAttr('lon', '+Ekman',      Ekman )
    call HistoryAddAttr('lon', '+MagPrandtl', Pm    )

    call HistoryAddAttr('lon', '+VelBoundary',  VelBC  )
    call HistoryAddAttr('lon', '+TempBoundary', TempBC )

   end subroutine output_gtool4_init

  subroutine output_gtool4
    !
    ! ヒストリー出力
    !
    !   各物理量の gtool4 ファイルへの出力を行う
    !
    write(6,*) 'it = ',it, '  time = ', time
    call HistoryPut('t',real(time))

   !---- 物理変数出力 ----
    call HistoryPut('torvel',xyz_Torvel)
    call HistoryPut('polvel',xyz_Polvel)
    call HistoryPut('tormag',xyz_Tormag)
    call HistoryPut('polmag',xyz_Polmag)
    call HistoryPut('laplapolvel',xyz_LaplaPolvel)
    call HistoryPut('temp',xyz_Temp)

    call HistoryPut('vlon',xyz_Vlon)
    call HistoryPut('vlat',xyz_Vlat)
    call HistoryPut('vrad',xyz_Vrad)

    call HistoryPut('zlon',xyz_Zlon)
    call HistoryPut('zlat',xyz_Zlat)
    call HistoryPut('zrad',xyz_Zrad)

    call HistoryPut('blon',xyz_Blon)
    call HistoryPut('blat',xyz_Blat)
    call HistoryPut('brad',xyz_Brad)

    call HistoryPut('jlon',xyz_Jlon)
    call HistoryPut('jlat',xyz_Jlat)
    call HistoryPut('jrad',xyz_Jrad)

   !---- 時間変化項出力 ----
    call HistoryPut('DtDTorvel',      wt_DtDTorvel1)
    call HistoryPut('DtDLaplaPolvel', wt_DtDLaplaPolvel1)
    call HistoryPut('DtDTemp',        wt_DtDTemp1)
    call HistoryPut('DtDTormag',      wt_DtDTormag1)
    call HistoryPut('DtDPolmag',      wt_DtDPolmag1)

   !---- 診断量出力 ----
    !-- 平均エネルギー --
    call HistoryPut('ek',AvrLonLatRad_xyz((xyz_Vlon**2+xyz_Vlat**2+xyz_Vrad**2)/2))
    call HistoryPut('em',AvrLonLatRad_xyz((xyz_Blon**2+xyz_Blat**2+xyz_Brad**2)/(2*Ekman*Pm)))

    !-- 全エネルギー --
    call HistoryPut('ektot',IntLonLatRad_xyz((xyz_Vlon**2+xyz_Vlat**2+xyz_Vrad**2)/2))
    call HistoryPut('emtot',IntLonLatRad_xyz((xyz_Blon**2+xyz_Blat**2+xyz_Brad**2)/(2*Ekman*Pm)))
    !-- 帯状流 --
    call HistoryPut('um',yz_avrLon_xyz(xyz_Vlon))
    call HistoryPut('hel',xyz_Vlon*xyz_Zlon + xyz_Vlat*xyz_Zlat + xyz_Vrad*xyz_Zrad)

   !---- 運動エネルギースペクトル診断量出力 ----
    nz_EkTor = nz_ToroidalEnergySpectrum_wt(wt_Torvel)
    nz_EkPol = nz_PoloidalEnergySpectrum_wt(wt_Polvel)

    call HistoryPut('ektor_nmz',nmz_ToroidalEnergySpectrum_wt(wt_Torvel))
    call HistoryPut('ekpol_nmz',nmz_PoloidalEnergySpectrum_wt(wt_Polvel))
    call HistoryPut('ektor_nz',nz_EkTor)
    call HistoryPut('ekpol_nz',nz_EkPol)
    call HistoryPut('ektor_n',a_Int_ag(nz_EkTor/(spread(z_Rad,1,nm+1)**2)))
    call HistoryPut('ekpol_n',a_Int_ag(nz_EkPol/(spread(z_Rad,1,nm+1)**2)))
    call HistoryPut('ektor',IntRad_z(sum(nz_EkTor,1)/z_Rad**2))
    call HistoryPut('ekpol',IntRad_z(sum(nz_EkPol,1)/z_Rad**2))
    call HistoryPut('ekpot',IntRad_z(sum(nz_EkTor,1)/z_Rad**2)+IntRad_z(sum(nz_EkPol,1)/z_Rad**2))

   !---- 磁場エネルギースペクトル診断量出力 ----
    nz_EmTor = nz_ToroidalEnergySpectrum_wt(wt_Tormag)/(Ekman*Pm)
    nz_EmPol = nz_PoloidalEnergySpectrum_wt(wt_Polmag)/(Ekman*Pm)

    call HistoryPut('emtor_nmz',nmz_ToroidalEnergySpectrum_wt(wt_Tormag)/(Ekman*Pm))
    call HistoryPut('empol_nmz',nmz_PoloidalEnergySpectrum_wt(wt_Polmag)/(Ekman*Pm))
    call HistoryPut('emtor_nz',nz_EmTor)
    call HistoryPut('empol_nz',nz_EmPol)
    call HistoryPut('emtor_n',a_Int_ag(nz_EmTor/(spread(z_Rad,1,nm+1)**2)))
    call HistoryPut('empol_n',a_Int_ag(nz_EmPol/(spread(z_Rad,1,nm+1)**2)))
    call HistoryPut('emtor',IntRad_z(sum(nz_EmTor,1)/z_Rad**2))
    call HistoryPut('empol',IntRad_z(sum(nz_EmPol,1)/z_Rad**2))
    call HistoryPut('empot',IntRad_z(sum(nz_EmTor,1)/z_Rad**2)+IntRad_z(sum(nz_EmPol,1)/z_Rad**2))

   !---- エンストロフィースペクトル診断量出力 ----
    nz_EnsTor = nz_ToroidalEnergySpectrum_wt(-wt_LaplaPolvel)
    nz_EnsPol = nz_PoloidalEnergySpectrum_wt(wt_Torvel)

    call HistoryPut('enstor_nmz',nmz_ToroidalEnergySpectrum_wt(-wt_LaplaPolvel))
    call HistoryPut('enspol_nmz',nmz_PoloidalEnergySpectrum_wt(wt_Torvel))
    call HistoryPut('enstor_nz',nz_EnsTor)
    call HistoryPut('enspol_nz',nz_EnsPol)
    call HistoryPut('enstor_n',a_Int_ag(nz_EnsTor/(spread(z_Rad,1,nm+1)**2)))
    call HistoryPut('enspol_n',a_Int_ag(nz_EnsPol/(spread(z_Rad,1,nm+1)**2)))
    call HistoryPut('enstor',IntRad_z(sum(nz_EnsTor,1)/z_Rad**2))
    call HistoryPut('enspol',IntRad_z(sum(nz_EnsPol,1)/z_Rad**2))
    call HistoryPut('enspot',IntRad_z(sum(nz_EnsTor,1)/z_Rad**2)+IntRad_z(sum(nz_EnsPol,1)/z_Rad**2))

  end subroutine output_gtool4

  subroutine output_gtool4_close
    !
    ! ヒストリー出力終了処理
    !
    call HistoryClose

  end subroutine output_gtool4_close

end program MHD_bsnsq_rot_3d_shell_wt_abcn_bench
