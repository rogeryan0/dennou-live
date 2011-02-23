!----------------------------------------------------------------------
!  Copyright (C) 2005--2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
! Sample program for gt4f90io and ISPACK  2005/09/28 S.Takehiro
! 
! Solving a non-linear 2-D shallow water system on a sphere. 
!     
!     d\zeta/dt = -\Dinv{a(1-\mu^2)}\DP{[(f+\zeta)U)]}{\lamdba} 
!                 -\Dinv{a}\DP{[(f+\zeta)V)]}{\mu} - F_\zeta^{Diff},
!
!     dD/dt = \Dinv{a(1-\mu^2)}\DP{[(f+\zeta)V)]}{\lamdba} 
!             -\Dinv{a}\DP{[(f+\zeta)U)]}{\mu}
!             -\Dlapla[g(h+h_s)+E] - F_D^{Diff},
!
!     dh/dt = -\Dinv{a(1-\mu^2)}\DP{(hU)]}{\lamdba} 
!             -\Dinv{a}\DP{(hV)}{\mu} - F_h^{Diff},
!
!     \Dlapla\psi = \zeta, \Dlapla\Xi = D,
!
!     U = -\frac{(1-\mu^2)}{a}\DP{\psi}{\mu} + \Dinv{a}\DP{\Xi}{\lambda}
!     V = \Dinv{a}\DP{\psi}{\lambda} + \frac{(1-\mu^2)}{a}\DP{\Xi}{\mu}
!
! where hs is hight of topography, E is Kinetic energy, (U^2+V^2)/2(1-\mu^2), 
! F_*^{Diff} are hyper viscosity
!
! The time integration is performed by using 4th-order Runge-Kutta scheme
! except for the dissipation terms (Crank-Nicolson scheme). 
!
! The program is organized to perform "case2" of Willamson et al. (1992).
! 
! History: 2005/09/28 S. Takehiro, created
!          2005/10/10 S. Takehiro, longname of div fixed.
!          2005/10/31 S. Nishizawa, define work variables for subroutines as global variable
!          2005/11/02 S. Nishizawa, change arguments of subroutine
!          2008/08/10 S. Takehiro, index changed, im -> 0:im-1
!
program spshallow_zd_rn4cn_case2

!== モジュール引用宣言 ================================================

  use w_module 
  use gt4_history, only : HistoryCreate, HistoryPut, HistoryGet, &
                          HistoryAddVariable, HistoryAddAttr,    &
                          HistoryClose, GT_HISTORY
  use dc_trace,    only : SetDebug, BeginSub, EndSub, DbgMessage
  use dc_message,  only : MessageNotify
  implicit none

!== 宣言部 ============================================================

 !---- 変数(格子点データ)---
  real(8), allocatable :: xy_Hsfc(:,:)          ! 表面変位擾乱 (t)
  real(8), allocatable :: xy_Vor(:,:)           ! 渦度 (t)
  real(8), allocatable :: xy_Div(:,:)           ! 発散 (t)

  real(8), allocatable :: xy_Coli(:,:)          ! 惑星渦度
  real(8), allocatable :: xy_Htopo(:,:)         ! 表面地形

  real(8), allocatable :: xy_VelLonCosLat(:,:)  ! U = xy_VelLon*Cos(xy_Lat)
  real(8), allocatable :: xy_VelLatCosLat(:,:)  ! V = xy_VelLat*Cos(xy_Lat)

  real(8), allocatable :: xy_HsfcTrue(:,:)      ! 表面変位解析解
  real(8), allocatable :: xy_VelLonTrue(:,:)    ! 速度経度成分解析解
  real(8), allocatable :: xy_VelLatTrue(:,:)    ! 速度緯度成分解析解
  real(8), allocatable :: xy_VelLon(:,:)        ! 速度経度成分(出力用変数)
  real(8), allocatable :: xy_VelLat(:,:)        ! 速度緯度成分(出力用変数)

  real(8), allocatable :: xy_FluxVorLon(:,:)    ! (ζ+f)U (副プログラム作業用)
  real(8), allocatable :: xy_FluxVorLat(:,:)    ! (ζ+f)V (副プログラム作業用)


 !---- 変数(スペクトルデータ) ----
  real(8), allocatable :: w_Hsfc(:)             ! 表面変位擾乱 (t)
  real(8), allocatable :: w_Vor(:)              ! 渦度 (t)
  real(8), allocatable :: w_Div(:)              ! 発散 (t)

  real(8), allocatable :: w_HsfcTmp(:)          ! 表面変位擾乱 (Runge-Kutta 用)
  real(8), allocatable :: w_VorTmp(:)           ! 渦度 (Runge-Kutta 用)
  real(8), allocatable :: w_DivTmp(:)           ! 発散 (Runge-Kutta 用)

  real(8), allocatable :: w_StrFunc(:)          ! 流線関数
  real(8), allocatable :: w_VelPot(:)           ! 速度ポテンシャル

  real(8), allocatable :: w_Htopo(:)            ! 表面地形

  real(8), allocatable :: w_KEnergy(:)          ! 運動エネルギー

  real(8), allocatable :: w_DHsfcDtK1(:)        ! 変位時間変化(RK 第1段)
  real(8), allocatable :: w_DHsfcDtK2(:)        ! 変位時間変化(RK 第2段)
  real(8), allocatable :: w_DHsfcDtK3(:)        ! 変位時間変化(RK 第3段)
  real(8), allocatable :: w_DHsfcDtK4(:)        ! 変位時間変化(RK 第4段)

  real(8), allocatable :: w_DVorDtK1(:)         ! 渦度時間変化(RK 第1段)
  real(8), allocatable :: w_DVorDtK2(:)         ! 渦度時間変化(RK 第2段)
  real(8), allocatable :: w_DVorDtK3(:)         ! 渦度時間変化(RK 第3段)
  real(8), allocatable :: w_DVorDtK4(:)         ! 渦度時間変化(RK 第4段)

  real(8), allocatable :: w_DDivDtK1(:)         ! 発散時間変化(RK 第1段)
  real(8), allocatable :: w_DDivDtK2(:)         ! 発散時間変化(RK 第2段)
  real(8), allocatable :: w_DDivDtK3(:)         ! 発散時間変化(RK 第3段)
  real(8), allocatable :: w_DDivDtK4(:)         ! 発散時間変化(RK 第4段)

 !---- 固定パラメタ -----
  real(8), parameter  :: pi = 3.141592653589793D0 ! 円周率
  character(len=20)   :: DbgMessageFmt='*** DbgMESSAGE ***'
  real(8), parameter  :: vmiss = -999.0       ! 欠損値

 !---- 作業変数 ----
  real(8), allocatable :: w_HVisc(:)         ! 超粘性係数(運動方程式)
  real(8), allocatable :: w_HDiff(:)         ! 超拡散係数(質量保存式)

  integer :: it=0                            ! 時間ステップ
  real(8) :: time                            ! モデル内時間
  integer :: n, m                            ! 全波数, 帯状波数

  type(GT_HISTORY) :: hst_rst                ! リスタート GT_HISTORY 変数

 !---- NAMELIST 変数 ----
  ! NAMELIST 入力用テンポラリファイル名(名前変更禁止)
  character(len=30),parameter  :: nmlfile='spshallow-zd_rn4cn_case2.nml'

  ! (メッセージ出力制御)
  logical            :: Verbose=.false.          ! 出力メッセージレベル
  logical            :: DebugOn=.false.          ! デバッグ出力コントロール
  namelist /message/  Verbose, DebugOn           !

  ! (空間解像度)
  integer :: nm=21             ! 切断全波数
  integer :: im=64             ! 経度方向格子点数 ( > 3*nm + 1)
  integer :: jm=32             ! 緯度方向格子点数 ( > 3*nm/2 )
  namelist /gridset/ nm, im, jm 

  ! (物理パラメタ)
  real(8)            :: Radius   = 6.37122D6     ! 惑星半径
  real(8)            :: Omega    = 7.292D-5      ! 回転角速度
  real(8)            :: Alpha    = 0.0           ! 回転軸方向(極との角度,deg.)
  real(8)            :: Grav     = 9.80616D0     ! 重力加速度
  real(8)            :: Hbar     = 4.0D3         ! 平均水深
  integer            :: HVOrder  = 2             ! 超粘性の次数
                                                 ! (水平ラプラシアンの階数)
  real(8)            :: HVisc    = 0.5D16        ! 超粘性係数
  integer            :: HDOrder  = 2             ! 超拡散の次数
                                                 ! (水平ラプラシアンの階数)
  real(8)            :: HDiff    = 0.5D16        ! 超拡散係数
  namelist /physics/ Radius, Omega, Alpha, Grav, Hbar, &
                     HVOrder, HVisc, HDOrder, HDiff

  ! (初期値)
  character(len=100) :: initial_file=''          ! 初期値データファイル名
  real               :: initial_time=0.0         ! 初期時刻
  namelist /initial/ initial_file, initial_time  ! 

  ! (時間積分)
  real(8) :: delta_t=1.0e-7                      ! 時間積分刻み
  integer :: nstep=2000                          ! 時間積分ステップ数
  namelist /tint/    delta_t, nstep              ! 

  ! (ヒストリー出力)
  character(len=100) :: hst_file=   ''           ! ヒストリーファイル名
  character(len=100) :: title = &                ! タイトル
       '2-dim shallow water fluid on a rotating sphere'
  integer :: hst_intstep=200                     ! ヒストリー出力間隔ステップ数
  namelist /history/    hst_file, title, hst_intstep

  ! (リスタート出力)
  character(len=100) :: rst_file=''              ! リスタート出力ファイル名
  integer :: rst_intstep=200                     ! リスタート出力間隔ステップ数
  namelist /restart/   rst_file, rst_intstep 

!== メインルーチン ==========================================================

 !---------------- NAMELIST 読み込み ---------------------
  write(6,nml=message) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=message) ; write(6,nml=message) ; close(10)

  if (verbose) write(6,nml=gridset) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=gridset) ; write(6,nml=gridset) ; close(10)

  if (verbose) write(6,nml=physics) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=physics) ; write(6,nml=physics) ; close(10)

  if (verbose) write(6,nml=initial) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=initial) ; write(6,nml=initial) ; close(10)

  if (verbose) write(6,nml=tint) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=tint) ; write(6,nml=tint) ; close(10)

  if (verbose) write(6,nml=history) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=history) ; write(6,nml=history) ; close(10)

  if (verbose) write(6,nml=restart) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=restart) ; write(6,nml=restart) ; close(10)

 !---------------- デバッグ出力制御設定 -----------------
  if (DebugOn) then
    call SetDebug
  end if

 !------------------ 変数の割り付け ---------------------
  allocate(xy_Vor(0:im-1,jm),xy_Div(0:im-1,jm))
  allocate(xy_Hsfc(0:im-1,jm),xy_Htopo(0:im-1,jm))

  allocate(w_Vor((nm+1)*(nm+1)),w_Div((nm+1)*(nm+1)))
  allocate(w_Hsfc((nm+1)*(nm+1)),w_Htopo((nm+1)*(nm+1)))
  allocate(w_StrFunc((nm+1)*(nm+1)),w_VelPot((nm+1)*(nm+1)))
  allocate(w_KEnergy((nm+1)*(nm+1)))

  allocate(w_VorTmp((nm+1)*(nm+1)),w_DivTmp((nm+1)*(nm+1)))
  allocate(w_HsfcTmp((nm+1)*(nm+1)))

  allocate(xy_VelLonCosLat(0:im-1,jm),xy_VelLatCosLat(0:im-1,jm))

  allocate(w_DVorDtK1((nm+1)*(nm+1)),w_DVorDtK2((nm+1)*(nm+1)),  &
           w_DVorDtK3((nm+1)*(nm+1)),w_DVorDtK4((nm+1)*(nm+1)) )
  allocate(w_DDivDtK1((nm+1)*(nm+1)),w_DDivDtK2((nm+1)*(nm+1)),  &
           w_DDivDtK3((nm+1)*(nm+1)),w_DDivDtK4((nm+1)*(nm+1)) )
  allocate(w_DHsfcDtK1((nm+1)*(nm+1)),w_DHsfcDtK2((nm+1)*(nm+1)), &
           w_DHsfcDtK3((nm+1)*(nm+1)),w_DHsfcDtK4((nm+1)*(nm+1)) )

  allocate(xy_Coli(0:im-1,jm))
  allocate(w_HVisc((nm+1)*(nm+1)),w_HDiff((nm+1)*(nm+1)))

  allocate(xy_VelLon(0:im-1,jm),xy_VelLat(0:im-1,jm))
  allocate(xy_HsfcTrue(0:im-1,jm))
  allocate(xy_VelLonTrue(0:im-1,jm),xy_VelLatTrue(0:im-1,jm))
  
  allocate(xy_FluxVorLon(0:im-1,jm),xy_FluxVorLat(0:im-1,jm))

 !------------------ 座標値の設定 -----------------------
  call DbgMessage(fmt='call %c', c1='w_initial') 
  call w_Initial(nm,im,jm)
  w_spectrum_VMiss = vmiss

 !------------------ 物理係数の設定 -----------------------
  xy_Coli = 2 * Omega * ( -cos(xy_Lon)*cos(xy_Lat)*sin(Alpha*pi/180.0) &
                         + sin(xy_Lat)*cos(Alpha*pi/180.0) )

  w_HVisc = HVisc &
          *( (-rn(:,1)/Radius**2)**HVOrder &
              -(2.0D0/Radius**2)**HVOrder )

  w_HDiff = HDiff * (-rn(:,1)/Radius**2)**HDOrder

  ! rn(ln(0,0,1) は正の値なので修正しておく.
  w_HVisc(l_nm(0,0)) = 0.0D0

 !------------------- 初期値設定 ----------------------
  time = initial_time

  if ( initial_file == "") then
     ! リスタートファイルを指定しない場合, 
     ! 内部で w_Vor, w_Div, w_Hsfc, w_Htopo を与える. 
     call set_initial_values
  else
     !  初期値設定(リスタートファイルからの読みこみ)
     call HistoryGet( trim(initial_file), 'w_vor',   w_Vor,   time )
     call HistoryGet( trim(initial_file), 'w_div',   w_div,   time )
     call HistoryGet( trim(initial_file), 'w_hsfc',  w_Hsfc,  time )
     call HistoryGet( trim(initial_file), 'w_htopo', w_Htopo )
  endif

 ! 渦度・発散から流線関数・速度ポテンシャルならびに速度成分を計算
  call VorDiv2Velocity(                  &
       w_Vor, w_Div         )              !(in)  渦度, 発散

  xy_Vor = xy_w(w_Vor) ; xy_Div = xy_w(w_Div) ; xy_HSfc = xy_w(w_Hsfc)

 !------------------- 解析解 --------------------
 ! 解析解は定常
  xy_HsfcTrue = xy_Hsfc                       
  xy_VelLonTrue = xy_VelLonCosLat/cos(xy_Lat)
  xy_VelLatTrue = xy_VelLatCosLat/cos(xy_Lat)

 !------------- 時間積分(Runge-Kutta + Crank-Nicolson 法) --------------
  call output_restart_init
  call output_history_init

  if ( initial_file == '' ) call output_history    ! 内部で与えた初期値は出力

  call DbgMessage(fmt='%c %c', &
&                 c1=DbgMessageFmt, &
&                 c2='Time integration starts.') 

  do it=1,nstep
     time = initial_time + it * delta_t

     !---- 1 段目 [ k1 = f( x_n, t_n) ] ----
     call TimeDerivetives_noDisppation(                 &
       w_Vor, w_Div, w_Hsfc,                            &!(in) 渦度, 発散, 変位
       w_DVorDtK1, w_DDivDtK1, w_DHsfcDtK1)              !(out) 時間変化

     !---- 2 段目 [ k2 = f( x_n+k1*dt/2, t_n+dt/2 ) ] ----
     w_VorTmp  = w_Vor  + w_DVorDtK1  * delta_t/2.0D0
     w_DivTmp  = w_Div  + w_DDivDtK1  * delta_t/2.0D0
     w_HsfcTmp = w_Hsfc + w_DHsfcDtK1 * delta_t/2.0D0

     xy_Vor = xy_w(w_VorTmp)
     xy_Div = xy_w(w_DivTmp)
     xy_HSfc = xy_w(w_HsfcTmp)

     call TimeDerivetives_noDisppation(                 &
       w_VorTmp, w_DivTmp, w_HsfcTmp,                   &!(in) 渦度, 発散, 変位
       w_DVorDtK2, w_DDivDtK2, w_DHsfcDtK2)              !(out) 時間変化

     !---- 3 段目 [ k3 = f( x_n+k2*dt/2, t_n+dt/2 ) ] ----
     w_VorTmp  = w_Vor  + w_DVorDtK2  * delta_t/2.0D0
     w_DivTmp  = w_Div  + w_DDivDtK2  * delta_t/2.0D0
     w_HsfcTmp = w_Hsfc + w_DHsfcDtK2 * delta_t/2.0D0

     xy_Vor = xy_w(w_VorTmp)
     xy_Div = xy_w(w_DivTmp)
     xy_HSfc = xy_w(w_HsfcTmp)

     call TimeDerivetives_noDisppation(                 &
       w_VorTmp, w_DivTmp, w_HsfcTmp,                   &!(in) 渦度, 発散, 変位
       w_DVorDtK3, w_DDivDtK3, w_DHsfcDtK3)              !(out) 時間変化

     !---- 4 段目 [ k4 = f( x_n+k3*dt, t_n+dt ) ] ----
     w_VorTmp  = w_Vor  + w_DVorDtK3  * delta_t
     w_DivTmp  = w_Div  + w_DDivDtK3  * delta_t
     w_HsfcTmp = w_Hsfc + w_DHsfcDtK3 * delta_t

     xy_Vor = xy_w(w_VorTmp)
     xy_Div = xy_w(w_DivTmp)
     xy_HSfc = xy_w(w_HsfcTmp)

     call TimeDerivetives_noDisppation(                 &
       w_VorTmp, w_DivTmp, w_HsfcTmp,                   &!(in) 渦度, 発散, 変位
       w_DVorDtK4, w_DDivDtK4, w_DHsfcDtK4)              !(out) 時間変化

     !---- 積分(Runge-Kutta) ----
     w_Vor  = w_Vor  + delta_t * (   w_DVorDtK1/6.0D0 + w_DVorDtK2/3.0D0 &
                                   + w_DVorDtK3/3.0D0 + w_DVorDtK4/6.0D0   )
     w_Div  = w_Div  + delta_t * (  w_DDivDtK1/6.0D0 + w_DDivDtK2/3.0D0 &
                                   + w_DDivDtK3/3.0D0 + w_DDivDtK4/6.0D0   )
     w_Hsfc = w_Hsfc + delta_t * (   w_DHsfcDtK1/6.0D0 + w_DHsfcDtK2/3.0D0 &
                                   + w_DHsfcDtK3/3.0D0 + w_DHsfcDtK4/6.0D0  )

     !---- 散逸項積分(Crank-Nicolson) ----

     w_Vor  = (1-w_HVisc*delta_t/2.0D0)/(1+w_HVisc*delta_t/2.0D0) * w_Vor
     w_Div  = (1-w_HVisc*delta_t/2.0D0)/(1+w_HVisc*delta_t/2.0D0) * w_Div
     w_Hsfc = (1-w_HDiff*delta_t/2.0D0)/(1+w_HDiff*delta_t/2.0D0) * w_Hsfc

     xy_Vor = xy_w(w_Vor) ; xy_Div = xy_w(w_Div) ; xy_HSfc = xy_w(w_Hsfc)

     if(mod(it,hst_intstep) .eq. 0)then                    ! ヒストリー出力
       ! 渦度・発散から流線関数・速度ポテンシャルならびに速度成分を計算
        call VorDiv2Velocity(                  &
          w_Vor, w_Div         )              !(in)  渦度, 発散

        call output_history
     endif

     if(mod(it,rst_intstep) .eq. 0)then                    ! リスタート出力
        call output_restart
     endif
  enddo

  call DbgMessage(fmt='%c %c',      &
&                 c1=DbgMessageFmt, &
&                 c2='Time integration end.') 

  if(.not. mod(it-1,rst_intstep) .eq. 0)then                    ! 最終出力
     call output_restart
  endif

  call output_restart_close
  call output_history_close

! 以上 メインプログラム 
!-----------------------------------------------------------------------------
! 以下 サブルーチン

contains

!=========================== 初期値設定 ============================
 !
 !  初期値設定(リスタートファイルない場合のデフォルト設定)
 !
  subroutine set_initial_values    ! w_Vor, w_Div, w_Hsfc, w_Htopo を与える. 

    real(8) :: U0    = 3.861068D1           ! 剛体回転流の速度(m/sec)

    namelist /case2/  U0

    if (verbose) write(6,nml=case2) 
    open(10,file=nmlfile,status='OLD')
    read(10,nml=case2) ; write(6,nml=case2) ; close(10)

    w_Vor = w_Lapla_w(w_xy( &
                     - Radius * U0 * &
                         (sin(xy_Lat)*cos(Alpha*pi/180.0) &
                           -cos(xy_Lon)*cos(xy_Lat)*sin(Alpha*pi/180.0)) &
             ))/Radius**2

    w_Div= 0.0D0

    xy_Hsfc= - (Radius*Omega*U0+ U0**2/2.0) &
              * ( -cos(xy_Lon)*cos(xy_Lat)*sin(Alpha*pi/180.0) &
                  +sin(xy_Lat)*cos(Alpha*pi/180.0))**2 &
              /Grav

    w_Hsfc = w_xy(xy_Hsfc) 
    w_Htopo = 0.0

  end subroutine set_initial_values

!=========================== 時間変化項計算 ============================

  !
  ! 散逸項を除いた時間変化計算
  !
  subroutine TimeDerivetives_noDisppation( &
       w_Vor, w_Div, w_Hsfc,               &             !(in) 渦度, 発散, 変位
       w_DVorDtNoDisp, w_DDivDtNoDisp, w_DHsfcDtNoDisp)  !(out) 時間変化

   !---- 変数(入力) ----
    real(8), intent(in) :: w_Vor(:)  ! 渦度
    real(8), intent(in) :: w_Div(:)  ! 発散
    real(8), intent(in) :: w_Hsfc(:) ! 変位

   !---- 変数(出力) ----
    real(8), intent(out) :: w_DHsfcDtNoDisp(:) ! 変位時間変化
    real(8), intent(out) :: w_DVorDtNoDisp(:)   ! 渦度時間変化
    real(8), intent(out) :: w_DDivDtNoDisp(:)   ! 発散時間変化

    ! 渦度・発散から流線関数・速度ポテンシャルならびに速度成分を計算
    call VorDiv2Velocity(                &
       w_Vor, w_Div         )              !(in)  渦度, 発散

     xy_FluxVorLon = (xy_Vor + xy_Coli) * xy_VelLonCosLat
     xy_FluxVorLat = (xy_Vor + xy_Coli) * xy_VelLatCosLat

     w_KEnergy = w_xy((xy_VelLonCosLat**2 + xy_VelLatCosLat**2) &
                       /(2.0*cos(xy_Lat)**2))                

     ! 時間変化項の見積り
     w_DVorDtNoDisp = - w_DivLambda_xy(xy_FluxVorLon)/Radius &
                      - w_DivMu_xy(xy_FluxVorLat)/Radius

     w_DDivDtNoDisp =   w_DivLambda_xy(xy_FluxVorLat)/Radius            &
                    - w_DivMu_xy(xy_FluxVorLon)/Radius                  &
                    - w_Lapla_w( Grav*(w_Hsfc+ w_Htopo)+w_KEnergy )/Radius**2 

     w_DHsfcDtNoDisp = - w_DivLambda_xy(xy_Hsfc*xy_VelLonCosLat)/Radius &
                       - w_DivMu_xy(xy_Hsfc*xy_VelLatCosLat)/Radius &
                       - Hbar * w_Div
    
  end subroutine TimeDerivetives_noDisppation

 !
 ! 渦度・発散から流線関数・速度ポテンシャルならびに速度成分を計算
 !
  subroutine VorDiv2Velocity(            &
       w_Vor, w_Div         )              !(in)  渦度, 発散

   !---- 変数(入力) ----
    real(8), intent(in)  :: w_Vor(:)                ! 渦度
    real(8), intent(in)  :: w_Div(:)                ! 発散


    ! 渦度・発散から流線関数・速度ポテンシャルを計算
    w_Strfunc = w_LaplaInv_w(w_Vor) * Radius**2
    w_VelPot  = w_LaplaInv_w(w_Div) * Radius**2

    ! 流線関数・速度ポテンシャルから速度成分を計算
    xy_VelLonCosLat =  -xy_Gradmu_w(w_StrFunc)/Radius &
                      + xy_GradLambda_w(w_VelPot)/Radius 
    xy_VelLatCosLat =   xy_GradLambda_w(w_StrFunc)/Radius &
                      + xy_Gradmu_w(w_VelPot)/Radius 

  end subroutine VorDiv2Velocity


!=========================== リスタート出力 ============================
 !
 ! リスタート出力初期化
 !
  subroutine output_restart_init
    call HistoryCreate( &
           file=trim(rst_file), &
           title=trim(title), &
           source='spshallow-zd_rn4cn_case2.f90 (2008/008/10)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','nm ','t  '/), &
           dimsizes=(/im,jm,(nm+1)**2,0/),&
           longnames=(/'Longitude            ','Latitude             ',&
                       'Hor.wave number index','time                 '/),&
           units=(/'radian','radian','1     ','sec   '/),   &
           origin=real(time), interval=real(rst_intstep*delta_t), &
           xtypes=(/'real'/), history=hst_rst)

   !---- 座標変数定義, 出力 ----
    call HistoryPut('lon',x_Lon, hst_rst)                     ! 変数出力
    call HistoryAddattr('lon','topology','circular', hst_rst) ! 周期属性
    call HistoryAddattr('lon','modulo',2*pi, hst_rst)         ! 周期属性
    call HistoryPut('lat',y_Lat, hst_rst)                     ! 変数出力
    call HistoryPut('nm',(/(dble(n),n=0,(nm+1)**2)/), hst_rst)! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
           varname='lon_weight', dims=(/'lon'/), & 
           longname='weight for integration in longitude', &
           units='radian', xtype='double',history=hst_rst)
    call HistoryAddVariable( &                                ! 変数定義
           varname='coslat_lat_weight', dims=(/'lat'/), & 
           longname='cos(lat) weight for integration in latitide', &
           units='1', xtype='double',history=hst_rst)
    call HistoryPut('lon_weight',x_Lon_weight,hst_rst)        ! 変数出力
    call HistoryPut('coslat_lat_weight',y_Lat_weight,hst_rst) ! 変数出力

   !---- 物理変数定義 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='w_vor', dims=(/'nm','t '/), & 
           longname='Vorticity', &
           units='1/sec', xtype='double', history=hst_rst)
    call HistoryAddVariable( &                                ! 変数定義
           varname='w_div', dims=(/'nm','t '/), & 
           longname='Divergence', &
           units='1/sec', xtype='double', history=hst_rst)
    call HistoryAddVariable( &                                ! 変数定義
           varname='w_hsfc', dims=(/'nm','t '/), & 
           longname='Suface height', &
           units='m', xtype='double', history=hst_rst)
    call HistoryAddVariable( &                                ! 変数定義
           varname='w_htopo', dims=(/'nm'/), & 
           longname='Bottom topography', &
           units='m', xtype='double', history=hst_rst)

   !---- 物理変数出力 ----
    call HistoryPut('w_htopo', w_Htopo, hst_rst)

   !---- 実験パラメターを属性として定義, 出力(全て Global 属性) ----
    call HistoryAddAttr('lon','+Radius',  Radius,  hst_rst)
    call HistoryAddAttr('lon','+Omega',   Omega,   hst_rst)
    call HistoryAddAttr('lon','+Alpha',   Alpha,   hst_rst)
    call HistoryAddAttr('lon','+Grav',    Grav,    hst_rst)
    call HistoryAddAttr('lon','+Hbar',    Hbar,    hst_rst)
    call HistoryAddAttr('lon','+HVOrder', HVOrder, hst_rst)
    call HistoryAddAttr('lon','+HVisc',   HVisc,   hst_rst)
    call HistoryAddAttr('lon','+HDOrder', HDOrder, hst_rst)
    call HistoryAddAttr('lon','+HDiff',   HDiff,   hst_rst)
    call HistoryAddAttr('lon','+delta_t', delta_t, hst_rst)

  end subroutine output_restart_init

 !
 ! リスタート出力
 !
  subroutine output_restart
    write(6,*) ' Restart file output at it = ',it, '  time = ', time
    call HistoryPut('t',real(time),hst_rst)

   !---- 物理変数出力 ----
    call HistoryPut('w_vor', w_Vor, hst_rst)
    call HistoryPut('w_div', w_Div, hst_rst)
    call HistoryPut('w_hsfc',w_Hsfc,hst_rst)
  end subroutine output_restart

 !
 ! リスタート出力終了
 !
  subroutine output_restart_close
    call HistoryClose(hst_rst)
  end subroutine output_restart_close


!=========================== ヒストリー出力 ============================
 !
 ! ヒストリー出力初期化
 !
  subroutine output_history_init

   !---- ヒストリーファイル作成 ----
    call HistoryCreate( &
           file=trim(hst_file), &
           title=trim(title), &
           source='spshallow-zd_rn4cn_case2.f90 (2008/08/10)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','nm ','n  ','m  ','t  '/), &
           dimsizes=(/im,jm,(nm+1)**2,nm+1,2*nm+1,0/),&
           longnames=(/'Longitude            ','Latitude             ',&
                       'Hor.wave number index','Hor.total wave number',&
                       'zonal wave number    ','time                 '/),&
           units=(/'degree','degree','1     ','1     ','1     ','sec   '/),   &
           origin=real(time), interval=real(hst_intstep*delta_t), &
           xtypes=(/'real'/))

   !---- 座標変数定義, 出力 ----
    call HistoryPut('lon',x_Lon/pi*180)                       ! 変数出力
    call HistoryAddattr('lon','topology','circular')          ! 周期属性
    call HistoryAddattr('lon','modulo',360.0)                 ! 周期属性
    call HistoryPut('lat',y_Lat/pi*180)                       ! 変数出力
    call HistoryPut('nm',(/(dble(n),n=0,(nm+1)**2)/))         ! 変数出力
    call HistoryPut('n',(/(dble(n),n=0,nm)/))                 ! 変数出力
    call HistoryPut('m',(/(dble(m),m=-nm,nm)/))               ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
           varname='lon_weight', dims=(/'lon'/), & 
           longname='weight for integration in longitude', &
           units='radian', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='coslat_lat_weight', dims=(/'lat'/), & 
           longname='cos(lat) weight for integration in latitide', &
           units='1', xtype='double')
    call HistoryPut('lon_weight',x_Lon_weight)                ! 変数出力
    call HistoryPut('coslat_lat_weight',y_Lat_weight)         ! 変数出力

   !---- 物理変数定義 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='vor', dims=(/'lon','lat','t  '/), & 
           longname='Vorticity', units='1/sec', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='div', dims=(/'lon','lat','t  '/), & 
           longname='Divergence', units='1/sec', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='hsfc', dims=(/'lon','lat','t  '/), & 
           longname='Surface height', units='m', xtype='double')

    call HistoryAddVariable( &                                ! 変数定義
           varname='strfunc', dims=(/'lon','lat','t  '/), & 
           longname='Stream function', units='m2/sec', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='velpot', dims=(/'lon','lat','t  '/), & 
           longname='Velocity potential', units='m2/sec', xtype='double')

    call HistoryAddVariable( &                                ! 変数定義
           varname='vellon', dims=(/'lon','lat','t  '/), & 
           longname='lon-velocity', units='m/sec', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vellat', dims=(/'lon','lat','t  '/), & 
           longname='lat-velocity', units='m/sec', xtype='double')

    call HistoryAddVariable( &                                ! 変数定義
           varname='htopo', dims=(/'lon','lat'/), & 
           longname='Bottom topography', units='m', xtype='double')

   !---- 診断変数定義 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='l1hsfc', dims=(/'t'/), & 
           longname='Surface height error (type1)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='l2hsfc', dims=(/'t'/), & 
           longname='Surface height error (type2)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='linfhsfc', dims=(/'t'/), & 
           longname='Surface height error (type infty)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='l1vel', dims=(/'t'/), & 
           longname='Velocity error (type1)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='l2vel', dims=(/'t'/), & 
           longname='Velocity error (type2)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='linfvel', dims=(/'t'/), & 
           longname='Velocity error (type infty)', &
           units='1', xtype='double')

   !---- 物理変数出力 ----
    call HistoryPut('htopo', xy_Htopo)

   !---- 実験パラメターを属性として定義, 出力(全て Global 属性) ----
    call HistoryAddAttr('lon','+Radius',  Radius )
    call HistoryAddAttr('lon','+Omega',   Omega  )
    call HistoryAddAttr('lon','+Alpha',   Alpha  )
    call HistoryAddAttr('lon','+Grav',    Grav   )
    call HistoryAddAttr('lon','+Hbar',    Hbar   )
    call HistoryAddAttr('lon','+HVOrder', HVOrder)
    call HistoryAddAttr('lon','+HVisc',   HVisc  )
    call HistoryAddAttr('lon','+HDOrder', HDOrder)
    call HistoryAddAttr('lon','+HDiff',   HDiff  )
    call HistoryAddAttr('lon','+delta_t', delta_t)

  end subroutine output_history_init

 !
 ! ヒストリー出力
 !
  subroutine output_history
    write(6,*) ' History file output at it = ',it, '  time = ', time
    call HistoryPut('t',real(time))

   !---- 物理変数出力 ----
    call HistoryPut('vor', xy_Vor)
    call HistoryPut('div', xy_Div)
    call HistoryPut('hsfc',xy_Hsfc)

    call HistoryPut('strfunc', xy_w(w_StrFunc))
    call HistoryPut('velpot',  xy_w(w_VelPot))

    xy_VelLon = xy_VelLonCosLat/cos(xy_Lat)
    xy_VelLat = xy_VelLatCosLat/cos(xy_Lat)
    call HistoryPut('vellon',xy_VelLon)
    call HistoryPut('vellat',xy_VelLat)

   !---- 診断変数出力 ----
    call HistoryPut('l1hsfc', &
         AvrLonLat_xy(abs(xy_Hsfc-xy_HsfcTrue))/AvrLonLat_xy(abs(xy_HsfcTrue)))
    call HistoryPut('l2hsfc', &
         sqrt(AvrLonLat_xy((xy_Hsfc-xy_HsfcTrue)**2)&
              /AvrLonLat_xy(xy_HsfcTrue**2)))
    call HistoryPut('linfhsfc', &
         maxval(abs(xy_Hsfc-xy_HsfcTrue))/maxval(abs(xy_HsfcTrue)))
    call HistoryPut('l1vel', &
           AvrLonLat_xy(sqrt((xy_VelLon-xy_VelLonTrue)**2 &
                           +(xy_VelLat-xy_VelLatTrue)**2)) &
          /AvrLonLat_xy(sqrt(xy_VelLonTrue**2+xy_VelLatTrue**2)))
    call HistoryPut('l2vel', &
           sqrt(AvrLonLat_xy((xy_VelLon-xy_VelLonTrue)**2 &
                           +(xy_VelLat-xy_VelLatTrue)**2) &
                /AvrLonLat_xy(xy_VelLonTrue**2+xy_VelLatTrue**2)))
    call HistoryPut('linfvel', &
           maxval(sqrt((xy_VelLon-xy_VelLonTrue)**2 &
                           +(xy_VelLat-xy_VelLatTrue)**2)) &
           /maxval(sqrt(xy_VelLonTrue**2+xy_VelLatTrue**2)))
  end subroutine output_history

 !
 ! ヒストリー出力終了
 !
  subroutine output_history_close
    call HistoryClose
  end subroutine output_history_close

end program spshallow_zd_rn4cn_case2
