!--------------------------------------------------------------------------
!  Copyright (C) 2005 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
!
!Title(表題): Two-dimensional barotropic model on a double cyclic region
!             (2 重周期境界領域上の2次元順圧モデル)
!
!       The time integration is performed with the 4th order 
!       Runge-Kutta scheme. 
!       The beta parameter is constant. 
!       The viscous linear term is transformed by the exponential function.  
!       The Program is organized for test3 (test for modon solution). 
!       This program should be complied with Fujitsu Fortran90 compiler 
!    and linked with SSL2 library since the subroutines for calculation 
!    of Bessel functions are used. 
!
!History(履歴):
!   2005/10/28  竹広真一, 新規作成
!   2005/10/31  竹広真一, k,l 次元出力 bug fix
!
! The governing equation(支配方程式): 
!
!      d\nabla^2\psi/dt 
!          = -J(\psi,\nabla^2\psi) - beta d\psi/dx
!            +(-1)^{p+1}\nu_{2p}\nabla^2p \nabla^2\psi
!
program plbaro_beta_rn4exp_test3

!== モジュール引用宣言 ================================================
  use ee_module 
  use gt4_history, only : GT_HISTORY, HistoryCreate, HistoryPut, HistoryGet, &
                          HistoryAddVariable, HistoryClose, HistoryAddAttr
  use dc_trace,    only : SetDebug, BeginSub, EndSub, DbgMessage
  use dc_message,  only : MessageNotify
  implicit none

!== 宣言部 ============================================================

 !---- 変数(格子点データ) ----
  real(8), allocatable :: yx_VelX(:,:)        ! 速度経度成分
  real(8), allocatable :: yx_VelY(:,:)        ! 速度緯度成分
  real(8), allocatable :: yx_Vor(:,:)         ! 渦度(鉛直成分)
  real(8), allocatable :: yx_StrFunc(:,:)     ! 流線関数

  real(8), allocatable :: yx_VorAnalytic(:,:) ! 渦度(鉛直成分)解析解

 !---- 変数(スペクトルデータ) ----
  real(8), allocatable :: ee_Vor(:,:)         ! 渦度(鉛直成分)
  real(8), allocatable :: ee_StrFunc(:,:)     ! 流線関数

  real(8), allocatable :: ee_Vortmp(:,:)      ! 渦度(ルンゲクッタ計算作業用)
  real(8), allocatable :: ee_k1(:,:)          ! 拡散以外の渦度時間変化(1段目)
  real(8), allocatable :: ee_k2(:,:)          ! 拡散以外の渦度時間変化(2段目)
  real(8), allocatable :: ee_k3(:,:)          ! 拡散以外の渦度時間変化(3段目)
  real(8), allocatable :: ee_k4(:,:)          ! 拡散以外の渦度時間変化(4段目)

  real(8), allocatable :: ee_VorAnalytic(:,:) ! 渦度(鉛直成分)解析解

 !---- 固定パラメタ -----
  real(8), parameter  :: pi = 3.141592653589793D0 ! 円周率
  character(len=20)   :: DbgMessageFmt='*** DbgMESSAGE ***'
  real(8), parameter  :: vmiss = -999.0       ! 欠損値

 !---- NAMELIST 変数 ----
  ! NAMELIST 入力用テンポラリファイル名(名前変更禁止)
  character(len=30),parameter  :: nmlfile='plbaro-beta_rn4exp_test3.nml'

  logical            :: Verbose=.false.          ! 冗長なメッセージ出力 On/Off
  logical            :: DebugOn=.false.          ! デバッグメッセージ On/Off
  namelist /message/  Verbose, DebugOn           !
                                                 !-- 格子点・スペクトル --
  integer :: km=21                               ! X 方向切断全波数
  integer :: lm=21                               ! Y 方向切断全波数
  integer :: im=64                               ! X 方向格子点数 (>3*km)
  integer :: jm=64                               ! Y 方向格子点数 (>3*lm)
  namelist /gridset/ km, lm, im, jm
                                                 !-- 物理パラメター --
  real(8)            :: XLength=1.0D0            ! 領域の大きさ(X 方向)
  real(8)            :: YLength=1.0D0            ! 領域の大きさ(Y 方向)
  real(8)            :: Beta=0.0D0               ! βパラメター
  integer            :: HVOrder=1                ! 超粘性の次数(1 で普通の粘性,
                                                 ! 水平ラプラシアンの階数)
  real(8)            :: HVisc=1.0D0              ! 超粘性係数
  namelist /physics/  XLength, YLength, Beta, HVOrder, HVisc
                                                 ! -- 初期値 --
  character(len=100) :: initial_file=''          ! 初期値データファイル名
                                                 ! (空なら内部で初期値を計算)
  real               :: initial_time=0.0         ! 初期時刻
  namelist /initial/ initial_file, initial_time  ! 
                                                 ! -- 時間積分 --
  real(8) :: delta_t=1.0e-7                      ! 時間積分刻み
  integer :: nstep=2000                          ! 時間積分ステップ数
  namelist /tint/    delta_t, nstep              ! 
                                                 ! -- ヒストリー出力 --
  character(len=100) :: hst_file=   ''           ! ヒストリーファイル名
  character(len=100) :: title = &                ! タイトル
   'Test of linear terms of 2-dim barotropic model on a double-cyclic domain'

  integer :: hst_intstep=200                     ! ヒストリー出力間隔ステップ数
  namelist /history/    hst_file, title, hst_intstep

  character(len=100) :: rst_file=''              ! リスタート出力ファイル名
  integer :: rst_intstep=200                     ! リスタート出力間隔ステップ数
  namelist /restart/   rst_file, rst_intstep 

                                                 !---- 初期値 1 モドン
  real(8) :: Radius = 1.0                        ! モドン半径
  real(8) :: Speed  = 1.0                        ! モドン伝播速度
  real(8) :: XCenter = 0.5                       ! 初期中心位置 X 座標
  real(8) :: YCenter = 0.5                       ! 初期中心位置 Y 座標

  namelist /modon/ Radius, Speed, XCenter, YCenter

 !---- 作業変数 ----
  real(8), allocatable :: ee_HVisc(:,:)      ! 超粘性係数

  integer :: it=0                            ! 時間ステップ
  real(8) :: time                            ! モデル内時間
  integer :: k, l                            ! 波数
  integer :: i, j                            ! 座標変数インデックス

  type(GT_HISTORY) :: hst_rst                ! リスタート GT_HISTORY 変数

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

  if (verbose) write(6,nml=modon) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=modon) ; write(6,nml=modon) ; close(10)

 !---------------- デバッグ出力制御設定 -----------------
  if (DebugOn) then
    call SetDebug
  end if

 !------------------ 変数の割り付け ---------------------
  allocate(yx_VelX(0:jm-1,0:im-1),yx_VelY(0:jm-1,0:im-1))
  allocate(yx_Vor(0:jm-1,0:im-1),yx_StrFunc(0:jm-1,0:im-1))
  allocate(yx_VorAnalytic(0:jm-1,0:im-1))

  allocate(ee_Vor(-lm:lm,-km:km),ee_StrFunc(-lm:lm,-km:km))
  allocate(ee_Vortmp(-lm:lm,-km:km))

  allocate(ee_k1(-lm:lm,-km:km),ee_k2(-lm:lm,-km:km))
  allocate(ee_k3(-lm:lm,-km:km),ee_k4(-lm:lm,-km:km))

  allocate(ee_HVisc(-lm:lm,-km:km))
  allocate(ee_VorAnalytic(-lm:lm,-km:km))

 !------------------ 座標値の設定 -----------------------
  call DbgMessage(fmt='call %c', c1='ee_initial') 
  call ee_Initial(im,jm,km,lm,0.0D0,XLength,0.0D0,YLength)

 !------------------ 物理係数の設定 -----------------------
  ee_Vor = 1.0D0
  ee_HVisc = (-1)**HVOrder * HVisc*(ee_Lapla_ee(ee_Vor))**HVOrder

 !------------------- 初期値設定 ----------------------
  time = initial_time

  if ( initial_file == "") then
     ! リスタートファイルを指定しない場合内部で ee_Vor を与える. 
     call set_initial_values
  else
     !  初期値設定(リスタートファイルからの読みこみ)
     call HistoryGet( trim(initial_file), 'ee_vor', ee_Vor,  time )
  endif

  ee_Strfunc = ee_LaplaInv_ee(ee_Vor)

 !---------------- 時間積分(Runge-Kutta + exp 変換法) -----------------
  call output_restart_init
  call output_history_init

  if ( initial_file == '' ) call output_history    ! 内部で与えた初期値は出力

  call DbgMessage(fmt='%c %c', &
&                 c1=DbgMessageFmt, &
&                 c2='Time integration starts.') 

  do it=1,nstep
     time = initial_time + it * delta_t

     !---- 1 段目 [ k1 = f( x_n, t_n) ] ----
     ee_k1 = ee_DVorDt_ee_ee(ee_Vor,ee_Strfunc)

     !---- 2 段目 [ k2 = f( x_n+k1*dt/2, t_n+dt/2 ) ] ----
     ee_Vortmp = exp(-ee_HVisc*delta_t/2) * ( ee_Vor + ee_k1 * delta_t/2.0D0 )
     ee_StrFunc = ee_LaplaInv_ee(ee_Vortmp)
     ee_k2 = exp(ee_HVisc*delta_t/2) * ee_DVorDt_ee_ee(ee_Vortmp,ee_Strfunc)

     !---- 3 段目 [ k3 = f( x_n+k2*dt/2, t_n+dt/2 ) ] ----
     ee_Vortmp = exp(-ee_HVisc*delta_t/2) * ( ee_Vor + ee_k2 * delta_t/2.0D0 )
     ee_StrFunc = ee_LaplaInv_ee(ee_Vortmp)
     ee_k3 = exp(ee_HVisc*delta_t/2) * ee_DVorDt_ee_ee(ee_Vortmp,ee_Strfunc)

     !---- 4 段目 [ k4 = f( x_n+k3*dt, t_n+dt ) ] ----
     ee_Vortmp = exp(-ee_HVisc*delta_t) * ( ee_Vor + ee_k3 * delta_t )
     ee_StrFunc = ee_LaplaInv_ee(ee_Vortmp)
     ee_k4 = exp(ee_HVisc*delta_t) * ee_DVorDt_ee_ee(ee_Vortmp,ee_Strfunc)

     !---- 積分 ----
     ee_Vor = exp(-ee_HVisc*delta_t) &
          * ( ee_Vor + delta_t * (   ee_k1/6.0D0 + ee_k2/3.0D0 &
                                   + ee_k3/3.0D0 + ee_k4/6.0D0 )  )

     ! 渦度から流線関数を計算
     ee_StrFunc = ee_LaplaInv_ee(ee_Vor)

     if(mod(it,hst_intstep) .eq. 0)then                    ! ヒストリー出力
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
 !======================= 時間変化(散逸項なし) ====================

  function ee_DVorDt_ee_ee(ee_Vor,ee_Strfunc)

    real(8) :: ee_Vor(:,:)                             ! 渦度(鉛直成分)
    real(8) :: ee_StrFunc(:,:)                         ! 流線関数

    ! 渦度時間変化(散逸なし)
    real(8) :: ee_DVorDt_ee_ee(size(ee_Vor,1),size(ee_Vor,2))

    ee_DVorDt_ee_ee = - ee_Jacobian_ee_ee(ee_StrFunc,ee_Vor)  &
                      - Beta * ee_Dx_ee(ee_StrFunc)           

  end function ee_DVorDt_ee_ee

!=========================== モドン解 ============================
 !
 !  モドン解渦度設定
 !
  subroutine set_VorMod( &
                    yx_VorMod,  & !(out) モドン解渦度       
                    Ra,         & !(in)  モドン解半径       
                    Cs,         & !(in)  モドン解伝播速度   
                    Xc,         & !(in)  モドン解中心 X 座標
                    Yc          & !(in)  モドン解中心 Y 座標
                   )

    real(8), intent(out):: yx_VorMod(0:,0:)     ! モドン解渦度
    real(8), intent(in) :: Ra                   ! モドン解半径
    real(8), intent(in) :: Cs                   ! モドン解伝播速度
    real(8), intent(in) :: Xc                   ! モドン解中心 X 座標
    real(8), intent(in) :: Yc                   ! モドン解中心 Y 座標

    ! 内部パラメター
    real(8),parameter :: eps=1.0D-10            ! K を定めるための打ちきり誤差

    ! 作業変数
    real(8) :: Kp                  ! モドン解パラメター
    real(8) :: Qp                  ! モドン解パラメター
    real(8) :: J1KR, J1K           ! 第 1 種ベッセル関数の値
    real(8) :: K1KR, K1K           ! 第 2 種変形ベッセル関数の値
    real(8) :: Rad                 ! 中心からの半径
    real(8) :: SinTheta            ! 中心極座標系 \sin\theta
    integer :: ICON                ! サブルーチンエラーコード
    real(8) :: DX, DY              ! 作業変数

    Qp = Ra*sqrt(Beta/Cs)
    Kp = Modon_Kparameter(Qp,eps)

    call dbj1(Kp,J1K,ICON)
    call dbk1(Qp,K1K,ICON)
    do i=0,im-1
       do j=0,jm-1
          if ( yx_X(j,i)-mod(Xc,XLength) .LT. -XLength/2 )then 
             DX=yx_X(j,i)+XLength-mod(Xc,XLength)
          else if ( yx_X(j,i)-mod(Xc,XLength) .GT. XLength/2 )then 
             DX=yx_X(j,i)-XLength-mod(Xc,XLength)
          else
             DX=yx_X(j,i)-mod(Xc,XLength)
          endif
          if ( yx_Y(j,i)-mod(Yc,YLength) .LT. -YLength/2 )then 
             DY=yx_Y(j,i)+YLength-mod(Yc,YLength)
          else if ( yx_Y(j,i)-mod(Yc,YLength) .GT. YLength/2 )then 
             DY=yx_Y(j,i)-YLength-mod(Yc,YLength)
          else
             DY=yx_Y(j,i)-mod(Yc,YLength)
          endif

          Rad   = sqrt( DX**2 + DY**2 )
          if ( Rad .NE. 0.0D0 ) then
             SinTheta = DY/Rad
          else
             SinTheta = 0.0D0
          endif
          if ( Rad .LE. Ra ) then
             call dbj1(Kp*Rad/Ra,J1KR,ICON)
             yx_VorMod(j,i) = - Cs * Qp**2/Ra * SinTheta * J1KR/J1K
          else
             call dbk1(Qp*Rad/Ra,K1KR,ICON)
             yx_VorMod(j,i) = - Cs * Qp**2/Ra * SinTheta * K1KR/K1K
          endif
       enddo
    enddo

  end subroutine set_VorMod
  !
  ! Solving Modon dispersion relation.                S.Takehiro Oct, 21, 2005.
  !
  ! Calculating parameter k by giving the value of parameter q
  ! with the relation,  
  !      -J_2(k)/kJ_1(k) = K_2(q)/qK_1(q). 
  ! The bi-section method is used in the function. 
  !
  function Modon_Kparameter(Qp,eps)

    real(8), intent(in) :: Qp                      ! パラメター q
    real(8), intent(in) :: eps                     ! 打ちきり判定誤差
    real(8)             :: Modon_Kparameter        ! パラメター k

    ! 作業変数
    real(8) :: k1 = 3.83171       ! 初期値 J_1 第一零点
    real(8) :: k2 = 5.13562       ! 初期値 J_2 第一零点
    real(8) :: kp                 ! パラメター予想値

    real(8) :: J1K, J2K           ! 第 1 種ベッセル関数の値
    real(8) :: K1Q, K2Q           ! 第 2 種変形ベッセル関数の値
    integer :: ICON               ! サブルーチンエラーコード
    real(8) :: f1, f2, fp         ! 作業変数

  1000 continue  
    call dbk1(Qp,K1Q,ICON)
    call dbkn(Qp,2,K2Q,ICON)

    call dbj1(k1,J1K,ICON)
    call dbjn(k1,2,J2K,ICON)

    f1 = J2K + K2Q/(Qp*K1Q)*k1*J1K   ! J_2(k) + K_2(q)/qK_1(q) * kJ_1(k) =0

    call dbj1(k2,J1K,ICON)
    call dbjn(k2,2,J2K,ICON)
    f2 = J2K + K2Q/(Qp*K1Q)*k2*J1K   ! J_2(k) + K_2(q)/qK_1(q) * kJ_1(k) =0

    if ( f1*f2 .gt. 0.0D0 ) then
       write(6,*) 'f1 and f2 are the same sign.'
       stop
    endif

  !  kp = (abs(f1)*k2 + abs(f2)*k1)/abs(f1-f2)
    kp = (k1+k2)/2

    call dbj1(kp,J1K,ICON)
    call dbjn(kp,2,J2K,ICON)
    fp = J2K + K2Q/(Qp*K1Q)*kp*J1K   ! J_2(k) + K_2(q)/qK_1(q) * kJ_1(k) =0

    if ( abs(k1-k2)/abs(k1) .lt. eps ) then
        Modon_Kparameter=kp
        return
    end if

    if ( f1*fp .gt. 0.0D0 ) then
       k1 = kp
    else
       k2 = kp
    endif

    goto 1000

 end function Modon_Kparameter

!=========================== 初期値設定 ============================
 !
 !  初期値設定(リスタートファイルない場合のデフォルト設定)
 !
  subroutine set_initial_values    ! w_Vor を定める

    call set_VorMod( &
                    yx_Vor,     & !(out) モドン解渦度       
                    Radius,     & !(in)  モドン解半径       
                    Speed,      & !(in)  モドン解伝播速度   
                    XCenter,    & !(in)  モドン解中心 X 座標
                    YCenter     & !(in)  モドン解中心 Y 座標
                   )
    
    ee_Vor = ee_yx(yx_Vor)

  end subroutine set_initial_values

!=========================== リスタート出力 ============================
 !
 ! リスタート出力初期化
 !
  subroutine output_restart_init
    call HistoryCreate( &
           file=trim(rst_file), &
           title=trim(title), &
           source='plbaro-beta_rn4exp_test3.f90 (2005/10/31)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'x','y','k','l','t'/), &
           dimsizes=(/im,jm,2*km+1,2*lm+1,0/),&
           longnames=(/'X           ','Y           ',&
                       'X-wavenumber','Y-wavenumber',&
                       'time        '/),&
           units=(/'1','1','1','1','1'/),   &
           origin=real(time), interval=real(rst_intstep*delta_t), &
           xtypes=(/'real'/), history=hst_rst)

   !---- 座標変数定義, 出力 ----
    call HistoryPut('x',x_X, hst_rst)                       ! 変数出力
    call HistoryAddattr('x','topology','circular', hst_rst) ! 周期属性
    call HistoryAddattr('x','modulo',XLength, hst_rst)      ! 周期属性
    call HistoryPut('y',y_Y, hst_rst)                       ! 変数出力
    call HistoryAddattr('y','topology','circular', hst_rst) ! 周期属性
    call HistoryAddattr('y','modulo',YLength, hst_rst)      ! 周期属性

    call HistoryPut('k',(/(dble(k),k=-km,km)/), hst_rst)    ! 変数出力
    call HistoryPut('l',(/(dble(l),l=-lm,lm)/), hst_rst)    ! 変数出力

    call HistoryAddVariable( &                              ! 変数定義
           varname='x_weight', dims=(/'x'/), & 
           longname='Weight for integration in X', &
           units='1', xtype='double',history=hst_rst)
    call HistoryPut('x_weight',x_X_Weight,hst_rst)          ! 変数出力

    call HistoryAddVariable( &                              ! 変数定義
           varname='y_weight', dims=(/'y'/), & 
           longname='Weight for integration in Y', &
           units='1', xtype='double',history=hst_rst)
    call HistoryPut('y_weight',y_Y_Weight,hst_rst)          ! 変数出力


   !---- 物理変数定義 ----
    call HistoryAddVariable( &                              ! 変数定義
           varname='ee_vor', dims=(/'l','k','t'/), & 
           longname='Vorticity', &
           units='1', xtype='double', history=hst_rst)

   !---- 実験パラメターを属性として定義, 出力(全て Global 属性) ----
    call HistoryAddAttr('x','+delta_t', delta_t ,hst_rst)
    call HistoryAddAttr('x','+Beta',    Beta    ,hst_rst)
    call HistoryAddAttr('x','+HVOrder', HVOrder ,hst_rst)
    call HistoryAddAttr('x','+HVisc',   HVisc   ,hst_rst)

  end subroutine output_restart_init

 !
 ! リスタート出力
 !
  subroutine output_restart
    write(6,*) ' Restart file output at it = ',it, '  time = ', time
    call HistoryPut('t',real(time),hst_rst)

   !---- 物理変数出力 ----
    call HistoryPut('ee_vor', ee_Vor, hst_rst)
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

    call HistoryCreate( &
           file=trim(hst_file), &
           title=trim(title), &
           source='plbaro-beta_rn4exp_test3.f90 (2005/10/31)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'x','y','k','l','t'/), &
           dimsizes=(/im,jm,2*km+1,2*lm+1,0/),&
           longnames=(/'X           ','Y           ',&
                       'X-wavenumber','Y-wavenumber',&
                       'time        '/),&
           units=(/'1','1','1','1','1'/),   &
           origin=real(time), interval=real(rst_intstep*delta_t), &
           xtypes=(/'real'/))

   !---- 座標変数定義, 出力 ----
    call HistoryPut('x',x_X )                               ! 変数出力
    call HistoryAddattr('x','topology','circular')          ! 周期属性
    call HistoryAddattr('x','modulo',XLength )              ! 周期属性
    call HistoryPut('y',y_Y )                               ! 変数出力
    call HistoryAddattr('y','topology','circular')          ! 周期属性
    call HistoryAddattr('y','modulo',YLength )              ! 周期属性

    call HistoryPut('k',(/(dble(k),k=-km,km)/) )            ! 変数出力
    call HistoryPut('l',(/(dble(l),l=-lm,lm)/) )            ! 変数出力

    call HistoryAddVariable( &                              ! 変数定義
           varname='x_weight', dims=(/'x'/), & 
           longname='Weight for integration in X', &
           units='1', xtype='double')
    call HistoryPut('x_weight',x_X_Weight)                  ! 変数出力

    call HistoryAddVariable( &                              ! 変数定義
           varname='y_weight', dims=(/'y'/), & 
           longname='Weight for integration in Y', &
           units='1', xtype='double')
    call HistoryPut('y_weight',y_Y_Weight)                  ! 変数出力


   !---- パラメター定義, 出力 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='hvisc', dims=(/'l','k'/), & 
           longname='hyper diffusivity', units='1', xtype='double')

    call HistoryPut('hvisc',ee_HVisc)  ! 変数出力

   !---- 物理変数定義 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='vor', dims=(/'x','y','t'/), & 
           longname='Vorticity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='strfunc', dims=(/'x','y','t'/), & 
           longname='Stream function', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='velx', dims=(/'x','y','t'/), & 
           longname='x-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vely', dims=(/'x','y','t'/), & 
           longname='y-velocity', units='1', xtype='double')

    call HistoryAddVariable( &                                ! 変数定義
           varname='vor_analytic', dims=(/'x','y','t'/), & 
           longname='Vorticity(analytic)', units='1', xtype='double')

   !---- 診断量定義 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='ek', dims=(/'t'/), & 
           longname='mean kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='ens', dims=(/'t'/), & 
           longname='mean enstrophy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='ektot', dims=(/'t'/), & 
           longname='total kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='enstot', dims=(/'t'/), & 
           longname='total enstrophy', units='1', xtype='double')

    call HistoryAddVariable( &                                ! 変数定義
           varname='l2vor', dims=(/'t'/), & 
           longname='vorrticity error (type2)', &
           units='1', xtype='double')

  !---- 実験パラメターを属性として定義, 出力(全て Global 属性) ----
    call HistoryAddAttr('x','+delta_t', delta_t )
    call HistoryAddAttr('x','+Beta',    Beta    )
    call HistoryAddAttr('x','+HVOrder', HVOrder )
    call HistoryAddAttr('x','+HVisc',   HVisc   )

  end subroutine output_history_init

 !
 ! ヒストリー出力
 !
  subroutine output_history
    write(6,*) ' History file output at it = ',it, '  time = ', time
    call HistoryPut('t',real(time))

   !---- 物理変数出力 ----
    call set_VorMod( &
                    yx_VorAnalytic,     & !(out) モドン解渦度       
                    Radius,             & !(in)  モドン解半径       
                    Speed,              & !(in)  モドン解伝播速度   
                    XCenter+Speed*time, & !(in)  モドン解中心 X 座標
                    YCenter             & !(in)  モドン解中心 Y 座標
                   )
    yx_VorAnalytic = yx_ee(ee_yx(yx_VorAnalytic))

    yx_Vor = yx_ee(ee_Vor)
    yx_StrFunc = yx_ee(ee_StrFunc)
    yx_VelX = -yx_ee(ee_Dy_ee(ee_StrFunc))
    yx_VelY = yx_ee(ee_Dx_ee(ee_StrFunc))

    call HistoryPut('velx',transpose(yx_VelX))
    call HistoryPut('vely',transpose(yx_VelY))
    call HistoryPut('vor',transpose(yx_Vor))
    call HistoryPut('strfunc',transpose(yx_StrFunc))
    call HistoryPut('vor_analytic',transpose(yx_VorAnalytic))

   !---- 診断量出力 ----
    call HistoryPut('ek',  AvrYX_yx((yx_VelX**2+yx_VelY**2)/2.0d0))
    call HistoryPut('ens', AvrYX_yx(yx_Vor**2/2.0d0))
    call HistoryPut('ektot', IntYX_yx((yx_VelX**2+yx_VelY**2)/2.0d0))
    call HistoryPut('enstot', IntYX_yx(yx_Vor**2/2.0d0))

    call HistoryPut('l2vor', &
                    sqrt(AvrYX_yx((yx_Vor-yx_VorAnalytic)**2)&
                         /AvrYX_yx(yx_VorAnalytic**2)))

  end subroutine output_history

 !
 ! ヒストリー出力終了
 !
  subroutine output_history_close
    call HistoryClose
  end subroutine output_history_close

end program plbaro_beta_rn4exp_test3
