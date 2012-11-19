!--------------------------------------------------------------------------
!  Copyright (C) 2005 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
!
!Title(表題): Two-dimensional barotropic model on a double cyclic region
!             (2 重周期境界領域上の2次元順圧モデル)
!
!       The time integration is performed with the 1st order Euler scheme. 
!       The beta parameter is constant. 
!       Program is organized for test1 (test for advection term). 
!
!History(履歴):
!   2005/10/15  竹広真一, 新規作成
!   2005/10/31  竹広真一, k,l 次元出力 bug fix
!
! The governing equation(支配方程式): 
!
!      d\nabla^2\psi/dt 
!          = -J(\psi,\nabla^2\psi) - beta d\psi/dx
!            +(-1)^{p+1}\nu_{2p}\nabla^2p \nabla^2\psi
!
program plbaro_beta_euler_test1

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
  real(8), allocatable :: yx_VorBasic(:,:)    ! 一般流渦度

 !---- 変数(スペクトルデータ) ----
  real(8), allocatable :: ee_Vor(:,:)         ! 渦度(鉛直成分)
  real(8), allocatable :: ee_StrFunc(:,:)     ! 流線関数

  real(8), allocatable :: ee_DVorDt(:,:)      ! 渦度時間変化(t)

 !---- 固定パラメタ -----
  real(8), parameter  :: pi = 3.141592653589793D0 ! 円周率
  character(len=20)   :: DbgMessageFmt='*** DbgMESSAGE ***'
  real(8), parameter  :: vmiss = -999.0       ! 欠損値

 !---- NAMELIST 変数 ----
  ! NAMELIST 入力用テンポラリファイル名(名前変更禁止)
  character(len=30),parameter  :: nmlfile='plbaro-beta_euler_test2.nml'

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
   'Test of advection term of 2-dim barotropic model on a double-cyclic domain'

  integer :: hst_intstep=200                     ! ヒストリー出力間隔ステップ数
  namelist /history/    hst_file, title, hst_intstep

  character(len=100) :: rst_file=''              ! リスタート出力ファイル名
  integer :: rst_intstep=200                     ! リスタート出力間隔ステップ数
  namelist /restart/   rst_file, rst_intstep 

 !---- NAMELIST 変数(初期値用) ----
                                          !---- 平行流 + ガウス型渦
  real(8) :: U0    = 1.0D0                ! 平行流振幅
  real(8) :: Sigma = 1.0D-2               ! 平行流境界層幅
  namelist /initflow/ U0, Sigma

  real(8) :: VorAmplitude = 1.0D0         ! 渦の振幅(流線関数の値)
  real(8) :: VorRadius    = 1.0D-2        ! 渦半径
  real(8) :: VorX0        = 0.5D0         ! 渦の初期位置 X 座標
  real(8) :: VorY0        = 0.5D0         ! 渦の初期位置 X 座標
  namelist /initvor/ VorAmplitude, VorRadius, VorX0, VorY0

 !---- 作業変数 ----
  real(8), allocatable :: ee_HVisc(:,:)      ! 超粘性係数

  integer :: it=0                            ! 時間ステップ
  real(8) :: time                            ! モデル内時間
  integer :: k, l                            ! 波数

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

 !---------------- デバッグ出力制御設定 -----------------
  if (DebugOn) then
    call SetDebug
  end if

 !------------------ 変数の割り付け ---------------------
  allocate(yx_VelX(0:jm-1,0:im-1),yx_VelY(0:jm-1,0:im-1))
  allocate(yx_Vor(0:jm-1,0:im-1),yx_StrFunc(0:jm-1,0:im-1))
  allocate(yx_VorAnalytic(0:jm-1,0:im-1),yx_VorBasic(0:jm-1,0:im-1))

  allocate(ee_Vor(-lm:lm,-km:km),ee_StrFunc(-lm:lm,-km:km))
  allocate(ee_DVorDt(-lm:lm,-km:km))
  allocate(ee_HVisc(-lm:lm,-km:km))

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

     if (verbose) write(6,nml=initflow) 
     open(10,file=nmlfile,status='OLD')
     read(10,nml=initflow) ; write(6,nml=initflow) ; close(10)

     if (verbose) write(6,nml=initvor) 
     open(10,file=nmlfile,status='OLD')
     read(10,nml=initvor) ; write(6,nml=initvor) ; close(10)

     call set_initial_values
  else
     !  初期値設定(リスタートファイルからの読みこみ)
     call HistoryGet( trim(initial_file), 'ee_vor', ee_Vor,  time )
  endif

  ee_Strfunc = ee_LaplaInv_ee(ee_Vor)


 !------------------- 時間積分(Euler 法) --------------------
  call output_restart_init
  call output_history_init

  if ( initial_file == '' ) call output_history    ! 内部で与えた初期値は出力

  call DbgMessage(fmt='%c %c', &
&                 c1=DbgMessageFmt, &
&                 c2='Time integration starts.') 

  do it=1,nstep
     time = initial_time + it * delta_t

     ! 時間変化項の見積り
     ee_DVorDt = - ee_Jacobian_ee_ee(ee_StrFunc,ee_Vor)  &
                 - Beta * ee_Dx_ee(ee_StrFunc)           &
                 - ee_HVisc * ee_Vor

     ! 時間積分計算(Euler scheme)
     ee_Vor = ee_Vor + delta_t * ee_DVorDt

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

!=========================== 初期値設定 ============================
 !
 !  初期値設定(リスタートファイルない場合のデフォルト設定)
 !
  subroutine set_initial_values    ! w_Vor を定める

    !--- 平行流設定
    yx_StrFunc(0:jm/2-1,:)  &
         = -U0 * Sigma * ( log(cosh((yx_Y(0:jm/2-1,:)-YLength/4)/Sigma)) &
                                - log(cosh((YLength/4)/Sigma)) )
    yx_StrFunc(jm/2:jm-1,:) &
         =  U0 * Sigma * ( log(cosh((yx_Y(jm/2:jm-1,:)-3*YLength/4)/Sigma)) &
                                - log(cosh((YLength/4)/Sigma)) )

    !--- 一般流を保存
    yx_VorBasic = yx_ee(ee_Lapla_ee(ee_yx(yx_StrFunc)))

    !--- 局所渦を重ねる
    yx_Vor = yx_VorBasic  &
!!$        + VorAmplitude *( exp ( -((yx_X-VorX0)**2 + (yx_Y-VorY0)**2)  &
!!$                                   /VorRadius**2) - pi*VorRadius**2  )
        + VorAmplitude * (1.0D0 - (yx_X-VorX0)**2 + (yx_Y-VorY0)**2 ) &
                       * ( exp ( -((yx_X-VorX0)**2 + (yx_Y-VorY0)**2) &
                                   /VorRadius**2) )
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
           source='plbaro-beta_euler_test1.f90 (2005/10/31)', &
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
           source='plbaro-beta_euler_test1.f90 (2005/10/31)', &
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
    yx_Vor = yx_ee(ee_Vor)
    yx_StrFunc = yx_ee(ee_StrFunc)
    yx_VelX = -yx_ee(ee_Dy_ee(ee_StrFunc))
    yx_VelY = yx_ee(ee_Dx_ee(ee_StrFunc))

    yx_VorAnalytic = yx_VorBasic &
!!$         + VorAmplitude &
!!$         *( exp ( -((yx_X-mod(VorX0+U0*time,XLength))**2 &
!!$                     + (yx_Y-VorY0)**2) &
!!$              /VorRadius**2) - pi*VorRadius**2  )
        + VorAmplitude &
           * (1.0D0 - (yx_X-mod(VorX0+U0*time,XLength))**2    &
                       + (yx_Y-VorY0)**2 )                    &
           * ( exp ( -((yx_X-mod(VorX0+U0*time,XLength))**2   &
                        + (yx_Y-VorY0)**2) &
                       /VorRadius**2) )
  
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

end program plbaro_beta_euler_test1

