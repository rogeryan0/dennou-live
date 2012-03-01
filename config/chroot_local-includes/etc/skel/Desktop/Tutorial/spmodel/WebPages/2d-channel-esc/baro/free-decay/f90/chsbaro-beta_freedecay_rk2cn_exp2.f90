!--------------------------------------------------------------------------
!  Copyright (C) 2008 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
!
!Title(表題): Two-dimensional barotropic model on a channel region
!             (2 次元水路領域上の2次元順圧モデル)
!
!       The time integration is performed with the 2nd order 
!       2nd order Runge-Kutta scheme and Clank-Nicolson scheme. 
!       The beta parameter depends on y.
!       Program is organized for free-decay turbulence problem, exp2, 
!       The initial energy is concentrated in a band of total wavenumbers.
!
!History(履歴):
!   2008/10/13  竹広真一, plbaro より改造
!
! The governing equation(支配方程式): 
!
!      d\nabla^2\psi/dt 
!          = -J(\psi,\nabla^2\psi) - beta(y) d\psi/dx
!            +(-1)^{p+1}\nu_{2p}\nabla^2p \nabla^2\psi
!
!      \psi = d^2\psi/dy^2 = 0 at y=y_0,y_1
!
program chsbaro_beta_freedecay_rk2cn_exp2

!== モジュール引用宣言 ================================================
  use esc_module
  use gt4_history, only : GT_HISTORY, HistoryCreate, HistoryPut, HistoryGet, &
                          HistoryAddVariable, HistoryClose, HistoryAddAttr
  use dc_trace,    only : SetDebug, BeginSub, EndSub, DbgMessage
  use dc_message,  only : MessageNotify
  use dc_types
  use dc_string,   only : StoA
  use dc_args
  implicit none

!== 宣言部 ============================================================

 !---- 変数(格子点データ) ----
  real(8), allocatable :: yx_VelX(:,:)        ! 速度経度成分
  real(8), allocatable :: yx_VelY(:,:)        ! 速度緯度成分
  real(8), allocatable :: yx_Vor(:,:)         ! 渦度(鉛直成分)
  real(8), allocatable :: yx_StrFunc(:,:)     ! 流線関数

  real(8), allocatable :: yx_Beta(:,:)        ! βパラメター
  real(8), allocatable :: yx_FCor(:,:)        ! コリオリパラメター

 !---- 変数(スペクトルデータ) ----
  real(8), allocatable :: es_Vor(:,:)         ! 渦度(鉛直成分)
  real(8), allocatable :: es_StrFunc(:,:)     ! 流線関数

  real(8), allocatable :: es_Vortmp(:,:)      ! 渦度(t+Δt/2)

 !---- 固定パラメタ -----
  real(8), parameter  :: pi = 3.141592653589793D0 ! 円周率
  character(len=20)   :: DbgMessageFmt='*** DbgMESSAGE ***'
  real(8), parameter  :: vmiss = -999.0       ! 欠損値

 !---- NAMELIST 変数 ----
  ! コマンドライン引数用変数
  type(ARGS) :: arg
  character(STRING), pointer :: argv(:) => null()

  ! NAMELIST 入力用デフォルトファイル名
  character(len=100)  :: nmlfile='chsbaro-beta_freedecay_rk2cn_exp2.nml'

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
   'Test of linear terms of 2-dim barotropic model on a chanel domain'

  integer :: hst_intstep=200                     ! ヒストリー出力間隔ステップ数
  namelist /history/    hst_file, title, hst_intstep

  character(len=100) :: rst_file=''              ! リスタート出力ファイル名
  integer :: rst_intstep=200                     ! リスタート出力間隔ステップ数
  namelist /restart/   rst_file, rst_intstep 

 !---- 作業変数 ----
  real(8), allocatable :: es_HVisc(:,:)      ! 超粘性係数

  integer :: it=0                            ! 時間ステップ
  real(8) :: time                            ! モデル内時間
  integer :: k, l                            ! 波数

  type(GT_HISTORY) :: hst_rst                ! リスタート GT_HISTORY 変数

 !---------------- NAMELIST 読み込み ---------------------
  call Open(arg)
  call Debug(arg) ; call Help(arg) ; call Strict(arg)
  call Get(arg, argv)
  if ( size(argv) .le. 0 ) then
     call MessageNotify('W','chsbaro-beta_rk2cn_exp2',&
                            'There is no argument. The defalut namlist file, '&
                            //trim(nmlfile)//' is used.')
  else
     nmlfile=argv(1)
     call MessageNotify('M','chsbaro-beta_rk2cn_exp2',&
                            'Namlist file is '//trim(nmlfile))
  endif
  deallocate(argv)
  call Close(arg)

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
  allocate(yx_VelX(0:jm,0:im-1),yx_VelY(0:jm,0:im-1))
  allocate(yx_Vor(0:jm,0:im-1),yx_StrFunc(0:jm,0:im-1))
  allocate(yx_Beta(0:jm,0:im-1),yx_FCor(0:jm,0:im-1))

  allocate(es_Vor(-km:km,lm),es_StrFunc(-km:km,lm))
  allocate(es_Vortmp(-km:km,lm))
  allocate(es_HVisc(-km:km,lm))

 !------------------ 座標値の設定 -----------------------
  call DbgMessage(fmt='call %c', c1='esc_initial') 
  call esc_Initial(im,jm,km,lm,0.0D0,XLength,0.0D0,YLength)

 !------------------ 物理係数の設定 -----------------------
  es_Vor = 1.0D0
  es_HVisc = (-1)**HVOrder * HVisc*(es_Lapla_es(es_Vor))**HVOrder

!!$  yx_Beta = Beta * (sin(2*PI*yx_Y/YLength)+1)/2.0
  yx_Beta = Beta * sin(PI*yx_Y/YLength)
  yx_FCor = - Beta * YLength/PI * cos(PI*yx_Y/YLength)

 !------------------- 初期値設定 ----------------------
  time = initial_time

  if ( initial_file == "") then
     ! リスタートファイルを指定しない場合内部で es_Vor を与える. 
!!$     call set_initial_values
     call set_initial_values_real
  else
     !  初期値設定(リスタートファイルからの読みこみ)
     call HistoryGet( trim(initial_file), 'es_vor', es_Vor,  time )
  endif

  es_Strfunc = es_LaplaInv_es(es_Vor)

 !------------- 時間積分(Adams-Bashfort and Clank Nicolson 法) -----------
  call output_restart_init
  call output_history_init

  if ( initial_file == '' ) call output_history    ! 内部で与えた初期値は出力

  call DbgMessage(fmt='%c %c', &
&                 c1=DbgMessageFmt, &
&                 c2='Time integration starts.') 

  do it=1,nstep
     time = initial_time + it * delta_t

     ! 時間変化項の見積り

     ! 拡散以外の項を時間積分(Runge Kutta 1 段目)
     es_Vortmp = es_Vor &
          + delta_t/2 *( -es_Jacobian_es_es(es_StrFunc,es_Vor)  &
                        - es_yx(yx_Beta * yx_es(es_Dx_es(es_StrFunc))) )
     
     es_StrFunc = es_LaplaInv_es(es_Vortmp)

     ! 拡散以外の項を時間積分(Runge Kutta 2 段目)
     es_Vor = es_Vor &
          + delta_t *( -es_Jacobian_es_es(es_StrFunc,es_Vortmp)  &
                       -es_yx(yx_Beta * yx_es(es_Dx_es(es_StrFunc))) )

     ! 拡散項の時間積分計算(Clank-Nicolson scheme)
     es_Vor = ( 1.0d0 - 0.5*delta_t * es_HVisc )&
             /( 1.0d0 + 0.5*delta_t * es_HVisc )* es_Vor

     ! 渦度から流線関数を計算
     es_StrFunc = es_LaplaInv_es(es_Vor)

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
  subroutine set_initial_values    ! es_Vor を定める

   !---- 初期値全波数帯領域に乱数分布
    real(8)              :: Ntotal             ! 全波数
    integer              :: random_seed_size   ! 乱数の種の長さ
    integer, allocatable :: seedarray(:)       ! 乱数の種
    real                 :: harvest            ! 乱数の値

    integer    :: Seed=0           ! seed(1)に設定する種の値
    real(8)    :: Nmin=7.0D0       ! 初期エネルギー分布の全波数領域域最小値
    real(8)    :: Nmax=9.0D0       ! 初期エネルギー分布の全波数領域最大値
    real(8)    :: Etotal=1.0D0     ! 初期平均エネルギーの値

    namelist /initvalue/ Seed, Nmin, Nmax, Etotal

    if (verbose) write(6,nml=initvalue) 
    open(10,file=nmlfile,status='OLD')
    read(10,nml=initvalue) ; write(6,nml=initvalue) ; close(10)

    if ( Nmax .gt. km ) then
       call MessageNotify('E','set_initial_values',&
            'Nmax should be smaller equalt to  sqrt(km^2 + lm^2)' ) 
    endif
    if ( Nmin .lt. 0 .OR. Nmax .lt. 0 ) then
       call MessageNotify('E','set_initial_values',&
            'Nmin and Nmax should be larger equal to zero' ) 
    endif

    ! 乱数設定
    call random_seed(size=random_seed_size)
    allocate(seedarray(random_seed_size))
    call random_seed(get=seedarray)
    seedarray(1)=Seed
    call random_seed(put=seedarray)

    ! エネルギーを全波数 Ntotal が Nmin <= Ntotal <= Nmax にばらまく
    es_StrFunc = 0.0d0
    do k=-km,km
       do l=1,lm
          Ntotal = sqrt(k**2.0+l**2.0)
          if ( (Nmin .le. Ntotal) .AND. (Ntotal .le. Nmax) ) then
             call random_number(harvest)
             es_StrFunc(k,l) = 2.0 * harvest - 1     ! [-1,1] の一様乱数
          endif
       enddo
    enddo

    yx_VelX = -yx_ec(ec_Dy_es(es_StrFunc))
    yx_VelY = yx_es(es_Dx_es(es_StrFunc))
    es_StrFunc = es_StrFunc &
         * sqrt(Etotal/AvrYX_yx((yx_VelX**2+yx_VelY**2)/2.0d0))

    es_Vor = es_Lapla_es(es_StrFunc)

  end subroutine set_initial_values

  subroutine set_initial_values_real    ! es_Vor を定める

   !---- 初期値全波数帯領域に乱数分布
    real(8)              :: Ntotal             ! 全波数
    integer              :: random_seed_size   ! 乱数の種の長さ
    integer, allocatable :: seedarray(:)       ! 乱数の種
    real                 :: harvest            ! 乱数の値

    integer    :: Seed=0           ! seed(1)に設定する種の値
    real(8)    :: Nmin=7.0D0       ! 初期エネルギー分布の全波数領域域最小値
    real(8)    :: Nmax=9.0D0       ! 初期エネルギー分布の全波数領域最大値
    real(8)    :: Etotal=1.0D0     ! 初期平均エネルギーの値

    namelist /initvalue/ Seed, Nmin, Nmax, Etotal
    
    real(8)    :: kk, ll

    if (verbose) write(6,nml=initvalue) 
    open(10,file=nmlfile,status='OLD')
    read(10,nml=initvalue) ; write(6,nml=initvalue) ; close(10)

    if ( Nmax .gt. km ) then
       call MessageNotify('E','set_initial_values',&
            'Nmax should be smaller equalt to  sqrt(km^2 + lm^2)' ) 
    endif
    if ( Nmin .lt. 0 .OR. Nmax .lt. 0 ) then
       call MessageNotify('E','set_initial_values',&
            'Nmin and Nmax should be larger equal to zero' ) 
    endif

    ! 乱数設定
    call random_seed(size=random_seed_size)
    allocate(seedarray(random_seed_size))
    call random_seed(get=seedarray)
    seedarray(1)=Seed
    call random_seed(put=seedarray)

    ! エネルギーを全波数 Ntotal が Nmin <= Ntotal <= Nmax にばらまく
    yx_Strfunc = 0.0d0
    do k=-km,km
       do l=0,lm
          Ntotal = sqrt(k**2.0+l**2.0)
          if ( (Nmin .le. Ntotal) .AND. (Ntotal .le. Nmax) ) then
             call random_number(harvest)
             kk = 2*PI/XLength*k ; ll = PI/YLength*l
             yx_Strfunc= yx_Strfunc &
                  + cos(kk*yx_X + 2*PI*harvest ) &
                  * sin(ll*yx_Y)
          endif
       enddo
    enddo

   ! (平均)エネルギーを規格化
    es_Strfunc=es_yx(yx_Strfunc)
    yx_VelX = -yx_ec(ec_Dy_es(es_StrFunc))
    yx_VelY = yx_es(es_Dx_es(es_StrFunc))
    es_StrFunc = es_StrFunc &
         * sqrt(Etotal/AvrYX_yx((yx_VelX**2+yx_VelY**2)/2.0d0))

    es_Vor = es_Lapla_es(es_Strfunc)

  end subroutine set_initial_values_real

!=========================== リスタート出力 ============================
 !
 ! リスタート出力初期化
 !
  subroutine output_restart_init
    call HistoryCreate( &
           file=trim(rst_file), &
           title=trim(title), &
           source='plbaro-beta_freedecay_rk2cn_exp2.f90 (2008/10/13)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'x','y','k','l','t'/), &
           dimsizes=(/im,jm+1,2*km+1,lm,0/),&
           longnames=(/'X           ','Y           ',&
                       'X-wavenumber','Y-wavenumber',&
                       'time        '/),&
           units=(/'1','1','1','1','1'/),   &
           origin=real(time), interval=real(rst_intstep*delta_t), &
           history=hst_rst)

   !---- 座標変数定義, 出力 ----
    call HistoryPut('x',x_X, hst_rst)                       ! 変数出力
    call HistoryAddattr('x','topology','circular', hst_rst) ! 周期属性
    call HistoryAddattr('x','modulo',XLength, hst_rst)      ! 周期属性
    call HistoryPut('y',y_Y, hst_rst)                       ! 変数出力
    call HistoryAddattr('y','topology','circular', hst_rst) ! 周期属性
    call HistoryAddattr('y','modulo',YLength, hst_rst)      ! 周期属性

    call HistoryPut('k',(/(dble(k),k=-km,km)/), hst_rst)    ! 変数出力
    call HistoryPut('l',(/(dble(l),l=1,lm)/), hst_rst)    ! 変数出力

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
           varname='es_vor', dims=(/'l','k','t'/), & 
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
    call HistoryPut('es_vor', es_Vor, hst_rst)
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
           source='chsbaro-beta_freedecay_rk2cn_exp2.f90 (2008/10/13)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'x','y','k','l','t'/), &
           dimsizes=(/im,jm+1,2*km+1,lm,0/),&
           longnames=(/'X           ','Y           ',&
                       'X-wavenumber','Y-wavenumber',&
                       'time        '/),&
           units=(/'1','1','1','1','1'/),   &
           origin=real(time), interval=real(hst_intstep*delta_t))

   !---- 座標変数定義, 出力 ----
    call HistoryPut('x',x_X )                               ! 変数出力
    call HistoryAddattr('x','topology','circular')          ! 周期属性
    call HistoryAddattr('x','modulo',XLength )              ! 周期属性
    call HistoryPut('y',y_Y )                               ! 変数出力
    call HistoryAddattr('y','topology','circular')          ! 周期属性
    call HistoryAddattr('y','modulo',YLength )              ! 周期属性

    call HistoryPut('k',(/(dble(k),k=-km,km)/) )            ! 変数出力
    call HistoryPut('l',(/(dble(l),l=1,lm)/) )            ! 変数出力

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

    call HistoryPut('hvisc',es_HVisc)  ! 変数出力

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
           varname='pv', dims=(/'x','y','t'/), & 
           longname='potential vorticity', units='1', xtype='double')

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
    yx_Vor = yx_es(es_Vor)
    yx_StrFunc = yx_es(es_StrFunc)
    yx_VelX = -yx_ec(ec_Dy_es(es_StrFunc))
    yx_VelY = yx_es(es_Dx_es(es_StrFunc))

    call HistoryPut('velx',transpose(yx_VelX))
    call HistoryPut('vely',transpose(yx_VelY))
    call HistoryPut('vor',transpose(yx_Vor))
    call HistoryPut('strfunc',transpose(yx_StrFunc))

   !---- 診断量出力 ----
    call HistoryPut('ek',  AvrYX_yx((yx_VelX**2+yx_VelY**2)/2.0d0))
    call HistoryPut('ens', AvrYX_yx(yx_Vor**2/2.0d0))
    call HistoryPut('ektot', IntYX_yx((yx_VelX**2+yx_VelY**2)/2.0d0))
    call HistoryPut('enstot', IntYX_yx(yx_Vor**2/2.0d0))

    call HistoryPut('pv',transpose(yx_FCor+yx_Vor))

  end subroutine output_history

 !
 ! ヒストリー出力終了
 !
  subroutine output_history_close
    call HistoryClose
  end subroutine output_history_close

end program chsbaro_beta_freedecay_rk2cn_exp2
