!--------------------------------------------------------------------------
!  Copyright (C) 2005-2007 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
!
!Title(表題): Two-dimensional barotropic model on a rotating sphere. 
!             (回転球面上の2次元順圧モデル)
!
!       The time integration is performed with the Euler scheme. 
!       Program is organized for decaying tubulence problem. 
!
!History(履歴):  2005/03/16  S. Takehiro, created.
!                2007/12/07  S. Takehiro, namelist file is set from 
!                                         the command line argument. 
!                2008/02/11  S. Takehiro, beta coefficient fixed.
!                2008/02/13  S. Takehiro, Energy and Enstrophy output fixed.
!                2008/08/10  S. Takehiro, index changed, im -> 0:im-1
!
! The governing equation(支配方程式): 
!
!      d\nabla^2\psi/dt 
!          = -J(\psi,\nabla^2\psi)/a^2 - 2\Omega/a^2 d\psi/d\lambda 
!            +(-1)^{p+1}\nu_{2p}(\nabla^2 + 2/a^2)^p \nabla^2\psi
!
program spbaro_euler_freedecay

!== モジュール引用宣言 ================================================
  use w_module 
  use gt4_history, only : HistoryCreate, HistoryPut, HistoryGet, &
                          HistoryAddVariable, HistoryClose, HistoryAddAttr
  use dc_trace,    only : SetDebug, BeginSub, EndSub, DbgMessage
  use dc_message,  only : MessageNotify
  use dc_types
  use dc_string,   only : StoA
  use dc_args
  implicit none

!== 宣言部 ============================================================

 !---- 変数(格子点データ) ----
  real(8), allocatable :: xy_VelLon(:,:)      ! 速度経度成分
  real(8), allocatable :: xy_VelLat(:,:)      ! 速度緯度成分
  real(8), allocatable :: xy_Vor(:,:)         ! 渦度(鉛直成分)
  real(8), allocatable :: xy_StrFunc(:,:)     ! 流線関数

 !---- 変数(スペクトルデータ) ----
  real(8), allocatable :: w_Vor(:)            ! 渦度(鉛直成分)
  real(8), allocatable :: w_StrFunc(:)        ! 流線関数

  real(8), allocatable :: n_ESp(:)            ! エネルギースペクトル
  real(8), allocatable :: n_EnsSp(:)          ! エンストロフィースペクトル
 
 !---- 固定パラメタ -----
  real(8), parameter  :: pi = 3.141592653589793D0 ! 円周率
  character(len=20)   :: DbgMessageFmt='*** DbgMESSAGE ***  '

 !---- NAMELIST 変数 ----
  ! コマンドライン引数用変数
  type(ARGS) :: arg
  character(STRING), pointer :: argv(:) => null()

  ! NAMELIST 入力用ファイル名
  character(STRING)  :: nmlfile

  logical            :: Verbose=.false.          ! 出力メッセージレベル
  logical            :: DebugOn=.false.          ! デバッグ出力コントロール
  namelist /message/  Verbose, DebugOn           !
                                                 !-- 格子点・スペクトル --
  integer :: nm=21                               ! 切断全波数
  integer :: im=64                               ! 経度方向格子点数 (>3*nm+1)
  integer :: jm=32                               ! 緯度方向格子点数 (>3*nm/2)
  namelist /gridset/ nm, im, jm
                                                 !-- 物理パラメター --
  real(8)            :: Radius=1.0D0             ! 球の半径
  real(8)            :: Omega=0.0D0              ! 回転角速度
  integer            :: HVOrder=1                ! 超粘性の次数(1 で普通の拡散)
  real(8)            :: HVisc=1.0D0              ! 超粘性係数
  namelist /physics/  Radius, Omega, HVOrder, HVisc
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
       'Decaying turbulence of 2-dim barotropic fluid on a rotating sphere'
  character(len=100) :: source = &               ! ソース名
       'spbaro_euler_freedecay.f90 (2008/08/10)' !
  integer :: ndisp=200                           ! 出力間隔ステップ数
  namelist /output/    output_file, title, ndisp ! 

 !---- 作業変数 ----
  real(8) :: Beta=0.0d0                      ! βパラメター(2*Omega/radius)
  real(8), allocatable :: w_HVisc(:)         ! 超粘性係数
  real(8), allocatable :: n_Damptime(:)      ! 減衰時間(e-folding time)

  integer :: it=0                            ! 時間ステップ
  real(8) :: time                            ! モデル内時間
  integer :: n, m                            ! 全波数, 帯状波数

 !---------------- NAMELIST 読み込み ---------------------
  call MessageNotify('M','main', trim(source))

  call Open(arg)
  call Debug(arg) ; call Help(arg) ; call Strict(arg)
  call Get(arg, argv)
  if ( size(argv) .le. 0 ) then
     call MessageNotify('M','main',&
          'Usage: spbaro_euler_freedecay.out [namelist file]')
     call MessageNotify('E','main','There is no argument. &
          & Please set the namelist file name after the execution.')
  else
     nmlfile=argv(1)
     call MessageNotify('M','main','Namelist file is '//trim(nmlfile))
  endif
  deallocate(argv)
  call Close(arg)

  write(6,nml=message)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=message) ; write(6,nml=message) ; close(10)

  if(verbose)write(6,nml=gridset)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=gridset) ; write(6,nml=gridset) ; close(10)

  if(verbose)write(6,nml=physics)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=physics) ; write(6,nml=physics) ; close(10)

  if(verbose)write(6,nml=initial)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=initial) ; write(6,nml=initial) ; close(10)

  if(verbose)write(6,nml=tint) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=tint)    ; write(6,nml=tint)     ; close(10)

  if(verbose)write(6,nml=output)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=output)  ; write(6,nml=output)   ; close(10)

 !---------------- デバッグ出力制御設定 -----------------
  if (DebugOn) then
    call SetDebug
  end if

 !------------------ 変数の割り付け ---------------------
   allocate(xy_VelLon(0:im-1,jm),xy_VelLat(0:im-1,jm))
   allocate(xy_Vor(0:im-1,jm),xy_StrFunc(0:im-1,jm))

   allocate(w_Vor((nm+1)*(nm+1)),w_StrFunc((nm+1)*(nm+1)))
   allocate(w_HVisc((nm+1)*(nm+1)))

   allocate(n_ESp(0:nm),n_EnsSp(0:nm))
   allocate(n_Damptime(0:nm))

 !------------------ 座標値の設定 -----------------------
  call DbgMessage(fmt='call %c', c1='w_initial') 
  call w_Initial(nm,im,jm)

 !------------------ 物理係数の設定 -----------------------
  Beta  = 2 *Omega/Radius
  w_HVisc = (-1)**(HVOrder+1)*HVisc*((rn(:,1)+2.0D0)/Radius**2)**HVOrder
  w_HVisc(l_nm(0,0)) = 0.0d0   ! rn(ln(0,0,1) は正の値なので修正しておく.

  do n=0,nm
     if ( w_HVisc(l_nm(n,0)) .NE. 0.0D0 ) then
        n_Damptime(n)=-1.0D0/w_HVisc(l_nm(n,0))
     else
!        n_Damptime(n)=huge(0.0E0)              ! 一番大きい数を代入
        n_Damptime(n)=huge(0.0D0)              ! 一番大きい数を代入
     endif
  enddo

 !------------------- 初期値設定 ----------------------
  if ( initial_file == "") then
     call set_initial_values
  else
     call initial_read_gtool4
  endif

  time = initial_time
  w_Vor = w_xy(xy_Vor)
  w_StrFunc = w_xy(xy_StrFunc)

 !------------------- 時間積分(Euler 法) --------------------
  call output_gtool4_init
  if ( initial_file == '' ) call output_gtool4    ! 内部で与えた初期値は出力

  call DbgMessage(fmt='%c %c', &
&                 c1=DbgMessageFmt, &
&                 c2='Time integration starts.') 

  do it=1,nstep
     time = initial_time + it * delta_t

     w_Vor = w_Vor + delta_t *                          &
&         ( - w_Jacobian_w_w(w_StrFunc,w_Vor)/Radius**2 &
&           - Beta/Radius * w_DLon_w(w_StrFunc)         &
&           + w_HVisc * w_Vor )

     w_StrFunc = w_LaplaInv_w(w_Vor) * Radius**2

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo

  call DbgMessage(fmt='%c %c',      &
&                 c1=DbgMessageFmt, &
&                 c2='Time integration end.') 

  call output_gtool4_close

! 以上 メインプログラム 
!-----------------------------------------------------------------------------
! 以下 サブルーチン

contains

 !------------------- 初期値設定(ファイルなし) ----------------------
  subroutine set_initial_values

!!$   !---- 初期値剛体回転場
!!$    real(8), parameter :: Omega0   = 1.0D0
!!$    real(8), parameter :: LatOmega = 90.0D0/180.0D0*pi
!!$    real(8), parameter :: LonOmega = 0.0D0/180.0D0*pi
!!$
!!$    xy_StrFunc = Omega0 * Radius &
!!$                   * ( cos(LatOmega)*cos(xy_Lat)*cos(LonOmega-xy_Lon) &
!!$                       + sin(LatOmega)*sin(xy_Lat) )
!!$    w_StrFunc = w_xy(xy_StrFunc)
!!$    w_Vor = w_Lapla_w(w_StrFunc)/Radius**2
!!$    xy_Vor =  xy_w(w_Vor)
!!$
!!$   !---- 初期値 1 波数成分
!!$    integer, parameter :: ninit=4, minit=3
!!$
!!$    w_StrFunc = 0.0d0 ; w_StrFunc(l_nm(ninit,minit))=1.0d0
!!$    xy_StrFunc = xy_w(w_StrFunc)
!!$    w_Vor = w_Lapla_w(w_StrFunc)/Radius**2
!!$    xy_Vor =  xy_w(w_Vor)
   !---- 初期値乱数分布
    integer              :: random_seed_size   ! 乱数の種の長さ
    integer, allocatable :: seedarray(:)       ! 乱数の種
    real, allocatable    :: harvest(:)         ! 乱数の値

    integer    :: seed=0           ! seed(1)に設定する種の値
    integer    :: nzero=10         ! 初期エネルギースペクトル分布のパラメタ
    integer    :: gamma=100        ! 初期エネルギースペクトル分布のパラメタ
    real(8)    :: Etotal=1.0D0     ! 初期全エネルギーの値

    namelist /initvalue/ seed, nzero, gamma, Etotal

    if(verbose)write(6,nml=initvalue)
    open(10,file=nmlfile,status='OLD')
    read(10,nml=initvalue) ; write(6,nml=initvalue) ; close(10)

    ! 乱数設定
    call random_seed(size=random_seed_size)
    allocate(seedarray(random_seed_size))
    call random_seed(get=seedarray)
    seedarray(1)=seed
    call random_seed(put=seedarray)

    w_StrFunc = 0.0d0
    do n=1,nm
       allocate(harvest(-n:n))
       call random_number(harvest)
       w_StrFunc(l_nm(n,(/(m,m=-n,n)/)))=2.0 * harvest - 1 ! [-1,1] の一様乱数
       n_ESp(n) = 0.5 * n*(n+1)*sum(w_StrFunc(l_nm(n,(/(m,m=-n,n)/)))**2)
       deallocate(harvest)
    enddo

    ! スペクトル分布の形を決める
    do n=0,nm
       do m=-n,n
          if ( n_ESp(n) .ne. 0.0d0 ) then
             w_StrFunc(l_nm(n,m)) = w_StrFunc(l_nm(n,m)) &
                   * sqrt( (dble(n)**(gamma/2)/dble(n+nzero)**gamma) /n_ESp(n))
          endif
       enddo
    enddo

    ! エネルギースペクトル再計算
    do n=1,nm
       n_ESp(n) = 0.5 * n*(n+1)* sum( w_StrFunc(l_nm(n,(/(m,m=-n,n)/)))**2 )
    enddo

    ! エネルギーの大きさを定める. 
    w_StrFunc = w_StrFunc * sqrt(Etotal/sum(n_ESp))
    xy_StrFunc = xy_w(w_StrFunc)
    w_Vor = w_Lapla_w(w_StrFunc)/Radius**2
    xy_Vor =  xy_w(w_Vor)

  end subroutine set_initial_values

 !------------------- 初期値設定(ファイルから) ----------------------  
  subroutine initial_read_gtool4

    call HistoryGet( trim(initial_file), 'vellon', xy_VelLon,  time )
    call HistoryGet( trim(initial_file), 'vellat', xy_VelLat,  time )
    call HistoryGet( trim(initial_file), 'vor',    xy_Vor,     time )
    call HistoryGet( trim(initial_file), 'strfunc',xy_StrFunc, time )

  end subroutine initial_read_gtool4

 !------------------- 出力 ----------------------
  subroutine output_gtool4_init

   !---- ヒストリーファイル作成 ----
    call HistoryCreate( &
           file=trim(output_file), &
           title=trim(title), &
           source='spbaro_euler_freedecay.f90 (2008/08/10)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','n  ','t  '/), &
           dimsizes=(/im,jm,nm+1,0/),&
           longnames=(/'Longitude            ','Latitude             ',&
                       'Hor.total wave number','time                 '/),&
           units=(/'1','1','1','1'/),   &
           origin=real(time), interval=real(ndisp*delta_t), &
           xtypes=StoA('real','real','int','real'))

   !---- 座標変数定義, 出力 ----
    call HistoryPut('lon',x_Lon/pi*180)                       ! 変数出力
    call HistoryAddattr('lon','topology','circular')          ! 周期属性
    call HistoryAddattr('lon','modulo',360.0)                 ! 周期属性
    call HistoryPut('lat',y_Lat/pi*180)                       ! 変数出力
    call HistoryPut('n',(/(dble(n),n=0,nm)/))                 ! 変数出力

   !---- パラメター定義, 出力 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='hvisc', dims=(/'n'/), & 
           longname='hyper diffusivity', units='1', xtype='double')

    call HistoryAddVariable( &                                ! 変数定義
           varname='damptime', dims=(/'n'/), & 
           longname='dissipative damping time (e-folding)', &
           units='1', xtype='double')

    call HistoryPut('hvisc',w_HVisc(l_nm((/(n,n=0,nm)/),0)))  ! 変数出力
    call HistoryPut('damptime',n_Damptime)                    ! 変数出力

   !---- 物理変数定義 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='vor', dims=(/'lon','lat','t  '/), & 
           longname='Vorticity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='strfunc', dims=(/'lon','lat','t  '/), & 
           longname='Stream function', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vellon', dims=(/'lon','lat','t  '/), & 
           longname='lon-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='vellat', dims=(/'lon','lat','t  '/), & 
           longname='lat-velocity', units='1', xtype='double')

   !---- その他診断量定義 ----
    call HistoryAddVariable( &                                ! 変数定義
           varname='te', dims=(/'t  '/), & 
           longname='total energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='tens', dims=(/'t  '/), & 
           longname='total enstrophy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='mvlon', dims=(/'lat','t  '/), & 
           longname='mean zonal flow', units='1', xtype='double')

    call HistoryAddVariable( &                                ! 変数定義
           varname='esp', dims=(/'n','t'/), & 
           longname='energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='enssp', dims=(/'n','t'/), & 
           longname='enstrophy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='tesp', dims=(/'t'/), & 
           longname='Total energy (by spectrum)', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='tenssp', dims=(/'t'/), & 
           longname='Total enstrophy (by spectrum)', units='1', xtype='double')

   !---- 実験パラメターを属性として定義, 出力(全て Global 属性) ----
    call HistoryAddAttr('lon','+Radius',  Radius  )
    call HistoryAddAttr('lon','+delta_t', delta_t )
    call HistoryAddAttr('lon','+Omega',   Omega   )
    call HistoryAddAttr('lon','+HVOrder', HVOrder )
    call HistoryAddAttr('lon','+HVisc',   HVisc   )

  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it, '  time = ', time
    call HistoryPut('t',real(time))

   !---- 物理変数出力 ----
    xy_Vor = xy_w(w_Vor)
    xy_StrFunc = xy_w(w_StrFunc)
    xy_VelLon = -xy_GradLat_w(w_StrFunc)/Radius
    xy_VelLat = xy_GradLon_w(w_StrFunc)/Radius

    call HistoryPut('vellon',xy_VelLon)
    call HistoryPut('vellat',xy_VelLat)
    call HistoryPut('vor',xy_Vor)
    call HistoryPut('strfunc',xy_StrFunc)

    call HistoryPut('te', &
         IntLonLat_xy((xy_VelLon**2+xy_VelLat**2)/2.0d0)*Radius**2)
    call HistoryPut('tens',&
         IntLonLat_xy(xy_Vor**2/2.0d0)*Radius**2)
    call HistoryPut('mvlon',y_IntLon_xy(xy_VelLon))

    do n=0,nm
       n_ESp(n) = 0.5 * n*(n+1) * sum(w_StrFunc(l_nm(n,(/(m,m=-n,n)/)))**2 )
       n_EnsSp(n) =0.5 * n**2 * (n+1)**2 &
            * sum(w_StrFunc(l_nm(n,(/(m,m=-n,n)/)))**2)
    enddo
    call HistoryPut('esp',n_Esp/Radius**2)
    call HistoryPut('enssp',n_Enssp/Radius**2)
    call HistoryPut('tesp',(4*pi)*sum(n_Esp))
    call HistoryPut('tenssp',(4*pi/Radius**2)*sum(n_Enssp))

  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program spbaro_euler_freedecay
