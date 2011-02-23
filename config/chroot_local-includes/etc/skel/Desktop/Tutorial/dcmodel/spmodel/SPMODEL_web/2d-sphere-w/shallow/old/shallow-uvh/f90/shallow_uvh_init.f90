!----------------------------------------------------------------------
!     Copyright (c) 2003 Masatsugu Odaka. All rights reserved.
!----------------------------------------------------------------------
! Sample program for gt4_history/gt4f90io and ISPACK  2004/01/26 M. Odaka
! 
! Making initial condition for solving a non-linear 2-D shallow water 
! system on a sphere. 
!     
!     du/dt - 2\Omega\sin\phi v = -g/a\cos\phi dh/d\lambda - \nu\lapla^4 u,
!     dv/dt + 2\Omega\sin\phi u = -g/a  dh/d\phi - \nu\lapla^4 v,
!     dH/dt + ( 1/\cos\phi d([H-hs] u)/d\lambda 
!               + 1/\cos\phi d([H-hs] v\cos\phi)/d\phi ) = - \nu\lapla^4 h.
!
! where hs is hight of topography.
!
! The initial conditions are similar to those of Williamson et al. (1992) 
! [J. Comp. Phys., 102, 211-224]: 
!   case 1: advection of cosine bell over Pole
!   case 2: global steady state nonlinear zonal geostrophic flow
!   case 3: steady state nonlinear zonal geostrophic flow with copact support
!   case 5: zonal flow over an isolated mountain
!   case 6: Rossby-Haurwitz wave
!
  program shallow_uvh_init

    use w_module
    use gt4_history
    implicit none

  !---- 空間解像度設定 ----
!!$    integer, parameter :: im=512, jm=256            ! 格子点の設定(X,Y)
!!$    integer, parameter :: nm=170
!!$    integer, parameter :: im=192, jm=96             ! 格子点の設定(X,Y)
!!$    integer, parameter :: nm=63
!!$    integer, parameter :: im=128, jm=64             ! 格子点の設定(X,Y)
!!$    integer, parameter :: nm=42
!!$    integer, parameter :: im=96, jm=48              ! 格子点の設定(X,Y)
!!$    integer, parameter :: nm=31
    integer, parameter :: im=64, jm=32             ! 格子点の設定(X,Y)
    integer, parameter :: nm=21

  !---- 変数 ----
    real(8)            :: xy_U(im,jm)              ! 格子点データ(速度経度成分)
    real(8)            :: xy_V(im,jm)              ! 格子点データ(速度緯度成分)
    real(8)            :: xy_H(im,jm)              ! 格子点データ(変位)
    real(8)            :: xy_Zeta(im,jm)           ! 格子点データ(渦度)
    real(8)            :: xy_D(im,jm)              ! 格子点データ(発散)
    real(8)            :: xy_Psi(im,jm)            ! 格子点データ(流線関数)
    real(8)            :: xy_Topo(im,jm)           ! 格子点データ(地形)

    real(8)            :: w_H((nm+1)*(nm+1))       ! スペクトル(変位 t)
    real(8)            :: w_Zeta((nm+1)*(nm+1))    ! スペクトル(渦度 t)
    real(8)            :: w_D((nm+1)*(nm+1))       ! スペクトル(発散 t)
    real(8)            :: w_U((nm+1)*(nm+1))       ! スペクトル(速度経度成分)
    real(8)            :: w_V((nm+1)*(nm+1))       ! スペクトル(速度緯度成分)
    real(8)            :: w_Psi((nm+1)*(nm+1))     ! スペクトル(流線関数)
    real(8)            :: w_Chi((nm+1)*(nm+1))     ! スペクトル(ポテンシャル)

  !---- 物理パラメター ----
    real(8), parameter :: pi = 3.141592653589793   ! 円周率
    real(8)            :: R0                       ! 球の半径
    real(8)            :: Omega                    ! 回転角速度
    real(8)            :: Grav                     ! 重力加速度
    real(8)            :: H0                       ! 平均変位
    namelist /nmpcon/ R0, Omega, Grav, H0

  !---- 初期条件パラメター (case1 & case5) ---- 
    real(8), parameter :: alpha = 0.0              ! 流れの軸の傾き
!    real(8), parameter :: alpha = 0.05             ! 流れの軸の傾き
!    real(8), parameter :: alpha = pi * 0.25        ! 流れの軸の傾き
!    real(8), parameter :: alpha = pi * 0.5 - 0.05  ! 流れの軸の傾き
!    real(8), parameter :: alpha = pi * 0.5         ! 流れの軸の傾き
    real(8), parameter :: T0 = 2.0D3               ! 山頂高度
    real(8), parameter :: Lat_topo = pi/6.0        ! 山頂緯度
    real(8), parameter :: Lon_topo = 0.5*pi        ! 山頂経度
    real(8), parameter :: R_topo   = pi/9.0        ! 地形の半径

  !---- 初期条件パラメター (case3) ---- 
    real(8), parameter :: Lat_b = - pi/6.0         ! 
    real(8), parameter :: Lat_e = 0.5*pi           ! 
    real(8), parameter :: Xe0   = 0.3D0

  !---- 初期条件パラメター (case6) ----
    real(8), parameter :: Omega0 = 7.848D-6        ! 初期角速度
    real(8), parameter :: Wnum = 4.0D0             ! 初期波数

  !---- 初期条件変数 ---- 
    real(8)            :: U0                       ! 初期速度

  !---- 作業変数 ----
    integer            :: i,j 
    real(8)            :: gamma 
    real(8),parameter  :: tinit = 0.0              ! 初期時刻
    real(8)            :: Xe                       ! case 3 初期値計算用座標

  !----- NAMELIST 変数 ----
    character(len=100) :: run=''                   ! 実験名
    character(len=100) :: sign=''                  ! 実行者名
    namelist /nmrun/  run, sign
    !----------------------
    character(len=100) :: model_name=''            ! モデル名
    character(len=10)  :: model=''                 ! モデル設定名
    namelist /nmodel/ model_name, model 
    !---------------------
    character(len=100) :: exp_case=''              ! 初期値設定名
    namelist /nmcase/ exp_case

    character(len=100) :: output_file='shallow_uvh_init.nc' ! 初期値ファイル名

  !------------------- 座標値の設定 --------------------
    write(*,*) "call w_initial"
    call w_initial(nm,im,jm)                       ! ISPACK初期化

  !------------------- パラメータ読み込み ---------------
    read(5,nml=nmrun)  ; write(6,nml=nmrun)     
    read(5,nml=nmodel) ; write(6,nml=nmodel)
    read(5,nml=nmpcon) 
    read(5,nml=nmcase) ; write(6,nml=nmcase)

  !------------------- 初期値設定 ----------------------

    select case (exp_case)

  !------------------- case 1: cos 型 bell の移留 ----------------------
    case ('case1')                                 ! 
      write(6,*) 'test case 1: advection of cosine bell over Pole'

      H0 = 1.0D3                                   ! 平均変移
      U0 = 2*pi*R0/(12*86400.0)                    ! 初期速度

      write(6,nml=nmpcon)

      xy_Topo = 0.0 

      xy_Psi = - U0*R0*(sin(xy_Lat)*cos(alpha) -               &
&                         cos(xy_Lon)*cos(xy_Lat)*sin(alpha))  

      do j=1,jm
        do i=1,im
          gamma =  R0*acos(cos(xy_Lat(i,j))*cos(xy_Lon(i,j)-1.5*pi))
          xy_H(i,j) = 0.0
          if ( gamma < R0/3 ) then 
            xy_H(i,j) = 0.5*H0*(1.0 + cos(3*pi*gamma/R0))
          endif
        enddo
      enddo

  !------------------- case 2: 非線形帯状地衡流 1 ----------------------
    case ('case2')                                 
      write(6,*) 'test case 2: global steady state nonlinear zonal geostrophic flow'

      H0 = 2.94D4/Grav                           ! 平均変移
      U0 = 2*pi*R0/(12*86400.0)                    ! 初期速度

      write(6,nml=nmpcon)

      xy_Topo = 0.0

      xy_Psi = - R0*U0*sin(xy_Lat)
      
      xy_H = H0 - (R0*Omega*U0 + 0.5*U0**2)*sin(xy_Lat)**2/Grav

  !------------------- case 3: 非線形帯状地衡流 2 ----------------------
    case ('case3')                                 
      write(6,*) 'test case 3: steady state nonlinear zonal geostrophic flow with copact support'

      H0 = 2.94D4/Grav                           ! 平均変移
      U0 = 2*pi*R0/(12*86400.0)                    ! 初期速度

      write(6,nml=nmpcon)

      xy_Topo = 0.0

      xy_U(:,jm) = 0.0

      do i = 1, im
      do j = jm-1, 1, -1
        xy_U(i,j) = 0.0D0
        Xe = Xe0*(xy_Lat(i,j) - Lat_b)/(Lat_e - Lat_b)
        if (Xe .gt. 0.0) then
          xy_U(i,j) = U0*exp(- Xe0/(Xe*(Xe0 - Xe)))*exp(4.0/Xe0)
        end if   
      end do
      end do

      xy_Psi(:,1) = 0.0
      do j = 2, jm
        xy_Psi(:,j) = xy_Psi(:,j-1) - R0*0.5*                    &
&                       (xy_U(:,j) + xy_U(:,j-1))*               &
&                       (xy_Lat(:,j) - xy_Lat(:,j-1)) 
      end do

      xy_H(:,1) = H0
      do j = 2, jm
        xy_H(:,j) = xy_H(:,j-1) - R0/Grav*0.5*                       &
&                     ((2*Omega*sin(xy_Lat(:,j))                     &
&                         + xy_U(:,j)*tan(xy_Lat(:,j))/R0)*xy_U(:,j) &
&                    + (2*Omega*sin(xy_Lat(:,j-1))                   &
&                         + xy_U(:,j-1)*tan(xy_Lat(:,j-1))/R0)*xy_U(:,j-1)) &
&                    *(xy_Lat(:,j) - xy_Lat(:,j-1)) 
      end do

  !------------------- case 5: 山岳応答問題 ----------------------
    case ('case5')                          
      write(6,*) 'test case 5: zonal flow over an isolated mountain'

      U0 = 20.0D0                                  ! 初期速度
      H0 = 5.96D3                                  ! 平均変位

      write(6,nml=nmpcon)

      xy_Topo = T0*(1.0 - sqrt(min(R_topo**2,                      &
&                                    (xy_Lon-Lon_topo)**2 +        &
&                                    (xy_Lat-Lat_topo)**2))/R_topo)

      xy_Psi = - U0*R0*(sin(xy_Lat)*cos(alpha) -                   &
&                         cos(xy_Lon)*cos(xy_Lat)*sin(alpha))  

      xy_H  = H0 - (R0*Omega*U0 + 0.5*U0**2)/Grav*                 &
&                     (- cos(xy_Lon)*cos(xy_Lat)*sin(alpha)        &
&                      + sin(xy_Lat)*cos(alpha))**2 

  !------------------- case 6: Rossby-Haurwitz 波  ----------------------
    case ('case6')                                 
      write(6,*) 'test case 6: Rossby-Haurwitz wave'

      U0 = 0.0                                     ! 初期風速
      H0 = 8.0D3                                   ! 平均変位

      write(6,nml=nmpcon)

      xy_Topo = 0.0 

      xy_Psi = - R0**2*Omega0*sin(xy_Lat)*(                         &
&                 1.0 - cos(xy_Lat)**Wnum*cos(xy_Lon*Wnum))     

      xy_H = H0 + R0**2/Grav*                                       &
&          (0.5*Omega0*(2*Omega + Omega0)*cos(xy_Lat)**2            &
&           + 0.25*Omega0**2*cos(xy_Lat)**(2*Wnum)*                 &
&               ((Wnum + 1)*cos(xy_Lat)**2                          &
&                + (2*Wnum**2 - Wnum - 2)                           &
&                - 2*Wnum**2*cos(xy_Lat)**(-2))                     &
&           + (2*(Omega + Omega0)*Omega0                            &   
&               /(Wnum + 1)/(Wnum +2)*cos(xy_Lat)**Wnum             & 
&               *((Wnum**2 + 2*Wnum + 2)                            &
&                 - (Wnum + 1)**2*cos(xy_Lat)**2))*cos(xy_lon*Wnum) &
&           + 0.25* Omega0**2*cos(xy_Lat)**(2*Wnum)                 & 
&               *((Wnum + 1)*cos(xy_Lat)**2 - (Wnum + 2))*cos(2*xy_lon*Wnum))

    case default
      write(6,*) 'invalid case number; program is forcedly terminated.' 
      stop
    end select

    w_Psi = w_xy(xy_Psi)
    w_Chi = 0.0

    w_H   = w_xy(xy_H)               
    w_Zeta= w_Lapla_w(w_Psi) / R0**2 
    w_D   = w_Lapla_w(w_Chi) / R0**2 

    w_U   = w_xy(xy_GradLon_w(w_Chi) - xy_GradLat_w(w_Psi)) / R0
    w_V   = w_xy(xy_GradLon_w(w_Psi) + xy_GradLat_w(w_Chi)) / R0

    call output_gtool4_init                        ! ヒストリー初期化
    call output_gtool4                             ! 初期値出力
    call output_gtool4_close                       ! ヒストリー終了

  contains
 
    subroutine output_gtool4_init
      call HistoryCreate( &                               ! ヒストリー作成
           file=trim(output_file),                                & 
&          title=trim(run),                                       &
&          source=trim(model_name),                               &
&          institution=trim(sign),                                &
&          dims=(/'lon','lat','t  '/), dimsizes=(/im,jm,0/),      &
&          longnames=(/'longitude','latitude ','time     '/),     &
&          units=(/'deg.','deg.','sec.'/),                        &
&          origin=real(tinit), interval=real(0.0) )

      call HistoryPut('lon',x_Lon*180/pi)                 ! 変数出力
      call HistoryPut('lat',y_Lat*180/pi)                 ! 変数出力

      call HistoryAddVariable( &                          ! 変数定義
&           varname='topo', dims=(/'lon','lat'/), & 
&           longname='Topography', units='m', xtype='double')

      call HistoryPut('topo',xy_Topo)

      call HistoryAddVariable( &                          ! 変数定義
&          varname='h', dims=(/'lon','lat','t  '/), & 
&          longname='surface displacement ', units='m', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
&          varname='u', dims=(/'lon','lat','t  '/), & 
&          longname='velocity(longitude) ', units='m/s', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
&          varname='v', dims=(/'lon','lat','t  '/), & 
&          longname='velocity(latitude) ', units='m/s', xtype='double')

      call HistoryAddVariable( &                          ! 変数定義
&          varname='zeta', dims=(/'lon','lat','t  '/), & 
&          longname='vorticity', units='1/s', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
&          varname='div', dims=(/'lon','lat','t  '/), & 
&          longname='divergence', units='1/s', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4
      xy_U    = xy_w(w_U)
      xy_V    = xy_w(w_V)
      xy_H    = xy_w(w_H)
      xy_Zeta = xy_w(w_Zeta)
      xy_D    = xy_w(w_D)

      call HistoryPut('t',real(tinit))
      call HistoryPut('u',xy_U)
      call HistoryPut('v',xy_V)
      call HistoryPut('h',xy_H)
      call HistoryPut('zeta',xy_Zeta)
      call HistoryPut('div',xy_D)
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program shallow_uvh_init

