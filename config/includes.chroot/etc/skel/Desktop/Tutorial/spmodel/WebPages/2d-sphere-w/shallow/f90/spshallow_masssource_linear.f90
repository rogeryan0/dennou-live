!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
! Sample program for gt4_history/gt4f90io and ISPACK   2002/08/22 S.Takehiro
!                                                      2004/01/26 M.Odaka
!                                                      2008/08/10 S.Takehiro
! 
! Solving a linear 2-D shallow water system on a sphere 
!     with an isolated mass source
!     du/dt - 2\Omega\sin\phi v = -g/a\cos\phi dh/d\lambda - \nu\lapla^4 u,
!     dv/dt + 2\Omega\sin\phi u  =  -g/a  dh/d\phi - \nu\lapla^4 v,
!     dh/dt + ( 1/\cos\phi d(H u)/d\lambda 
!               + 1/\cos\phi d(H v\cos\phi)/d\phi ) = Q - \nu\lapla^4 h.
!
!
  program shallow_masssource_linear

    use w_module
    use gt4_history
    implicit none

  !---- 空間解像度設定 ----
!!$    integer, parameter :: im=512, jm=256             ! 格子点の設定(X,Y)
!!$    integer, parameter :: nm=170
!!$    integer, parameter :: im=128, jm=64             ! 格子点の設定(X,Y)
!!$    integer, parameter :: nm=42
!!$    integer, parameter :: im=96, jm=48             ! 格子点の設定(X,Y)
!!$    integer, parameter :: nm=31
    integer, parameter :: im=64, jm=32             ! 格子点の設定(X,Y)
    integer, parameter :: nm=21

  !---- 変数 ----
    real(8)            :: xy_U(0:im-1,jm)          ! 格子点データ(速度経度成分)
    real(8)            :: xy_V(0:im-1,jm)          ! 格子点データ(速度緯度成分)
    real(8)            :: xy_H(0:im-1,jm)          ! 格子点データ(変位)

    real(8)            :: w_U((nm+1)*(nm+1))       ! スペクトル(速度経度成分)
    real(8)            :: w_V((nm+1)*(nm+1))       ! スペクトル(速度緯度成分)
    real(8)            :: w_H((nm+1)*(nm+1))       ! スペクトル(変位)

    real(8)            :: xy_U0(0:im-1,jm)         ! 格子点データ(速度経度成分)
    real(8)            :: xy_H0(0:im-1,jm)         ! 格子点データ(変位)
    real(8)            :: w_U0((nm+1)*(nm+1))      ! スペクトル(速度経度成分)
    real(8)            :: w_H0((nm+1)*(nm+1))      ! スペクトル(変位)

    real(8)            :: xy_Zeta(0:im-1,jm)       ! 格子点データ(渦度)
    real(8)            :: xy_Q(0:im-1,jm)          ! 格子点データ(熱源)

  !---- 時間積分パラメター ----
    real(8), parameter :: dt = 200                  ! 時間ステップ間隔
    integer, parameter :: nt = 30000, ndisp = 500   ! 時間積分数, 表示ステップ
    real(8)            :: t                         ! 時間
 
  !---- 物理パラメター ----
    real(8), parameter :: pi = 3.141592653589793   ! 円周率
    real(8), parameter :: R0 = 6.371D6             ! 球の半径
    real(8), parameter :: H0 = 10D3                ! 平均変位
    real(8), parameter :: Omega = 7.292D-5         ! 回転角速度
    real(8), parameter :: Grav = 9.8               ! 重力加速度

    real(8), parameter :: U0   = 15                ! 平均流
    real(8), parameter :: Lon0 = pi, Lat0=0        ! 質量源の中心位置
    real(8), parameter :: W0 = 8                   ! 質量源の広がりの逆数
    real(8), parameter :: Q0 = -1.0e-3             ! 質量源の強さ
    real(8), parameter :: Ta = 24*3600             ! 質量源の強さの立上り時間

    integer, parameter :: ndiff=4                  ! 超粘性の次数
    real(8), parameter :: nu=2.338D16              ! 超粘性係数

  !------ 作業変数 ------
    integer            :: i, j, it=0
    real(8)            :: Delta

  !---------------- 座標値の設定 ---------------------
    call w_initial(nm,im,jm)                ! ISPACK初期化

  !------------------- 初期値設定 ----------------------
    !xy_U0  = U0*cos(xy_Lat)
    !xy_H0  = ( Omega*R0*U0/(2*Grav) + U0**2/(4*Grav) )*cos(2*xy_Lat)

    xy_U0  = 0
    xy_H0  = 0

    xy_U  = 0 ; xy_V  = 0 ; xy_H  = 0

    w_U0 = w_xy(xy_U0) ; w_H0 = w_xy(xy_H0)
    w_U = w_xy(xy_U) ; w_V = w_xy(xy_V) ; w_H = w_xy(xy_H)
    call masssource

    call output_gtool4_init                        ! ヒストリー初期化
    call output_gtool4                             ! 初期値出力


  !------------------- 時間積分 ----------------------
    do it=1,nt
       t = it*dt
       call masssource
       w_U = ( w_U &
               + dt * w_xy( - xy_U0 * xy_GradLon_w(w_U) / R0   &
                            - xy_V  * xy_GradLat_w(w_U0) / R0  &
                            + xy_U0 * xy_V * tan(xy_Lat) / R0  &
                            + 2 * Omega * sin(xy_Lat) * xy_V   &
                           - Grav * xy_GradLon_w(w_H)/ R0   ) &
             )/(1+nu*(-rn(:,1)/R0**2)**(ndiff/2)*dt)

       xy_U = xy_w(w_U)

       w_V = ( w_V &
              + dt * w_xy( - xy_U0 * xy_GradLon_w(w_V) / R0   &
                           - xy_U * xy_U0 * tan(xy_Lat) / R0  &
                           - xy_U0 * xy_U * tan(xy_Lat) / R0  &
                           - 2 * Omega * sin(xy_Lat) * xy_U   &
                           - Grav * xy_GradLat_w(w_H) / R0 )  &
             )/(1+nu*(-rn(:,1)/R0**2)**(ndiff/2)*dt)

       xy_V = xy_w(w_V)

       w_H = ( w_H &
              + dt * ( - w_Div_xy_xy( xy_U*(H0+xy_H0)+xy_U0*xy_H,   &
                                      xy_V*(H0+xy_H0) ) / R0   )    &
                       + w_xy(xy_Q) &
              )/(1+nu*(-rn(:,1)/R0**2)**(ndiff/2)*dt)
      
       xy_H = xy_w(w_H)

       if(mod(it,ndisp) .eq. 0)then                        ! 出力
          call output_gtool4
       endif
    enddo

    call output_gtool4_close
    stop

  contains

  !------------------- 熱源設定 ----------------------
    subroutine masssource

      do j=1,jm
         do i=0,im-1
            Delta = acos( cos(xy_Lat(i,j))*cos(Lat0)*cos(xy_Lon(i,j)-Lon0) &
                 + sin(xy_Lat(i,j))*sin(Lat0) )
            if ( abs(Delta) <= pi/W0 ) then 
               xy_Q(i,j)= Q0 * (1+cos(W0*Delta))/2    &
                    * (tanh((t-5*Ta)/Ta)+1)/2
            else
               xy_Q(i,j)= 0
            endif
         enddo
      enddo
      return
    end subroutine masssource

    subroutine output_gtool4_init
      call HistoryCreate( &                               ! ヒストリー作成
           file='spshallow_masssource_linear.nc',                   &
           title='Shallow water equation on a sphere',            &
           source='Sample program of gtool_history/gtool4',       &
           institution='GFD_Dennou Club davis/spmodel project',   &
           dims=(/'lon','lat','t  '/), dimsizes=(/im,jm,0/),      &
           longnames=(/'longitude','latitude ','time     '/),     &
           units=(/'deg.','deg.','sec.'/),                        &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('lon',x_Lon*180/pi)              ! 変数出力
      call HistoryPut('lat',y_Lat*180/pi)              ! 変数出力

      call HistoryAddVariable( &                          ! 変数定義
           varname='h', dims=(/'lon','lat','t  '/), & 
           longname='surface displacement ', units='m', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='u', dims=(/'lon','lat','t  '/), & 
           longname='velocity(longitude) ', units='m/s', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='v', dims=(/'lon','lat','t  '/), & 
           longname='velocity(latitude) ', units='m/s', xtype='double')

      call HistoryAddVariable( &                          ! 変数定義
           varname='zeta', dims=(/'lon','lat','t  '/), & 
           longname='vorticity', units='1/s', xtype='double')

      call HistoryAddVariable( &                          ! 変数定義
           varname='q', dims=(/'lon','lat','t  '/), & 
           longname='mass source', units='m/s', xtype='double')
    end subroutine output_gtool4_init

    subroutine output_gtool4
      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('u',xy_U)
      call HistoryPut('v',xy_V)
      call HistoryPut('h',xy_H)
      call HistoryPut('q',xy_Q)
      xy_Zeta = xy_w(w_DivLon_xy(xy_V) - w_DivLat_xy(xy_U))/R0
      call HistoryPut('zeta',xy_Zeta)
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program shallow_masssource_linear
