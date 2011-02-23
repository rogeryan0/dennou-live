!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_module
!      磁場トロイダルポテンシャル拡散問題
!
!履歴  2002/08/09  竹広真一
!      2004/02/15  佐々木 洋平 gt4f90io の使用に伴うライブラリ名変更
!      2005/06/03  佐々木 洋平 it=0 を明示的に宣言
!
program wt_diff4

  use lumatrix
  use wt_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer,parameter  :: im=32, jm=16, km=8  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=8         ! 切断波数の設定(水平, 動径)

 !---- 座標変数など ----
  !real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径
  real(8),parameter  :: ri=0.1, ro=1.0       ! 内外半径

 !---- 変数 ----
  real(8), dimension(im,jm,0:km)     :: xyz_TMAG   ! ポロイダル磁場
  real(8), dimension((nm+1)**2,0:lm) :: wt_TMAG    ! ポロイダル磁場

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-4                 ! 時間ステップ間隔
  integer, parameter :: nt=20000, ndisp=1000    ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter :: eta=1.0                 ! 磁気粘性係数

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: lon1=pi                 ! 初期分布緯度座標
  real(8), parameter :: lat1=0.0                ! 初期分布経度座標
  real(8), parameter :: rad1=(ri+ro)/2.0        ! 初期分布動径座標
  real(8), parameter :: sigma=0.2               ! 初期分布の大きさ

 !---- その他 ----
  integer :: it=0

 !---------------- 座標値の設定 ---------------------
  call wt_Initial(im,jm,km,nm,lm,ri,ro)

 !------------------- 初期値設定 ----------------------
  !xyz_TMAG =  exp(-((xyz_LON-lon1)**2+(xyz_LAT-lat1)**2+(xyz_RAD-rad1)**2)&
  !               /(2*sigma**2))
  xyz_TMAG = sin(xyz_LAT) * sin( pi*(xyz_RAD-ri)/(ro-ri) )

  wt_TMAG = wt_xyz(xyz_TMAG)

  call output_gtool4_init
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Euler 法による時間積分
     wt_TMAG = wt_TMAG + dt *( eta * wt_Lapla_wt(wt_TMAG) )
     call wt_TormagBoundaries(wt_TMAG)

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        xyz_TMAG = xyz_wt(wt_TMAG)
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

 !------------------- 出力 ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! ヒストリー作成
           file='wt_diff4.nc', title='Diffusion model in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_LON/pi*180)                       ! 変数出力
    call HistoryPut('lat',y_LAT/pi*180)                       ! 変数出力
    call HistoryPut('rad',z_RAD)                              ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
           varname='tmag', dims=(/'lon','lat','rad','t  '/), & 
           longname='magnetic toroidal potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! 変数定義
           varname='Em', dims=(/'t  '/), & 
           longname='squared toroidal potential', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('tmag',xyz_TMAG)
    call HistoryPut('Em',IntLonLatRad_xyz(xyz_TMAG**2/2))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program wt_diff4
