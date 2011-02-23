!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  et_module
!      2 次元水路領域 拡散問題
!           d\zeta/dt  = \nu\nabla^2\zeta
!
!履歴  2002/08/20  竹広真一
!      2004/02/15  佐々木 洋平 gt4f90io の使用に伴うライブラリ名変更
!      2005/06/03  佐々木 洋平 it=0 を明示的に宣言
!
program et_diff1

  use et_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter  :: im=32, jm=16      ! 格子点の設定(X,Y)
  integer, parameter  :: km=10, lm=16      ! 切断波数の設定(X,Y)

 !---- 座標変数など ----
  real(8), parameter  :: xmin=0, xmax=1     ! X 座標範囲
  real(8), parameter  :: ymin=0, ymax=1     ! Y 座標範囲

 !---- 変数 ----
  real(8)             :: xy_Temp(0:jm,0:im-1)   ! 格子データ(温度/温度擾乱)
  real(8)             :: et_Temp(-km:km,0:lm)   ! スペクトルデータ(渦度)

  real(8)             :: x_TempBndry(0:im-1,2)  ! 境界値
  real(8)             :: e_TempBndry(-km:km,2)  ! 境界値

 !---- 時間積分パラメター ----

  real(8), parameter :: dt=2e-5                 ! 時間ステップ間隔
  integer, parameter :: nt=5000, ndisp=500      ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  character(len=2), parameter :: Tempbc='DD'    ! 温度境界条件(DD/DN/ND/NN)
  real(8), parameter :: Kappa=1.0               ! 拡散係数
  real(8), parameter :: X1=(xmin+xmax)/2.0      ! 初期分布 X 座標
  real(8), parameter :: Y1=(ymin+ymax)/2.0      ! 初期分布 Y 座標
  real(8), parameter :: Sigma=0.1               ! 初期分布の大きさ

 !---- その他 ----
  integer :: it=0

 !---------------- 座標値の設定 ---------------------
  call et_Initial(im,jm,km,lm,xmin,xmax,ymin,ymax)

 !------------------- 初期値設定 ----------------------
  xy_Temp=  exp(-((yx_X-X1)**2+(yx_Y-Y1)**2)/(2*Sigma**2))
  et_Temp = et_yx(xy_Temp)

 !------------------- 境界値設定 ----------------------
  x_TempBndry(:,1) = 0.0
  x_TempBndry(:,2) = 0.0                            ! 水平一様な境界
  !x_TempBndry(:,2) = sin(2*pi*x_X)                   ! 水平温度差のある境界
  e_TempBndry(:,1) = e_x(x_TempBndry(:,1))
  e_TempBndry(:,2) = e_x(x_TempBndry(:,2))

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Euler 法による時間積分
     et_Temp = et_Temp + dt * Kappa * et_Lapla_et(et_Temp)

     call et_Boundaries(et_Temp,e_TempBndry,cond=Tempbc)

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! ヒストリー後処理
  stop

contains

    subroutine output_gtool4_init
      call HistoryCreate( &                               ! ヒストリー作成
           file='et_diff1.nc',  title='2D diffusion model',   &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club spmodel project',     &
           dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),&
           units=(/'1','1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',x_X)                            ! 変数出力
      call HistoryAddattr('x','topology','circular')      ! 周期属性
      call HistoryAddattr('x','modulo',xmax-xmin)         ! 周期属性
      call HistoryPut('y',y_Y)                            ! 変数出力

      call HistoryAddVariable( &                          ! 変数定義
           varname='temp', dims=(/'x','y','t'/), & 
           longname='temperature', units='1', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4
      xy_Temp = yx_et(et_Temp)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('temp',transpose(xy_Temp))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program et_diff1

