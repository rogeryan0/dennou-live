!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ee_module
!      2 次元矩形領域周期境界条件 拡散問題
!           d\zeta/dt  = \nu\nabla^2\zeta
!
!履歴  2002/08/19  竹広真一
!      2004/02/15  佐々木 洋平 gt4f90io の使用に伴うライブラリ名変更
!      2005/03/25  佐々木 洋平 ee_module の suffix 変更に伴なう修正
!
program ee_diff1

  use ee_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32            ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=10            ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_Zeta(0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: ee_Zeta(-lm:lm,-km:km)    ! スペクトルデータ

 !---- 座標変数など ----
!  real(8)            :: xy_X(0:jm-1,0:im-1), xy_Y(0:jm-1,0:im-1)
  real(8), parameter :: xmin=0.0, xmax=1.0
  real(8), parameter :: ymin=0.0, ymax=1.0

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-4                 ! 時間ステップ間隔
  integer, parameter :: nt=1000, ndisp=100      ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter :: Nu=1.0                  ! 粘性係数
  real(8), parameter :: X1=(xmin+xmax)/2.0      ! 初期分布 X 座標
  real(8), parameter :: Y1=(ymin+ymax)/2.0      ! 初期分布 Y 座標
  real(8), parameter :: Sigma=0.1               ! 初期分布の大きさ

  integer :: it=0                           ! DO 変数


 !---------------- 座標値の設定 ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  yx_Zeta=  exp(-((yx_X-X1)**2+(yx_Y-Y1)**2)/(2*Sigma**2))
  ee_zeta = ee_yx(yx_Zeta)

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt                                         ! Euler 法による時間積分
     ee_Zeta = ee_Zeta + dt * Nu * ee_Lapla_ee(ee_Zeta)

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        yx_Zeta = yx_ee(ee_Zeta)
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! ヒストリー後処理
  stop

contains

  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! ヒストリー作成
           file='ee_diff1.nc', title='2D diffusion model',   &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club spmodel project',     &
           dims=(/'x','y','t'/), dimsizes=(/im,jm,0/),      &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),&
           units=(/'1','1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('x',x_X)                                  ! 変数出力
    call HistoryAddattr('x','topology','circular')            ! 周期属性
    call HistoryAddattr('x','modulo',xmax-xmin)               ! 周期属性

    call HistoryPut('y',y_Y)                                  ! 変数出力
    call HistoryAddattr('y','topology','circular')            ! 周期属性
    call HistoryAddattr('y','modulo',ymax-ymin)               ! 周期属性

    call HistoryAddVariable( &                                ! 変数定義
           varname='zeta', dims=(/'x','y','t'/), & 
           longname='vorticity', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('zeta',transpose(yx_Zeta))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program ee_diff1

