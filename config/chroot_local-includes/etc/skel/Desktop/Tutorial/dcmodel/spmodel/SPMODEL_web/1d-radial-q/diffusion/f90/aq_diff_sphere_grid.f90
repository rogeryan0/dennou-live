!----------------------------------------------------------------------
!   Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  aq_module サンプルプログラム
!
!      拡散方程式 
!   d temp/dt = kappa [1/r^2 d/dr (r^2 d temp/dr) - n(n+1)temp/r^2]
!             = kappa [1/r^2 rd/dr (r d temp/dr)+ 1/r^2(r d temp/dr)
!                      - n(n+1)temp/r^2]
!
!履歴  2008/04/02  竹広真一
!
program aq_diff_sphere_grid

  use lumatrix
  use aq_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, km=63       ! 格子点数, 切断波数

 !---- 座標変数など ----
  real(8), parameter :: ra=2.0D0            ! 半径
  real(8), parameter :: alpha=1.0D0         ! 展開多項式パラメター  0 < α <= 1
  real(8), parameter :: beta= 2.0D0         ! 展開多項式パラメター  0 < β
  integer, parameter :: mind=3             ! 展開スペクトル上付次数

 !---- 変数 ----
  real(8), dimension(im)    :: g_temp

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-5                 ! 時間ステップ間隔
  integer, parameter :: nt=100000, ndisp=10000    ! 時間積分数, 表示ステップ
 
 !---- 物理パラメター ----
  real(8), parameter :: tempbndry=0.0           ! 境界値
  character(len=2), parameter :: tempbc='N'     ! 境界条件(D/N)

!!$  real(8), parameter :: sigma=0.1               ! 初期値の幅
  real(8), parameter :: kappa=1.0               ! 拡散係数
 
  integer :: it=0

 !---------------- 座標値の設定 ---------------------
  call aq_Initial(im,km,ra,alpha,beta,(/mind/))

 !------------------- 初期値設定 ----------------------
  g_temp = g_R**mind * (ra-g_R)

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt
     g_temp = g_temp &
          + dt * kappa * &
                1.0D0/g_R**2*(g_q(q_rDr_q(q_rDr_q(q_g(g_temp))) &
                                +q_rDr_q(q_g(g_temp)))-mind*(mind+1)*g_temp )

     if ( tempbc == 'D' ) then
        call aq_BoundaryGrid_D(g_temp,tempbndry)
     else if ( tempbc == 'N' ) then
        call aq_BoundaryGrid_N(g_temp,tempbndry)
     else
        write(6,*) 'B.C. not supported'
     endif

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        call output_gtool4
     endif

  enddo
  call output_gtool4_close                           ! ヒストリー後処理

  stop

  contains

    subroutine output_gtool4_init
      call HistoryCreate( &                               ! ヒストリー作成
           file='aq_diff_sphere_grid.nc', &
           title='Diffusion equation in a sphere', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'r','t'/), dimsizes=(/im,0/),    &
           longnames=(/'radial coordinate',&
                       'time             '/),&
           units=(/'1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('r',g_R)                            ! 変数出力

      call HistoryAddVariable( &                          ! 変数定義
           varname='temp', dims=(/'r','t'/), & 
           longname='voticity', units='1', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('temp',g_temp)
    end subroutine output_gtool4
 
    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program aq_diff_sphere_grid
