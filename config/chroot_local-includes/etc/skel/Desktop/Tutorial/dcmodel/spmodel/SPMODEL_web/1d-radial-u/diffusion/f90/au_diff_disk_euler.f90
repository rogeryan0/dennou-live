!----------------------------------------------------------------------
!   Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_module サンプルプログラム
!
!      拡散方程式 d temp/dt = kappa 1/r d/dr (r d temp/dr)
!
!履歴  2007/12/28  竹広真一
!
program au_diff_disk

  use lumatrix
  use au_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=16                ! 格子点の設定
  integer, parameter :: km=16               ! 切断波数の設定

 !---- 座標変数など ----
  real(8), parameter :: ra=2.0D0            ! 半径

 !---- 変数 ----
  real(8), dimension(0:im)  :: g_temp
  real(8), dimension(0:km)  :: u_temp

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-5                   ! 時間ステップ間隔
  integer, parameter :: nt=100000, ndisp=10000    ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter :: tempbndry=0.0           ! 境界値
  character(len=2), parameter :: tempbc='D'     ! 境界条件(D/N)

!!$  real(8), parameter :: sigma=0.1               ! 初期値の幅
  real(8), parameter :: kappa=1.0               ! 拡散係数
 
  integer :: it=0

 !---------------- 座標値の設定 ---------------------
  call au_initial(im,km,ra,(/0/))

 !------------------- 初期値設定 ----------------------
  g_temp = 1.0D0
  u_temp = u_g(g_temp)

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt
     u_temp = u_temp &
          + dt * kappa * u_g(1.0D0/g_R * g_Dr_u(u_g(g_R*g_Dr_u(u_temp))))

     if ( tempbc == 'D' ) then
        call au_Boundary_D(u_temp,tempbndry)
     else if ( tempbc == 'N' ) then
        call au_Boundary_N(u_temp,tempbndry)
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
           file='au_diff_disk.nc', &
           title='Diffusion equation in a sphere', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'r','t'/), dimsizes=(/im+1,0/),    &
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
      g_temp = g_u(u_temp)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('temp',g_temp)
    end subroutine output_gtool4
 
    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program au_diff_disk
