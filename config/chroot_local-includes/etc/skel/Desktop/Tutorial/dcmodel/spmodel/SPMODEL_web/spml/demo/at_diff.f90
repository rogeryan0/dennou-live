!----------------------------------------------------------------------
!   Copyright (c) 2002--2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_module サンプルプログラム
!
!      拡散方程式 d zeta/dt = kappa d^2 zeta/dx^2
!
!履歴  2002/07/06  竹広真一
!      2005/03/15  竹広真一
!      2005/06/03  佐々木 洋平 it=0 を明示的に宣言
!
program at_diff

  use lumatrix
  use at_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=8                ! 格子点の設定
  integer, parameter :: km=8                ! 切断波数の設定

 !---- 座標変数など ----
  real(8), parameter :: xmin=-1.0D0, xmax=1.0D0   ! 範囲

 !---- 変数 ----
  real(8), dimension(0:im)  :: g_zeta
  real(8), dimension(0:km)  :: t_zeta

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-3                 ! 時間ステップ間隔
  integer, parameter :: nt=1000, ndisp=100      ! 時間積分数, 表示ステップ

 !---- 物理パラメター ----
  real(8), parameter, dimension(2) :: zetabndry=(/0.0,0.0/)  ! 境界値
  character(len=2), parameter :: tempbc='DN'    ! 境界条件(DD/DN/ND/NN)

  real(8), parameter :: sigma=0.1               ! 初期値の幅
  real(8), parameter :: kappa=1.0               ! 拡散係数
 
  integer :: it=0

 !---------------- 座標値の設定 ---------------------
  call at_initial(im,km,xmin,xmax)

 !------------------- 初期値設定 ----------------------
  g_zeta = exp( - ( (g_x-g_x(im/2))/sigma)**2 )
  t_zeta = t_g(g_zeta)
  call boundaries

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt
     t_zeta = t_zeta + dt * ( kappa * t_dx_t(t_dx_t(t_zeta) ) )
     call boundaries

     if(mod(it,ndisp) .eq. 0)then                    ! 出力
        g_zeta = g_t(t_zeta)
        call output_gtool4
     endif

  enddo
  call output_gtool4_close                           ! ヒストリー後処理

  stop

  contains

    subroutine boundaries
      if ( tempbc == 'DD' ) then
         call at_boundaries_DD(t_zeta,zetabndry)
      elseif ( tempbc == 'DN' ) then
         call at_boundaries_DN(t_zeta,zetabndry)
      elseif ( tempbc == 'ND' ) then
         call at_boundaries_ND(t_zeta,zetabndry)
      elseif ( tempbc == 'NN' ) then
         call at_boundaries_NN(t_zeta,zetabndry)
      else
         write(6,*) 'B.C. not supported'
      endif
    end subroutine boundaries

    subroutine output_gtool4_init
      call HistoryCreate( &                               ! ヒストリー作成
           file='at_diff.nc', title='Diffusion equation', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','t'/), dimsizes=(/im+1,0/),    &
           longnames=(/'X-coordinate','time        '/),&
           units=(/'1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',g_x)                            ! 変数出力

      call HistoryAddVariable( &                          ! 変数定義
           varname='zeta', dims=(/'x','t'/), & 
           longname='voticity', units='1', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4
      g_zeta = g_t(t_zeta)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('zeta',g_zeta)
    end subroutine output_gtool4
 
    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program at_diff
