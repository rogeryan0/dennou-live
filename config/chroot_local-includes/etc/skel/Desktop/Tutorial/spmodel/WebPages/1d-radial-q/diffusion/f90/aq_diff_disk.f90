!----------------------------------------------------------------------
!   Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  aq_module サンプルプログラム
!
!      拡散方程式 d temp/dt = kappa [1/r d/dr (r d temp/dr) - m^2 temp /r^2]
!
!履歴  2007/12/28  竹広真一
!
program aq_diff_disk

  use lumatrix
  use aq_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=16, km=31       ! 格子点数, 切断波数

 !---- 座標変数など ----
  real(8), parameter :: ra=2.0D0            ! 半径
  real(8), parameter :: alpha=1.0D0         ! 展開多項式パラメター  0 < α <= 1
  real(8), parameter :: beta= 1.0D0         ! 展開多項式パラメター  0 < β
  integer, parameter :: mind=5              ! 展開スペクトル上付次数

 !---- 変数 ----
  real(8), dimension(im)    :: g_temp
  real(8), dimension(0:km)  :: q_temp

 !---- 時間積分パラメター ----
  real(8), parameter :: dt=1e-6                  ! 時間ステップ間隔
  integer, parameter :: nt=10000, ndisp=1000    ! 時間積分数, 表示ステップ

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
  q_temp = q_g(g_temp)

  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4

 !------------------- 時間積分 ----------------------
  do it=1,nt
     q_temp = q_temp &
          + dt * kappa * q_g(1.0D0/g_R**2*g_q(q_rDr_q(q_rDr_q(q_temp))&
                                              -mind**2 * q_temp))

     if ( tempbc == 'D' ) then
        call aq_Boundary_D(q_temp,tempbndry)
!!$        call aq_BoundaryGrid_D(q_temp,tempbndry)
     else if ( tempbc == 'N' ) then
        call aq_Boundary_N(q_temp,tempbndry)
!!$        call aq_BoundaryGrid_N(q_temp,tempbndry)
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
           file='aq_diff_disk.nc', &
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
      g_temp = g_q(q_temp)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('temp',g_temp)
    end subroutine output_gtool4
 
    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program aq_diff_disk
