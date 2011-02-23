!----------------------------------------------------------------------
!     Copyright (c) 2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_module サンプルプログラム : 線形解析(チェビシェフ空間)
!
!      拡散方程式 d zeta/dt = kappa d^2 zeta/dx^2
!
!      境界固定条件  \zeta=0 at x=0,1 => \sigma = -\kappa \pi^2 m^2 (m=1,2,..)
!
!履歴  2005/01/27 竹広真一
!      2008/12/05 佐々木洋平. 文字列 sort の長さを 2 に
!
program at_diff_eigen_cheb

  use eigmatrix
  use lumatrix
  use at_module
  use gt4_history
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=16                ! 格子点の設定
  integer, parameter :: km=16                ! 切断波数の設定
  integer, parameter :: nm=10                ! 出力固有モードの設定

 !---- 座標変数など ----
  real(8), parameter :: xmin=0.0D0, xmax=1.0D0   ! 範囲

 !---- 変数 ----
  !real(8), dimension(0:im)  :: g_zeta
  real(8), dimension(0:km)  :: t_zeta

  real(8), dimension(0:km)  :: t_dzetadt

 !---- 線形解析用変数ー ----
  real(8), dimension(0:km-2,0:km-2)  :: matrix      ! 係数行列

  real(8), dimension(nm)             :: eigen_r     ! 固有値実部
  real(8), dimension(nm)             :: eigen_i     ! 固有値虚部
  real(8), dimension(0:km-2,nm)      :: eigvec_r    ! 固有ベクトル実部
  real(8), dimension(0:km-2,nm)      :: eigvec_i    ! 固有ベクトル虚部

  real(8), dimension(nm)             :: eigen_r_analytic  ! 固有値実部(解析解)

 !---- 物理パラメター ----
  real(8), parameter, dimension(2) :: zetabndry=(/0.0,0.0/)  ! 境界値
  character(len=2), parameter :: tempbc='DD'    ! 境界条件(DD/DN/ND/NN)
  real(8), parameter :: kappa=1.0               ! 拡散係数
  real(8) :: pi

  integer info, k, n

  pi = 4*atan(1.0d0)

 !---------------- 座標値の設定 ---------------------
  call at_initial(im,km,xmin,xmax)

 !---------------- 係数行列計算 ---------------------
  do k=0,km-2
     t_zeta    = 0.0
     t_zeta(k) = 1.0
     call boundaries
     t_dzetadt = kappa * t_dx_t(t_dx_t(t_zeta) )

     matrix(:,k) = t_dzetadt(0:km-2)
  enddo

 !------------------ 固有値計算 ---------------------
  call eigen(matrix,eigen_r,eigen_i,eigvec_r,eigvec_i,info,&
       sort=' R', reverse=.true.)

  eigen_r_analytic =(/(-kappa * pi**2 * n**2,n=1,nm)/)

 !------------------- 出力 ----------------------
  call output_gtool4_init                            ! ヒストリー初期化
  call output_gtool4
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
           file='at_diff_eigen_cheb.nc', title='Linear analysis fo diffusion equation (Chebyshev base)', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x   ','mode'/), dimsizes=(/im+1,0/), &
           longnames=(/'X-coordinate','mode number '/),    &
           units=(/'1','1'/),                              &
           origin=1.0, interval=1.0 ) 

      call HistoryPut('x',g_x)                            ! 変数出力

      call HistoryAddVariable( &                          ! 変数定義
           varname='eigen_r', dims=(/'mode'/), & 
           longname='Eigen value (R)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='eigen_i', dims=(/'mode'/), & 
           longname='Eigen value (I)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='eigen_r_analytic', dims=(/'mode'/), & 
           longname='Analytic Eigen value (R)', units='1', xtype='double')

      call HistoryAddVariable( &                          ! 変数定義
           varname='eigvec_r', dims=(/'x   ','mode'/), & 
           longname='Eigen vector (R)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! 変数定義
           varname='eigvec_i', dims=(/'x   ','mode'/), & 
           longname='Eigen vector (I)', units='1', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4
      do n=1,nm
         call HistoryPut('mode',real(n))
         call HistoryPut('eigen_r',eigen_r(n))
         call HistoryPut('eigen_i',eigen_i(n))
         call HistoryPut('eigen_r_analytic',eigen_r_analytic(n))

         t_zeta = 0.0 ; t_zeta(0:km-2) = eigvec_r(:,n)
         call boundaries
         call HistoryPut('eigvec_r',g_t(t_zeta))

         t_zeta = 0.0 ; t_zeta(0:km-2) = eigvec_i(:,n)
         call boundaries
         call HistoryPut('eigvec_i',g_t(t_zeta))
      enddo
    end subroutine output_gtool4
 
    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program at_diff_eigen_cheb

