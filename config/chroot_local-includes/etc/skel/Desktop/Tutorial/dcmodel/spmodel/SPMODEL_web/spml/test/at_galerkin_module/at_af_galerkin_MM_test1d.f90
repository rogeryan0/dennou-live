!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_af_galerkin_MM_test1d
!
!      チェビシェフ−ガラーキン法
!      ディリクレ・ノイマン混合境界条件用モジュールテストプログラム(1次元)
!          cfdx1_xmax f'(:,i=0)  + cfdx0_xmax f(:,i=0)  = 0,
!          cfdx1_xmin f'(:,i=im) + cfdx0_xmin f(:,i=im) = 0,
!
!      例1 : 両端ディリクレ条件
!            cfdx1_xmax=0, cfdx0_xmax = 1, cfdx1_xmin=0, cfdx0_xmin = 1
!            f(x) = (x-xmin) * (xmax-x)  
!
!      例2 : 片端ディリクレ片端ノイマン境界条件
!            cfdx1_xmax=0, cfdx0_xmax = 1, cfdx1_xmin=1, cfdx0_xmin = 0
!            f(x) = (x-xmin)**2 * (xmax-x)
!
!      例3 : 片端ノイマン片端ディリクレ境界条件
!            cfdx1_xmax=1, cfdx0_xmax = 0, cfdx1_xmin=0, cfdx0_xmin = 1
!            f(x) = (x-xmin)* (xmax-x)**2
!
!      例4 : ディリクレ・ノイマン混合境界条件
!            cfdx1_xmax=1, cfdx0_xmax = -1, cfdx1_xmin=1, cfdx0_xmin = 1
!            f(x) = x**3 - 2 x**2, 0<x<1
!
!      例5 : 両端ノイマン混合境界条件
!            cfdx1_xmax=1, cfdx0_xmax = 1, cfdx1_xmin=0, cfdx0_xmin = 0
!            f(x) = (x-xmin)**2 * (xmax-x)**2, 0<x<1
!
!履歴  2006/01/03  竹広真一  新規作成
!      2006/01/23  竹広真一  モジュール名変更に伴う改訂
!      2006/02/15  竹広真一  係数行列式 0 の場合に対応
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_af_galerkin_MM_test1d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_af_galerkin_MM
  use at_module

  implicit none

  integer, parameter :: im=8, km=4
  real(8), parameter :: xmin=0.0d0, xmax=1.0d0

  real(8) :: g_data(0:im)
  real(8) :: g_data_orig(0:im)

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = - 12

  call at_Initial(im,km,xmin,xmax)

  call MessageNotify('M','at_af_galerkin_MM_test1d', &
    & '1D Mixed B.C.')

  !--- 両端ディリクレ境界条件 ---
  call at_af_galerkin_MM_Initial(im,km,       &
       cfdx0_xmax=1.0D0, cfdx1_xmax=0.0D0, &
       cfdx0_xmin=1.0D0, cfdx1_xmin=0.0D0  )

  g_data = (g_X-xmin)*(xmax-g_X)
  g_data_orig = g_data
  g_data=g_f(f_t(t_g(g_data)))

  call check1d(g_data, g_data_orig, &
    & 'Check both Dirichlt B.C.')


  !--- 片端ディリクレ片端ノイマン境界条件 ---
  call at_af_galerkin_MM_Initial(im,km,       &
       cfdx0_xmax=1.0D0, cfdx1_xmax=0.0D0, &
       cfdx0_xmin=0.0D0, cfdx1_xmin=1.0D0  )

  g_data = (g_X-xmin)**2 * (xmax-g_X)
  g_data_orig = g_data
  g_data=g_f(f_t(t_g(g_data)))

  call check1d(g_data, g_data_orig, &
    & 'Check Dirichlt and Neuman B.C.')

  !--- 片端ノイマン片端ディリクレ境界条件 ---
  call at_af_galerkin_MM_Initial(im,km,       &
       cfdx0_xmax=0.0D0, cfdx1_xmax=1.0D0, &
       cfdx0_xmin=1.0D0, cfdx1_xmin=0.0D0  )

  g_data = (g_X-xmin) * (xmax-g_X)**2
  g_data_orig = g_data
  g_data=g_f(f_t(t_g(g_data)))

  call check1d(g_data, g_data_orig, &
    & 'Check Neumman and Dirichlt B.C.')

  !--- ディリクレ・ノイマン混合境界条件 ---
  call at_af_galerkin_MM_Initial(im,km,        &
       cfdx0_xmax=1.0D0, cfdx1_xmax=-1.0D0, &
       cfdx0_xmin=1.0D0, cfdx1_xmin=1.0D0    )

  g_data = g_X**3 - 2*g_X**2
  g_data_orig = g_data
  g_data=g_f(f_t(t_g(g_data)))

  call check1d(g_data, g_data_orig, &
    & 'Check Neumman and Dirichlt mixed B.C.')


  !--- 両端ノイマン境界条件 ---
  call at_af_galerkin_MM_Initial(im,km,       &
       cfdx0_xmax=0.0D0, cfdx1_xmax=1.0D0, &
       cfdx0_xmin=0.0D0, cfdx1_xmin=1.0D0  )

  g_data = (g_X-xmin)**2*(xmax-g_X)**2
  g_data_orig = g_data
  g_data=g_f(f_t(t_g(g_data)))

  call check1d(g_data, g_data_orig, &
    & 'Check both Neumman B.C.')

  call MessageNotify('M','at_af_galerkin_MM_test1d', &
    & '1D Mixed B.C. succeeded')

contains
  subroutine check1d(var, true, funcname)
    real(8) :: var(:)
    real(8) :: true(:)
    character(len=*), optional :: funcname

    call AssertEqual(&
      & message = funcname,   &
      & answer  = true,       &
      & check   = var,        &
      significant_digits = check_digits, ignore_digits = ignore   &
      )
  end subroutine check1d

end program at_af_galerkin_MM_test1d
