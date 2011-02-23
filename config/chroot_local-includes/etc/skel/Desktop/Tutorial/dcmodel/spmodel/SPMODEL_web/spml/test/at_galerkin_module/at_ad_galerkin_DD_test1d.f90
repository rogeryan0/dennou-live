!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_ad_galerkin_DD_test1d
!
!      チェビシェフ−ガラーキン法
!      両端ディリクレ境界条件用モジュールテストプログラム(1次元)
!          f(x=xmin)=f(x=xmax)=0
!
!      例 : f(x) = (x-xmin)*(xmax-x)
!
!履歴  2005/12/29  竹広真一  新規作成
!      2006/01/23  竹広真一  モジュール名変更に伴う改訂
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_ad_galerkin_DD_test1d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_ad_galerkin_DD
  use at_module

  implicit none

  integer, parameter :: im=8, km=4
  real(8), parameter :: xmin=0.0D0, xmax=1.0D0

  real(8) :: g_data(0:im)
  real(8) :: g_data_orig(0:im)
  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','at_ad_galerkin_DD_test1d', &
       & '1D both Dirichlet B.C.')

  call at_Initial(im,km,xmin,xmax)
  call at_ad_galerkin_DD_Initial(im,km)

  g_data = (g_X-xmin)*(xmax-g_X)
  g_data_orig = g_data
  g_data=g_t(t_d(d_t(t_g(g_data))))
  call check1d(g_data, g_data_orig, &
    &' test of Grid -> Galerkin -> Grid ')

  call MessageNotify('M','at_ad_galerkin_DD_test1d', &
       & '1D both Dirichlet B.C. succeeded')

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

end program at_ad_galerkin_DD_test1d
