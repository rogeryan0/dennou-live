!----------------------------------------------------------------------
! Copyright (c) 2002-2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_module テストプログラム
!
!履歴  2002/04/10  竹広真一
!      2007/10/25  竹広真一  エラーメッセージ追加
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_test_base1d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_module
  implicit none
  integer, parameter :: im=4, km=4
  real(8), parameter :: xmin=-1.0D0, xmax=1.0D0
  ! 判定誤差設定
  integer, parameter :: check_digits = 14
  integer, parameter :: ignore = -15

  real(8), dimension(0:im) :: g_z
  real(8), dimension(0:km) :: t_z_sol

  call MessageNotify('M','at_test_base1d', &
    & '1D basic functions test')

  call at_initial(im,km,xmin,xmax)

  call MessageNotify('M','at_test_base1d', 'z = T_2(x)')
  g_z = 2.0d0*g_x**2 - 1.0d0
  t_z_sol = 0.0d0 ; t_z_sol(2) = 1.0d0
  call check1d(t_g(g_z), t_z_sol, 'test of t_g')
  call check1d(g_t(t_g(g_z)), g_z, 'test of g_t')
  call check1d(g_t(t_Dx_t(t_g(g_z))), 4.0d0*g_x, 'test of t_Dx_t')

  call MessageNotify('M','at_test_base1d', 'z = T_4(x)')
  g_z = 8.0d0*g_x**4 - 8.0d0*g_x**2 + 1.0d0
  t_z_sol = 0.0d0; t_z_sol(4) = 2.0d0
  call check1d(t_g(g_z), t_z_sol, 'test of t_g')
  call check1d(g_t(t_g(g_z)), g_z, 'test of g_t')
  call check1d(g_t(t_Dx_t(t_g(g_z))), 32.0d0*g_x**3 - 16.0d0*g_x, &
    'test of t_Dx_t')

  call MessageNotify('M','at_test_base1d', &
    & '1D basic functions test succeeded')

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

end program at_test_base1d
