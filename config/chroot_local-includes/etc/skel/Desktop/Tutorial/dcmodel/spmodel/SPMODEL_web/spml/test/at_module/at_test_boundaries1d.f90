!----------------------------------------------------------------------
! Copyright (c) 2002-2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_module テストプログラム
!      1 次元境界値問題
!
!履歴  2002/11/19  竹広真一
!      2007/10/25  竹広真一  エラーメッセージ追加
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_test_boundaries1d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_module
  implicit none

  integer, parameter :: im=8, km=8
  real(8), parameter :: xmin=-1.0D0, xmax=1.0D0
  real(8), parameter :: amin=2.0D0, amax=-1.0D0

  real(8), dimension(0:im) :: g_a
  real(8), dimension(0:im) :: g_dadx
  real(8), dimension(0:km) :: t_a
  real(8), dimension(2)    :: values=(/amin,amax/)

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  call MessageNotify('M','at_test_boundaries1d', &
    & '1D Boundary value problem')

  call at_initial(im,km,xmin,xmax)

  g_a = sin(g_X)
  t_a = t_g(g_a)
!----- at_BoundariesGrid_DD ------
  call at_BoundariesGrid_DD(t_a,values)
  g_a = g_t(t_a)
  call check(g_a(0), amin, 'test of at_Boundaries_DD(0)')
  call check(g_a(im), amax, 'test of at_Boundaries_DD(im)')
  call MessageNotify('M','Test of at_BoundariesGrid_DD', &
       'succeeded!')
  
!----- at_BoundariesGrid_DN ------
  call at_BoundariesGrid_DN(t_a,values)
  g_a = g_t(t_a)
  g_dadx = g_t(t_dx_t(t_g(g_a)))
  call check(g_a(0), amin, 'test of at_Boundaries_DN(0)')
  call check(g_dadx(im), amax, 'test of at_Boundaries_DN(im)')
  call MessageNotify('M','Test of at_BoundariesGrid_DN', &
       'succeeded!')

!----- at_BoundariesGrid_ND ------
  call at_BoundariesGrid_ND(t_a,values)
  g_a = g_t(t_a)
  g_dadx = g_t(t_dx_t(t_g(g_a)))
  call check(g_dadx(0), amin, 'test of at_Boundaries_ND(0)')
  call check(g_a(im), amax, 'test of at_Boundaries_ND(im)')
  call MessageNotify('M','Test of at_BoundariesGrid_ND', &
       'succeeded!')

!----- at_BoundariesGrid_NN ------
  call at_BoundariesGrid_NN(t_a,values)
  g_a = g_t(t_a)
  g_dadx = g_t(t_dx_t(t_g(g_a)))
  call check(g_dadx(0), amin, 'test of at_Boundaries_NN(0)')
  call check(g_dadx(im), amax, 'test of at_Boundaries_NN(im)')
  call MessageNotify('M','Test of at_BoundariesGrid_NN', &
       'succeeded!')
!----- at_BoundariesTau_DD ------
  call at_BoundariesTau_DD(t_a,values)
  g_a = g_t(t_a)
  call check(g_a(0), amin, 'test of at_BoundariesTau_DD(0)')
  call check(g_a(im), amax, 'test of at_BoundariesTau_DD(im)')
  call MessageNotify('M','Test of at_BoundariesTau_DD', &
       'succeeded!')
  
!----- at_BoundariesTau_DN ------
  call at_BoundariesTau_DN(t_a,values)
  g_a = g_t(t_a)
  g_dadx = g_t(t_dx_t(t_g(g_a)))
  call check(g_a(0), amin, 'test of at_BoundariesTau_DN(0)')
  call check(g_dadx(im), amax, 'test of at_BoundariesTau_DN(im)')
  call MessageNotify('M','Test of at_BoundariesTau_DN', &
       'succeeded!')

!----- at_BoundariesTau_ND ------
  call at_BoundariesTau_ND(t_a,values)
  g_a = g_t(t_a)
  g_dadx = g_t(t_dx_t(t_g(g_a)))
  call check(g_dadx(0), amin, 'test of at_BoundariesTau_ND(0)')
  call check(g_a(im), amax, 'test of at_BoundariesTau_ND(im)')
  call MessageNotify('M','Test of at_BoundariesTau_ND', &
       'succeeded!')
!----- at_BoundariesTau_NN ------
  call at_BoundariesTau_NN(t_a,values)
  g_a = g_t(t_a)
  g_dadx = g_t(t_dx_t(t_g(g_a)))
  call check(g_dadx(0), amin, 'test of at_BoundariesTau_NN(0)')
  call check(g_dadx(im), amax, 'test of at_BoundariesTau_NN(im)')
  call MessageNotify('M','Test of at_BoundariesTau_NN', &
       'succeeded!')

  call MessageNotify('M','at_test_boundaries1d', &
    & '1D Boundary value problem suceeded')

contains
  subroutine check(var, true, funcname)
    real(8) :: var
    real(8) :: true
    character(len=*), optional :: funcname

    call AssertEqual(&
      & message = funcname,   &
      & answer  = true,       &
      & check   = var,        &
      significant_digits = check_digits, ignore_digits = ignore   &
      )
  end subroutine check


end program at_test_boundaries1d
