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
program at_test_base

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_module
  implicit none
  integer, parameter :: im=32, km=32, nm=2
  real(8), parameter :: xmin=-2.0D0, xmax=6.0D0
  ! 判定誤差設定
  integer, parameter :: check_digits = 7
  integer, parameter :: ignore = -8

  real(8), dimension(0:im) :: g_z
  real(8), dimension(0:im) :: g_z_sol
  real(8), dimension(nm,0:im) :: ag_z
  real(8), dimension(nm,0:im) :: ag_z_sol

  integer :: i

  call MessageNotify('M','at_test_base', &
    & 'basic function test')

  do i=1,2
    call at_initial(im,km/i,xmin,xmax)
    
    g_z = sin(g_X)
    g_z_sol = cos(g_X)
    call check1d(g_t(t_g(g_z)), g_z, 'test of g_t')
    call check1d(g_t(t_Dx_t(t_g(g_z))), g_z_sol, 'test of t_Dx_t')

    ag_z(1,:) = cos(g_X)
    ag_z(2,:) = exp(-g_X * 0.5d0)
    call check2d(ag_at(at_ag(ag_z)), ag_z, 'test of ag_at' )

    ag_z_sol(1,:) = -sin(g_X)
    ag_z_sol(2,:) = -exp(-g_X*0.5d0)*0.5d0
    call check2d(ag_at(at_Dx_at(at_ag(ag_z))), ag_z_sol, 'test of at_Dx_at' )
  enddo
  
  call MessageNotify('M','at_test_base', &
    & '1D basic transformation test suceeded')

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

  subroutine check2d(var, true, funcname)
    real(8) :: var(:,:)
    real(8) :: true(:,:)
    character(len=*), optional :: funcname

    call AssertEqual(&
      & message = funcname,   &
      & answer  = true,       &
      & check   = var,        &
      significant_digits = check_digits, ignore_digits = ignore   &
      )
  end subroutine check2d


end program at_test_base
