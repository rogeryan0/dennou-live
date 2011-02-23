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
program at_test_base2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_module
  implicit none
  integer, parameter :: im=8, km=5, nm=4
  real(8), parameter :: xmin=-1.0D0, xmax=1.0D0

  real(8), dimension(0:nm,0:im) :: ag_y
  real(8), dimension(0:nm,0:im) :: ag_y_deriv
  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  call MessageNotify('M','at_test_base2d', &
    & '2D basic functions test')

  call at_initial(im,km,xmin,xmax)
  call MessageNotify('M','at_test_base2d', &
    & 'y(n) = T_n(x), n=0..4')

  ag_y(0,:) = 1.0d0                                  ! T_0
  ag_y(1,:) = g_x                                    ! T_1
  ag_y(2,:) = 2.0d0*g_x**2 - 1.0d0                   ! T_2
  ag_y(3,:) = 4.0d0*g_x**3 - 3.0d0*g_x               ! T_3
  ag_y(4,:) = 8.0d0*g_x**4 - 8.0d0*g_x**2 + 1.0d0    ! T_4

  ag_y_deriv(0,:) = 0.0D0                            ! T_0'= 0
  ag_y_deriv(1,:) = 1.0D0                            ! T_1'= T_0
  ag_y_deriv(2,:) = 4.0d0*g_x                        ! T_2'= 4*T_1
  ag_y_deriv(3,:) = 12.0d0*g_x**2 - 3.0d0            ! T_3'= 6*T_2 + 3*T_0
  ag_y_deriv(4,:) = 32.0d0*g_x**3 - 16.0d0*g_x       ! T_4'= 8*T_3 + 8*T_1

  call check2d(ag_at(at_ag(ag_y)), ag_y, 'test of ag_at, at_ag')
  call check2d(ag_at(at_dx_at(at_ag(ag_y))), ag_y_deriv, 'test of at_Dx_at')

  call MessageNotify('M','at_test_base2d', &
    & '2D basic functions test succeeded')

contains
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

end program at_test_base2d

