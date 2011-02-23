!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  l_module_sjpack テストプログラム :: 積分・平均関数のテスト
!
!履歴  2009/09/22  竹広真一
!
program l_module_sjpack_integral_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use l_module_sjpack
  implicit none

  integer, parameter :: jm=16, nm=10

  real(8), dimension(1:jm)        ::  y_idata            ! 積分・平均の正解
  real(8)                         ::  idata              ! 積分・平均の正解
  real(8), parameter              ::  eps = 1.0D-10
  real(8) :: pi

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','l_module_sjpack_integral_test', &
                         'l_module_sjpack tests of integral/average functions') 

  call l_Initial( nm, jm )

  !---- 積分のテスト ----
  y_idata = pi*sin(y_Lat)
  idata   = 0.0D0                   ! IntLat_y

  call AssertEqual(&
    message='Test of IntLat_y for an odd function',               &
    answer = idata,                                               &
    check = IntLat_y(y_idata),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_idata = pi*sin(y_Lat)**2      
  idata   = 2*pi/3                  ! IntLat_y

  call AssertEqual(&
    message='Test of IntLat_y for an even function',              &
    answer = idata,                                               &
    check = IntLat_y(y_idata),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 平均のテスト ----
  y_idata = 0.5*sin(y_Lat)
  idata   = 0.0D0                   ! AvrLat_y

  call AssertEqual(&
    message='Test of AvrLat_y for an odd function',               &
    answer = idata,                                               &
    check = AvrLat_y(y_idata),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_idata = 0.5*sin(y_Lat)**2
  idata   = 1.0D0/6                 ! AvrLat_y

  call AssertEqual(&
    message='Test of AvrLat_y for an even function',              &
    answer = idata,                                               &
    check = AvrLat_y(y_idata),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','l_module_sjpack_integral_test', &
       'l_module_sjpack tests of integral/average functions succeeded') 

end program l_module_sjpack_integral_test
