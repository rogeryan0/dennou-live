!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  l_module_sjpack テストプログラム :: 補間関数のテスト
!
!履歴  2009/09/22  竹広真一
!
program l_module_sjpack_interpolate_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use l_module_sjpack
  implicit none

  integer, parameter :: jm=16, nm=10

  real(8), dimension(0:nm) ::  l_data
  real(8), dimension(1:jm) ::  y_data

  real(8) :: Alat
  real(8) :: y_data_sol
  real(8) :: error
  real(8) :: pi

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  pi = atan(1.0D0)*4.0D0
  Alat = pi/3

  call l_Initial( nm, jm )

  call MessageNotify('M','l_module_sjpack_Interpolate_test', &
                         'l_module_sjpack interpolate function tests') 


  !---- P_1 のテスト ----
  y_data = sqrt(3.0D0)*sin(y_Lat)                      ! P_1
  y_data_sol = sqrt(3.0D0)*sin(ALat)
  l_data = l_y(y_data)

  call AssertEqual(&
    message='P_1 Test of interpolate function',                   &
    answer = y_data_sol,                                          &
    check = Interpolate_l(l_data,alat),                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- P_2 のテスト ----
  y_data = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)                 ! P_2
  y_data_sol = sqrt(5.0D0)*(3.0/2*sin(alat)**2-1.0/2)
  l_data = l_y(y_data)

  call AssertEqual(&
    message='P_2 Test of interpolate function',                   &
    answer = y_data_sol,                                          &
    check = Interpolate_l(l_data,alat),                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 一般的な関数のテスト ----
  y_data = (sin(y_Lat)-1)**2*(sin(y_Lat)-0.5)*(sin(y_Lat)+1)
  y_data_sol = (sin(ALat)-1)**2*(sin(ALat)-0.5)*(sin(ALat)+1)
  l_data = l_y(y_data)
  error = Interpolate_l(l_data,alat)-y_data_sol

  call AssertEqual(&
    message='generaul function Test of interpolate function',     &
    answer = y_data_sol,                                          &
    check = Interpolate_l(l_data,alat),                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','l_module_sjpack_Interpolate_test', &
                         'l_module_sjpack interpolate function tests succeeded!') 

end program l_module_sjpack_interpolate_test
