!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_interpolate_module_sjpack テストプログラム :: 補間関数のテスト
!
!履歴  2009/09/04  竹広真一    w_test_interpolate00.f90 を改造, SJPACK 対応
!
program w_interpolate_module_sjpack_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use w_module_sjpack
  implicit none

  integer, parameter :: im=64, jm=32, nm=21

  real(8), dimension((nm+1)**2)   ::  w_data
  real(8), dimension(0:im-1,1:jm) ::  xy_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  real(8) :: Alon
  real(8) :: Alat
  real(8) :: data_sol
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0
  Alon = pi/5 ; Alat = pi/3

  call w_Initial( nm, im, jm )

  call MessageNotify('M','w_test_Interpolate', &
                         'w_interpolate_module interpolate(00) tests') 

  !---- Y_1^0 のテスト ----
  xy_data = sqrt(3.0D0)*sin(xy_Lat)                      ! Y_1^0
  data_sol = sqrt(3.0D0)*sin(ALat)
  w_data = w_xy(xy_data)
  call AssertEqual(&
    message='Interpolate_w with Y_1^0',                           &
    answer = data_sol,                                            &
    check = Interpolate_w(w_data,alon,alat),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_1^1 のテスト ----
  xy_data = sqrt(3.0D0/2)*cos(xy_Lat)*cos(xy_Lon)          ! Y_1^1
  data_sol = sqrt(3.0D0/2)*cos(ALat)*cos(ALon)
  w_data = w_xy(xy_data)
  call AssertEqual(&
    message='Interpolate_w with Y_1^1',                           &
    answer = data_sol,                                            &
    check = Interpolate_w(w_data,alon,alat),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_1^-1 のテスト ----
  xy_data = -sqrt(3.0D0)*cos(xy_Lat)*sin(xy_Lon)         ! Y_1^{-1}
  data_sol = -sqrt(3.0D0)*cos(ALat)*sin(ALon)
  w_data = w_xy(xy_data)
  call AssertEqual(&
    message='Interpolate_w with Y_1^-1',                          &
    answer = data_sol,                                            &
    check = Interpolate_w(w_data,alon,alat),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_2^0 のテスト ----
  xy_data = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)                 ! Y_2^0
  data_sol = sqrt(5.0D0)*(3.0/2*sin(alat)**2-1.0/2)
  w_data = w_xy(xy_data)
  call AssertEqual(&
    message='Interpolate_w with Y_2^0',                          &
    answer = data_sol,                                            &
    check = Interpolate_w(w_data,alon,alat),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_2^1 のテスト ----
  xy_data = sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)! Y_2^1
  data_sol = sqrt(5.0D0/6)*3.0*sin(ALat)*cos(ALat) * cos(ALon)
  w_data = w_xy(xy_data)
  call AssertEqual(&
    message='Interpolate_w with Y_2^1',                           &
    answer = data_sol,                                            &
    check = Interpolate_w(w_data,alon,alat),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_2^-1 のテスト ----
  xy_data = -sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat) * sin(xy_Lon)! Y_2^{-1}
  data_sol = -sqrt(5.0D0/6)*3.0*sin(ALat)*cos(ALat) * sin(ALon)
  w_data = w_xy(xy_data)
  call AssertEqual(&
    message='Interpolate_w with Y_2^-1',                          &
    answer = data_sol,                                            &
    check = Interpolate_w(w_data,alon,alat),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_2^2 のテスト ----
  xy_data = sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*cos(2*xy_Lon)             ! Y_2^2
  data_sol = sqrt(5.0D0/24)*3.0*cos(ALat)**2*cos(2*ALon)
  w_data = w_xy(xy_data)
  call AssertEqual(&
    message='Interpolate_w with Y_2^2',                           &
    answer = data_sol,                                            &
    check = Interpolate_w(w_data,alon,alat),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_2^-2 のテスト ----
  xy_data = -sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*sin(2*xy_Lon)          ! Y_2^-2
  data_sol = -sqrt(5.0D0/24)*3.0*cos(ALat)**2*sin(2*ALon)
  w_data = w_xy(xy_data)
  call AssertEqual(&
    message='Interpolate_w with Y_2^-2',                          &
    answer = data_sol,                                            &
    check = Interpolate_w(w_data,alon,alat),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 一般的な関数のテスト ----
  xy_data = cos(2*xy_Lon-pi/3) &
       *(sin(xy_Lat)-1)**2*(sin(xy_Lat)-0.5)*(sin(xy_Lat)+1)
  data_sol = cos(2*ALon-pi/3) &
       *(sin(ALat)-1)**2*(sin(ALat)-0.5)*(sin(ALat)+1)
  w_data = w_xy(xy_data)
  call AssertEqual(&
    message='Interpolate_w with generaul function',               &
    answer = data_sol,                                            &
    check = Interpolate_w(w_data,alon,alat),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','w_Interpolate_module_sjpack_test', &
                      'w_interpolate_module_sjpack function tests succeeded') 

end program w_interpolate_module_sjpack_test

