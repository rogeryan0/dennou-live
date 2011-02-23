!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_integral_module_sjpack テストプログラム :: 積分・平均関数のテスト
!
!履歴  2009/09/04  竹広真一    w_test_integral.f90 を改造, SJPACK 対応
!
program w_integral_module_sjpack_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use w_module_sjpack
  implicit none

  integer, parameter :: im=128, jm=64, nm=42

  real(8), dimension(0:im-1,1:jm) ::  xy_data            ! 元の関数
  real(8), dimension(0:im-1)      ::  x_idata            ! 積分・平均の正解
  real(8), dimension(1:jm)        ::  y_idata            ! 積分・平均の正解
  real(8)                         ::  idata              ! 積分・平均の正解
  real(8) :: pi

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','w_integral_module_sjpack', &
                         'w_integral_module_sjpack function tests') 

  call w_Initial( nm, im, jm )

  !---- 積分のテスト ----
  xy_data = sin(xy_Lat)**2*(1-cos(xy_Lon)**2)

  idata = 2*pi/3       ! IntLonLat_xy
  call AssertEqual(&
    message='IntLonLat_xy',                                       &
    answer = idata,                                               &
    check = IntLonLat_xy(xy_data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_idata = pi*sin(y_Lat)**2       ! y_IntLon_xy
  call AssertEqual(&
    message='y_IntLon_xy',                                        &
    answer = y_idata,                                             &
    check = y_IntLon_xy(xy_data),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_idata = 2.0D0/3*(1-cos(x_Lon)**2)       ! x_IntLat_xy
  call AssertEqual(&
    message='x_IntLat_xy',                                        &
    answer = x_idata,                                             &
    check = x_IntLat_xy(xy_data),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='IntLon_x',                                           &
    answer = idata,                                               &
    check = IntLon_x(x_idata),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='IntLat_y',                                           &
    answer = idata,                                               &
    check = IntLat_y(y_idata),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 平均のテスト ----
  xy_data = sin(xy_Lat)**2*(1-cos(xy_Lon)**2)

  idata = 1.0D0/6       ! AvrLonLat_xy
  call AssertEqual(&
    message='AvrLonLat_xy',                                       &
    answer = idata,                                               &
    check = AvrLonLat_xy(xy_data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_idata = 0.5*sin(y_Lat)**2       ! y_AvrLon_xy
  call AssertEqual(&
    message='y_AvrLon_xy',                                        &
    answer = y_idata,                                             &
    check = y_AvrLon_xy(xy_data),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_idata = 1.0D0/3*(1-cos(x_Lon)**2)       ! x_AvrLat_xy

  call AssertEqual(&
    message='x_AvrLat_xy',                                        &
    answer = x_idata,                                             &
    check = x_AvrLat_xy(xy_data),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='AvrLon_x',                                           &
    answer = idata,                                               &
    check = AvrLon_x(x_idata),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='AvrLat_y',                                           &
    answer = idata,                                               &
    check = AvrLat_y(y_idata),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )


  call MessageNotify('M','w_integral_sjpack_test', &
                         'w_integral_module_sjpack function tests succeded!') 

end program w_integral_module_sjpack_test
