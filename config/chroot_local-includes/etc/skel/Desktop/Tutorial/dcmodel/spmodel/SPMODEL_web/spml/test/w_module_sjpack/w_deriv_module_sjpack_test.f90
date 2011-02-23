!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_deriv_module_sjpack テストプログラム :: 微分関数のテスト
!
!履歴  2009/09/04  竹広真一
!
program w_deriv_module_sjpack_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use w_module_sjpack
  implicit none

  integer, parameter :: im=128, jm=64, nm=42

  real(8), dimension(0:im-1,1:jm)  ::  xy_data1              ! 元の関数
  real(8), dimension(0:im-1,1:jm)  ::  xy_data2              ! 元の関数
  real(8), dimension(0:im-1,1:jm)  ::  xy_ddata              ! 微分の正解
  real(8), dimension(0:im-1,1:jm)  ::  mu                    ! μ=sinφ

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  call MessageNotify('M','w_deriv_module_sjpack_test', &
                         'w_deriv_module_sjpack function tests') 

  call w_Initial( nm, im, jm )

  !---- Y_1^{-1} のテスト ----
  xy_data1 = -cos(xy_Lat)*sin(xy_Lon)         ! Y_1^{-1}

  xy_ddata = 2*cos(xy_Lat)*sin(xy_Lon)       ! w_Lapla_w
  call AssertEqual(&
    message='Y_1^0 test of w_Lapla_w',                            &
    answer = xy_ddata,                                            &
    check = xy_w(w_Lapla_w(w_xy(xy_data1))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = 1.0/2.0*cos(xy_Lat)*sin(xy_Lon)      ! w_LaplaInv_w
  call AssertEqual(&
    message='Y_1^0 test of w_LaplaInv_w',                         &
    answer = xy_ddata,                                            &
    check = xy_w(w_LaplaInv_w(w_xy(xy_data1))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = -cos(xy_Lat)*cos(xy_Lon)         ! w_DLon_w
  call AssertEqual(&
    message='Y_1^0 test of w_DLon_w',                             &
    answer = xy_ddata,                                            &
    check = xy_w(w_DLon_w(w_xy(xy_data1))),                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = -cos(xy_Lon)                     ! xy_GradLon_w
  call AssertEqual(&
    message='Y_1^0 test of xy_GradLon_w',                         &
    answer = xy_ddata,                                            &
    check = xy_GradLon_w(w_xy(xy_data1)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = sin(xy_Lat)*sin(xy_Lon)         ! xy_GradLat_w
  call AssertEqual(&
    message='Y_1^0 test of xy_GradLat_w',                         &
    answer = xy_ddata,                                            &
    check = xy_GradLat_w(w_xy(xy_data1)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_2^1 cosφ のテスト ----
  xy_data1 = sin(xy_Lat)*cos(xy_Lat)**2 * cos(xy_Lon) ! Y_2^1 cosφ

  xy_ddata = -sin(xy_Lat)*cos(xy_Lat)*sin(xy_Lon)    ! w_DivLon_xy
  call AssertEqual(&
    message='Y_2^1 cosφ test of xy_DivLon_w',                    &
    answer = xy_ddata,                                            &
    check = xy_w(w_DivLon_xy(xy_data1)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = cos(xy_Lat)*(1-4*sin(xy_Lat)**2)*cos(xy_Lon)      ! w_DivLat_w
  call AssertEqual(&
    message='Y_2^1 cosφ test of xy_DivLat_w',                    &
    answer = xy_ddata,                                            &
    check = xy_w(w_DivLat_xy(xy_data1)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Jacobian のテスト ----
  xy_data1 = -cos(xy_Lat)*sin(xy_Lon)                ! Y_1^{-1}
  xy_data2 = sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)   ! Y_2^1

  xy_ddata = 0.0
  call AssertEqual(&
    message='Y_1^-1 test of w_Jacobian_w_w',                      &
    answer = xy_ddata,                                            &
    check = xy_w(w_Jacobian_w_w(w_xy(xy_data1),w_xy(xy_data1))),  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = sin(xy_Lat)**2 - cos(xy_Lat)**2*cos(xy_Lon)**2
  call AssertEqual(&
    message='Y_1^-1 and Y_2^1 test of w_Jacobian_w_w',            &
    answer = xy_ddata,                                            &
    check = xy_w(w_Jacobian_w_w(w_xy(xy_data1),w_xy(xy_data2))),  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !============== 微分計算 (λ,μ座標系用) のテスト ==============
  mu = sin(xy_Lat)

  !----- Y_2^0 のテスト -----
  xy_data1 = 3*mu**2-1                              ! Y_2^0

  xy_ddata = 0.0
  call AssertEqual(&
    message='Y_2^0 test of xy_GradLambda_w',                      &
    answer = xy_ddata,                                            &
    check = xy_GradLambda_w(w_xy(xy_data1)),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = 6*mu*(1-mu**2)
  call AssertEqual(&
    message='Y_2^0 test of xy_GradMu_w',                          &
    answer = xy_ddata,                                            &
    check = xy_GradMu_w(w_xy(xy_data1)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !----- Y_2^0 cosφ のテスト -----
  xy_data1 = (3*mu**2-1)*(1-mu**2)                ! Y_2^0 (1-μ^2)

  xy_ddata = 0.0
  call AssertEqual(&
    message='Y_2^0 (1-μ^2) test of xy_DivLambda_w',                 &
    answer = xy_ddata,                                            &
    check = xy_w(w_DivLambda_xy(xy_data1)),                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = (2-3*mu**2)*4*mu
  call AssertEqual(&
    message='Y_2^0 (1-μ^2) test of xy_DivMu_w',                  &
    answer = xy_ddata,                                            &
    check = xy_w(w_DivMu_xy(xy_data1)),                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','w_deriv_module_sjpack_test', &
                         'w_deriv_module_sjpack function tests succeeded!') 

end program w_deriv_module_sjpack_test

