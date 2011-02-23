!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_zonal_module_sjpack テストプログラム :: 微分関数のテスト
!
!履歴  2009/09/22  竹広真一
!
program w_zonal_module_sjpack_deriv_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use w_zonal_module_sjpack
  implicit none

  integer, parameter :: im=1, jm=16, nm=10

  real(8), dimension(0:im-1,1:jm)  ::  xy_data1              ! 元の関数
  real(8), dimension(0:im-1,1:jm)  ::  xy_data2              ! 元の関数
  real(8), dimension(0:im-1,1:jm)  ::  xy_ddata              ! 微分の正解
  real(8), dimension(0:im-1,1:jm)  ::  mu                    ! μ=sinφ

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  real(8)                          ::  pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','w_zonal_module_sjpack_deriv_test', &
                         'w_zonal_module_sjpack derivative function tests') 

  call w_Initial( nm, im, jm )

  !---- Y_1^0 のテスト ----
  xy_data1 = sin(xy_Lat)                         ! Y_1^0

  xy_ddata = -2.0D0*sin(xy_Lat)                  ! w_Lapla_w
  call AssertEqual(&
    message='Y_1^0 Test of w_Lapla_w',                            &
    answer = xy_ddata,                                            &
    check = xy_w(w_Lapla_w(w_xy(xy_data1))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = -1.0D0/2.0D0*sin(xy_Lat)            ! w_LaplaInv_w
  call AssertEqual(&
    message='Y_1^0 Test of w_LaplaInv_w',                         &
    answer = xy_ddata,                                            &
    check = xy_w(w_LaplaInv_w(w_xy(xy_data1))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = 0.0D0                           ! w_DLon_w
  call AssertEqual(&
    message='Y_1^0 Test of w_DLon_w',                             &
    answer = xy_ddata,                                            &
    check = xy_w(w_DLon_w(w_xy(xy_data1))),                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = 0.0D0                           ! xy_GradLon_w
  call AssertEqual(&
    message='Y_1^0 Test of xy_GradLon_w',                         &
    answer = xy_ddata,                                            &
    check = xy_GradLon_w(w_xy(xy_data1)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = cos(xy_Lat)                    ! xy_GradLat_w
  call AssertEqual(&
    message='Y_1^0 Test of xy_GradLat_w',                         &
    answer = xy_ddata,                                            &
    check = xy_GradLat_w(w_xy(xy_data1)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_2^0 cosφ のテスト ----
  xy_data1 = (3.0D0/2.0D0*sin(xy_Lat)**2 - 1.0D0/2.0D0)*cos(xy_Lat)
  ! Y_2^0 cosφ

  xy_ddata = 0.0D0                          ! w_DivLon_xy
  call AssertEqual(&
    message='Y_2^0 cos φ Test of w_DivLon_xy',                   &
    answer = xy_ddata,                                            &
    check = xy_w(w_DivLon_xy(xy_data1)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = 3*sin(xy_Lat)*(cos(xy_Lat)**2-sin(xy_Lat)**2) + sin(xy_Lat)
  ! w_DivLat_w
  call AssertEqual(&
    message='Y_2^0 cos φ Test of w_DivLat_xy',                   &
    answer = xy_ddata,                                            &
    check = xy_w(w_DivLat_xy(xy_data1)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Jacobian のテスト ----
  xy_data1 = sin(xy_Lat)                             ! Y_1^0
  xy_data2 = 3.0D0/2.0D0*sin(xy_Lat)**2-1.0D0/2.0D0  ! Y_2^0

  xy_ddata = 0.0
  call AssertEqual(&
    message='Y_1^0 Test of w_Jacobian_w_w',                       &
    answer = xy_ddata,                                            &
    check = xy_w(w_Jacobian_w_w(w_xy(xy_data1),w_xy(xy_data1))),  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = 0.0D0
  call AssertEqual(&
    message='Y_1^0  and Y_2^0 Test of w_Jacobian_w_w',            &
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
    message='Y_2^0 Test of xy_GradLambda_w',                      &
    answer = xy_ddata,                                            &
    check = xy_GradLambda_w(w_xy(xy_data1)),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = 6*mu*(1-mu**2)
  call AssertEqual(&
    message='Y_2^0 Test of xy_GradMu_w',                          &
    answer = xy_ddata,                                            &
    check = xy_GradMu_w(w_xy(xy_data1)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !----- Y_2^0 cosφ のテスト -----
  xy_data1 = (3*mu**2-1)*(1-mu**2)                ! Y_2^0 (1-μ^2)

  xy_ddata = 0.0
  call AssertEqual(&
    message='Y_2^0 cos φ Test of w_DivLambda_xy',                &
    answer = xy_ddata,                                            &
    check = xy_w(w_DivLambda_xy(xy_data1)),                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_ddata = (2-3*mu**2)*4*mu
  call AssertEqual(&
    message='Y_2^0 cos φ Test of w_DivMu_xy',                    &
    answer = xy_ddata,                                            &
    check = xy_w(w_DivMu_xy(xy_data1)),                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','w_zonal_module_sjpack_deriv_test', &
       'w_zonal_module_sjpack derivative function tests succeeded!') 

end program w_zonal_module_sjpack_deriv_test
