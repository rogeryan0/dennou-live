!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_zonal_module_sjpack テストプログラム :: 基本変換関数のテスト
!
!履歴  2009/09/22  竹広真一  w_zonal_test_base.f90 より改造
!
program w_zonal_module_sjpack_base_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use w_zonal_module_sjpack
  implicit none

  integer, parameter :: im=1, jm=64, nm=42

  real(8), dimension(nm+1)          ::  w_data
  real(8), dimension(0:im-1,1:jm)   ::  xy_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','w_zonal_module_sjpack_base_test', &
                         'w_zonal_module_sjpack basic functions tests') 

  call w_Initial( nm, im, jm )

  !---- Y_1^0 のテスト ----
  xy_data = sqrt(3.0D0)*sin(xy_Lat)                      ! 
  w_data= 0.0D0 ; w_data(l_nm(1,0))=1.0D0

  call AssertEqual(&
    message='Y_1^0 Test of w_xy(xy_data)',                        &
    answer = w_data,                                              &
    check = w_xy(xy_data),                                        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_1^0 Test of xy_w(w_xy(xy_data))',                  &
    answer = xy_data,                                             &
    check = xy_w(w_xy(xy_data)),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_2^0 のテスト ----
  xy_data = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)                 ! Y_2^0
  w_data= 0.0D0 ; w_data(l_nm(2,0))=1.0D0

  call AssertEqual(&
    message='Y_2^0 Test of w_xy(xy_data)',                        &
    answer = w_data,                                              &
    check = w_xy(xy_data),                                        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^0 Test of xy_w(w_xy(xy_data))',                  &
    answer = xy_data,                                             &
    check = xy_w(w_xy(xy_data)),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 一般的関数のテスト ----
  xy_data = (sin(xy_Lat)-1)**2*(sin(xy_Lat)-0.5)*(sin(xy_Lat)+1)

  call AssertEqual(&
    message='general function Test of xy_w(w_xy(xy_data))',       &
    answer = xy_data,                                             &
    check = xy_w(w_xy(xy_data)),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','w_zonal_module_sjpack_base_test', &
                         'w_zonal_module_sjpack basic functions tests succeeded!') 

end program w_zonal_module_sjpack_base_test
