!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_zonal_module_sjpack テストプログラム :: 補間関数のテスト
!
!履歴  2009/09/22  竹広真一
!
program w_zonal_module_sjpack_interpolate_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use w_zonal_module_sjpack
  implicit none

  integer, parameter :: im=1, jm=16, nm=10

  real(8), dimension(nm+1)        ::  w_data
  real(8), dimension(0:im-1,1:jm) ::  xy_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  real(8) :: Alon
  real(8) :: Alat
  real(8) :: xy_data_sol
  real(8) :: error
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0
  Alon = pi/5 ; Alat = pi/3

  call w_Initial( nm, im, jm )

  call MessageNotify('M','w_zonal_module_sjpack_interpolate_test', &
                         'w_zonal_module_sjpack interpolate function tests') 


  !---- Y_1^0 のテスト ----
  xy_data = sqrt(3.0D0)*sin(xy_Lat)                      ! Y_1^0
  xy_data_sol = sqrt(3.0D0)*sin(ALat)
  w_data = w_xy(xy_data)

  call AssertEqual(&
    message='Y_1^0 Test of interpolate function',                 &
    answer = xy_data_sol,                                         &
    check = Interpolate_w(w_data,alon,alat),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
 
  !---- Y_2^0 のテスト ----
  xy_data = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)                 ! Y_2^0
  xy_data_sol = sqrt(5.0D0)*(3.0/2*sin(alat)**2-1.0/2)
  w_data = w_xy(xy_data)

  call AssertEqual(&
    message='Y_2^0 Test of interpolate function',                 &
    answer = xy_data_sol,                                         &
    check = Interpolate_w(w_data,alon,alat),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 一般的な関数のテスト ----
  xy_data = (sin(xy_Lat)-1)**2*(sin(xy_Lat)-0.5)*(sin(xy_Lat)+1)
  xy_data_sol = (sin(ALat)-1)**2*(sin(ALat)-0.5)*(sin(ALat)+1)
  w_data = w_xy(xy_data)

  call AssertEqual(&
    message='general function Test of interpolate function',      &
    answer = xy_data_sol,                                         &
    check = Interpolate_w(w_data,alon,alat),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','w_zonal_module_sjpack_interpolate_test', &
       'w_zonal_module_sjpack interpolate function tests succeeded!') 

end program w_zonal_module_sjpack_interpolate_test
