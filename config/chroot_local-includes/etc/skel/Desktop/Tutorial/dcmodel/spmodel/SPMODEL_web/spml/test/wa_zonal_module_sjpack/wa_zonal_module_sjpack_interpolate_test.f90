!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wa_zonal_module_sjpack テストプログラム :: 補間関数のテスト
!
!履歴  2009/09/24  竹広真一
!
program wa_zonal_module_interpolate_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wa_zonal_module_sjpack
  implicit none

  integer, parameter :: im=1, jm=16, nm=10, km=3

  real(8), dimension(nm+1,km)           ::  wa_data
  real(8), dimension(0:im-1,1:jm,km)    ::  xya_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  real(8) :: Alon
  real(8) :: Alat
  real(8), dimension(km) :: a_data_sol
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0
  Alon = pi/5 ; Alat = pi/3

  call wa_Initial( nm, im, jm, km )

  call MessageNotify('M','wa_zonal_module_sjpack_interpolate_test', &
                         'wa_zonal_module_sjpack interpolate function tests') 


  !---- Y_*^0 のテスト ----
  xya_data(:,:,1) = sqrt(3.0D0)*sin(xy_Lat)                      ! Y_1^0
  a_data_sol(1) = sqrt(3.0D0)*sin(ALat)
  xya_data(:,:,2) = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)     ! Y_2^0
  a_data_sol(2) = sqrt(5.0D0)*(3.0/2*sin(alat)**2-1.0/2)
  xya_data(:,:,3)  = sqrt(7.0D0)*(5.0/2*sin(xy_Lat)**3-3*sin(xy_Lat)/2.0) ! P_3
  a_data_sol(3) = sqrt(7.0D0)*(5.0/2*sin(alat)**3-3.0*sin(alat)/2)

  wa_data = wa_xya(xya_data)

  call AssertEqual(&
    message='Y_*^0 Test of interpolate function',                 &
    answer = a_data_sol,                                          &
    check = a_Interpolate_wa(wa_data,alon,alat),                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 一般的な関数のテスト ----
  xya_data(:,:,1) = (sin(xy_Lat)-1)**2*(sin(xy_Lat)-0.5)*(sin(xy_Lat)+1)
  a_data_sol(1) = (sin(ALat)-1)**2*(sin(ALat)-0.5)*(sin(ALat)+1)
  xya_data(:,:,2) =(sin(xy_Lat)-1)**2*(sin(xy_Lat)+1)**2
  a_data_sol(2) = (sin(aLat)-1)**2*(sin(aLat)+1)**2
  xya_data(:,:,3) = (sin(xy_Lat)-1)**3*(sin(xy_Lat)+1)
  a_data_sol(3) = (sin(aLat)-1)**3*(sin(aLat)+1)

  wa_data = wa_xya(xya_data)

  call AssertEqual(&
    message='general function Test of interpolate function',      &
    answer = a_data_sol,                                          &
    check = a_Interpolate_wa(wa_data,alon,alat),                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wa_zonal_module_sjpack_interpolate_test', &
       'wa_zonal_module_sjpack interpolate function tests succeeded!') 

end program wa_zonal_module_interpolate_test

