!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wa_interpolate_module_sjpack テストプログラム :: 補間関数のテスト
!
!履歴  2009/09/06  竹広真一 wa_test_interpolate00.f90 より SJPACK 用に改造
!
program wa_interpolate_module_sjpack

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wa_module_sjpack
  implicit none

  integer, parameter :: im=32, jm=16, nm=10, km=3

  real(8), dimension((nm+1)**2,km) ::  wa_data
  real(8), dimension(0:im-1,1:jm,km)     ::  xya_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  real(8) :: Alon
  real(8) :: Alat
  real(8), dimension(km) :: a_data_sol
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0
  Alon = pi/5 ; Alat = pi/3

  call wa_Initial( nm, im, jm, km )

  call MessageNotify('M','wa_test_Interpolate', &
                         'wa_interpolate_module interpolate(00) tests') 


  !---- Y_1^* のテスト ----
  xya_data(:,:,1) = sqrt(3.0D0)*sin(xy_Lat)                      ! Y_1^0
  a_data_sol(1) = sqrt(3.0D0)*sin(ALat)

  xya_data(:,:,2) = sqrt(3.0D0/2)*cos(xy_Lat)*cos(xy_Lon)          ! Y_1^1
  a_data_sol(2) = sqrt(3.0D0/2)*cos(ALat)*cos(ALon)

  xya_data(:,:,3)   = -sqrt(3.0D0)*cos(xy_Lat)*sin(xy_Lon)         ! Y_1^{-1}
  a_data_sol(3) = -sqrt(3.0D0)*cos(ALat)*sin(ALon)

  wa_data = wa_xya(xya_data)

  call AssertEqual(&
    message='a_Interpolate_wa with Y_1^*',                        &
    answer = a_Interpolate_wa(wa_data,alon,alat),                 &
    check =  a_data_sol,                                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_2^* のテスト ----
  xya_data(:,:,1) = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)            ! Y_2^0
  a_data_sol(1) = sqrt(5.0D0)*(3.0/2*sin(alat)**2-1.0/2)
  xya_data(:,:,2) = sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)!Y_2^1
  a_data_sol(2) = sqrt(5.0D0/6)*3.0*sin(ALat)*cos(ALat) * cos(ALon)
  xya_data(:,:,3) = -sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*sin(2*xy_Lon)    ! Y_2^-2
  a_data_sol(3) = -sqrt(5.0D0/24)*3.0*cos(ALat)**2*sin(2*ALon)

  wa_data = wa_xya(xya_data)

  call AssertEqual(&
    message='a_Interpolate_wa with Y_2^*',                        &
    answer = a_Interpolate_wa(wa_data,alon,alat),                 &
    check =  a_data_sol,                                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 一般的なのテスト ----
  xya_data(:,:,1) = cos(2*xy_Lon-pi/3) &
       *(sin(xy_Lat)-1)**2*(sin(xy_Lat)-0.5)*(sin(xy_Lat)+1)
  a_data_sol(1) = cos(2*ALon-pi/3) &
       *(sin(ALat)-1)**2*(sin(ALat)-0.5)*(sin(ALat)+1)
  xya_data(:,:,2) = sin(2*xy_Lon-pi/3)*(sin(xy_Lat)-1)**2*(sin(xy_Lat)+1)**2
  a_data_sol(2) = sin(2*aLon-pi/3)*(sin(aLat)-1)**2*(sin(aLat)+1)**2
  xya_data(:,:,3) = sin(2*xy_Lon+pi/3)*(sin(xy_Lat)-1)**3*(sin(xy_Lat)+1)
  a_data_sol(3) = sin(2*aLon+pi/3)*(sin(aLat)-1)**3*(sin(aLat)+1)

  wa_data = wa_xya(xya_data)

  call AssertEqual(&
    message='a_Interpolate_wa with a general function',           &
    answer = a_Interpolate_wa(wa_data,alon,alat),                 &
    check =  a_data_sol,                                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wa_Interpolate_module_sjpack_test', &
                      'wa_interpolate_module_sjpack function tests succeeded') 

end program wa_interpolate_module_sjpack
