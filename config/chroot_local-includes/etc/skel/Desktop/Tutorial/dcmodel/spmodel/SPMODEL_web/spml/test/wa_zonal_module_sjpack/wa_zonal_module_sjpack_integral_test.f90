!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wa_zonal_module_sjpack テストプログラム :: 積分・平均関数のテスト
!
!履歴  2009/09/24  竹広真一
!
program wa_zonal_module_sjpack_integral_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wa_zonal_module_sjpack
  use dc_trace, only : SetDebug
  implicit none

  integer, parameter :: im=1, jm=64, nm=42, km=2

  real(8), dimension(0:im-1,1:jm,km) ::  xya_data            ! 元の関数
  real(8), dimension(0:im-1,km)      ::  xa_idata            ! 積分・平均の正解
  real(8), dimension(1:jm,km)        ::  ya_idata            ! 積分・平均の正解
  real(8), dimension(km)             ::  a_idata             ! 積分・平均の正解

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','wa_zonal_module_sjpack_integral_test', &
                         'wa_zonal_module_sjpack integral/average function tests') 

  call wa_Initial( nm, im, jm, km )

  !---- 積分のテスト ----
  xya_data(:,:,1) = sin(xy_Lat)**2
  xya_data(:,:,2) = sin(xy_Lat)**4

  a_idata(1) = 4*pi/3       ! a_IntLonLat_xya
  a_idata(2) = 4*pi/5       ! a_IntLonLat_xya
  call AssertEqual(&
    message='a_IntLonLat_xya',                                    &
    answer = a_idata,                                             &
    check = a_IntLonLat_xya(xya_data),                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  ya_idata(:,1) = 2*pi*sin(y_Lat)**2       ! y_IntLon_xya
  ya_idata(:,2) = 2*pi*sin(y_Lat)**4       ! y_IntLon_xya
  call AssertEqual(&
    message='ya_IntLon_xya',                                      &
    answer = ya_idata,                                            &
    check = ya_IntLon_xya(xya_data),                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='a_IntLat_ya',                                        &
    answer = a_idata,                                             &
    check = a_IntLat_ya(ya_idata),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xa_idata(:,1) = 2.0D0/3       ! x_IntLat_xya
  xa_idata(:,2) = 2.0D0/5       ! x_IntLat_xya
  call AssertEqual(&
    message='xa_IntLat_xya',                                      &
    answer = xa_idata,                                            &
    check = xa_IntLat_xya(xya_data),                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='a_IntLon_xa',                                        &
    answer = a_idata,                                             &
    check = a_IntLon_xa(xa_idata),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 平均のテスト ----
  xya_data(:,:,1) = sin(xy_Lat)**2
  xya_data(:,:,2) = sin(xy_Lat)**4

  a_idata(1) = 1.0D0/3       ! AvrLonLat_xya
  a_idata(2) = 1.0D0/5       ! AvrLonLat_xya
  call AssertEqual(&
    message='a_AvrLonLat_xya',                                    &
    answer = a_idata,                                             &
    check = a_AvrLonLat_xya(xya_data),                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  ya_idata(:,1) = sin(y_Lat)**2       ! y_AvrLon_xya
  ya_idata(:,2) = sin(y_Lat)**4       ! y_AvrLon_xya
  call AssertEqual(&
    message='ya_AvrLon_xya',                                      &
    answer = ya_idata,                                            &
    check = ya_AvrLon_xya(xya_data),                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='a_AvrLat_ya',                                        &
    answer = a_idata,                                             &
    check = a_AvrLat_ya(ya_idata),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xa_idata(:,1) = 1.0D0/3       ! x_AvrLat_xya
  xa_idata(:,2) = 1.0D0/5       ! x_AvrLat_xya
  call AssertEqual(&
    message='xa_AvrLat_xya',                                      &
    answer = xa_idata,                                            &
    check = xa_AvrLat_xya(xya_data),                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='a_AvrLon_xa',                                        &
    answer = a_idata,                                             &
    check = a_AvrLon_xa(xa_idata),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wa_zonal_module_sjpack_integral_test', &
       'wa_zonal_module_sjpack integral/average function tests succeeded!') 

end program wa_zonal_module_sjpack_integral_test

