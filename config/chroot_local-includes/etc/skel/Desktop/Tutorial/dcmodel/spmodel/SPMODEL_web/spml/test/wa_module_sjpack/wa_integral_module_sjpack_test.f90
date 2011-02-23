!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_integrale_module テストプログラム :: 積分・平均関数のテスト
!
!履歴  2009/09/06  竹広真一 wa_test_deriv.f90 より SJPACK 用に改造
!
program wa_integral_module_sjpack

  use dc_message, only : MessageNotify
  use wa_module
  use dc_test, only : AssertEqual
  use dc_trace, only : SetDebug
  implicit none

  integer, parameter :: im=128, jm=64, nm=42, km=2

  real(8), dimension(0:im-1,1:jm,km) ::  xya_data            ! 元の関数
  real(8), dimension(0:im-1,km)      ::  xa_idata            ! 積分・平均の正解
  real(8), dimension(1:jm,km)        ::  ya_idata            ! 積分・平均の正解
  real(8), dimension(km)             ::  a_idata             ! 積分・平均の正解

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','wa_test_integral', &
                         'wa_integral_module function tests') 

  call wa_Initial( nm, im, jm, km )

  !---- 積分のテスト ----
  xya_data(:,:,1) = sin(xy_Lat)**2*(1-cos(xy_Lon)**2)
  xya_data(:,:,2) = sin(xy_Lat)**4*(1-sin(xy_Lon)**2)

  a_idata(1) = 2*pi/3       ! a_IntLonLat_xya
  a_idata(2) = 2*pi/5       ! a_IntLonLat_xya
  call AssertEqual(&
    message='a_IntLonLat_xya',                                    &
    answer = a_IntLonLat_xya(xya_data),                           &
    check =  a_idata,                                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  ya_idata(:,1) = pi*sin(y_Lat)**2       ! y_IntLon_xya
  ya_idata(:,2) = pi*sin(y_Lat)**4       ! y_IntLon_xya
  call AssertEqual(&
    message='ya_IntLon_xya',                                      &
    answer = ya_IntLon_xya(xya_data),                             &
    check =  ya_idata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='a_IntLat_ya',                                        &
    answer = a_IntLat_ya(ya_idata),                               &
    check =  a_idata,                                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xa_idata(:,1) = 2.0D0/3*(1-cos(x_Lon)**2)       ! x_IntLat_xya
  xa_idata(:,2) = 2.0D0/5*(1-sin(x_Lon)**2)       ! x_IntLat_xya

  call AssertEqual(&
    message='xa_IntLat_xya',                                      &
    answer = xa_IntLat_xya(xya_data),                             &
    check =  xa_idata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='a_IntLon_xa',                                        &
    answer = a_IntLon_xa(xa_idata),                               &
    check =  a_idata,                                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 平均のテスト ----
  xya_data(:,:,1) = sin(xy_Lat)**2*(1-cos(xy_Lon)**2)
  xya_data(:,:,2) = sin(xy_Lat)**4*(1-sin(xy_Lon)**2)

  a_idata(1) = 1.0D0/6       ! AvrLonLat_xya
  a_idata(2) = 1.0D0/10      ! AvrLonLat_xya
  call AssertEqual(&
    message='a_AvrLonLat_xya',                                    &
    answer = a_AvrLonLat_xya(xya_data),                           &
    check =  a_idata,                                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  ya_idata(:,1) = 0.5*sin(y_Lat)**2       ! y_AvrLon_xya
  ya_idata(:,2) = 0.5*sin(y_Lat)**4       ! y_AvrLon_xya
  call AssertEqual(&
    message='ya_AvrLon_xya',                                      &
    answer = ya_AvrLon_xya(xya_data),                             &
    check =  ya_idata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='a_AvrLat_ya',                                        &
    answer = a_AvrLat_ya(ya_idata),                               &
    check =  a_idata,                                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xa_idata(:,1) = 1.0D0/3*(1-cos(x_Lon)**2)       ! x_AvrLat_xya
  xa_idata(:,2) = 1.0D0/5*(1-sin(x_Lon)**2)       ! x_AvrLat_xya

  call AssertEqual(&
    message='xa_AvrLat_xya',                                      &
    answer = xa_AvrLat_xya(xya_data),                             &
    check =  xa_idata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='a_AvrLon_xa',                                        &
    answer = a_AvrLon_xa(xa_idata),                               &
    check =  a_idata,                                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wa_integral_module_sjpack_test', &
                         'wa_integral_module_sjpack function tests succeded!') 

end program wa_integral_module_sjpack

