!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wq_module_sjpack テストプログラム
!      積分・平均計算
!
!履歴  2009/12/09  竹広真一  wq_test_intavr.f90 より SJPACK 用に改造
!
program wq_module_sjpack_integral_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wq_module_sjpack

  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=31         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ra=1.5D0             ! 球半径

  real(8), dimension(0:im-1,1:jm,km)       :: xyr_Data
  real(8), dimension(0:im-1,1:jm)          :: xy_Data
  real(8), dimension(1:jm,km)              :: yr_Data
  real(8), dimension(0:im-1,km)            :: xr_Data
  real(8), dimension(0:im-1)               :: x_Data
  real(8), dimension(1:jm)                 :: y_Data
  real(8), dimension(km)                   :: r_Data

  real(8), parameter  :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 4
  integer, parameter :: ignore = -5


  !=============================== 積分 =================================

  call MessageNotify('M','wq_module_sjpack_integral_test', &
       'wq_module_sjpack integration functions tests')

  call wq_Initial(im,jm,km,nm,lm,ra)

 ! 定数
  xyr_Data = 1.0D0
  yr_Data = 2*pi
  call AssertEqual(&
       message='yr_IntLon_xyr with constant field',                &
       answer = yr_Data,                                           &
       check = yr_IntLon_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xr_Data = 2.0D0
  call AssertEqual(&
       message='xr_IntLat_xyr with constant field',                &
       answer = xr_Data,                                           &
       check = xr_IntLat_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xy_Data = 1/3.0D0*ra**3
  call AssertEqual(&
       message='xy_IntRad_xyr with constant field',                &
       answer = xy_Data,                                           &
       check = xy_IntRad_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  r_Data = 4*pi
  call AssertEqual(&
       message='r_IntLonLat_xyr with constant field',              &
       answer = r_Data,                                            &
       check = r_IntLonLat_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  x_Data = 2/3.0D0*ra**3
  call AssertEqual(&
       message='x_IntLatRad_xyr with constant field',              &
       answer = x_Data,                                            &
       check = x_IntLatRad_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  y_Data = 2*pi/3.0D0*ra**3
  call AssertEqual(&
       message='y_IntLonRad_xyr with constant field',              &
       answer = y_Data,                                            &
       check = y_IntLonRad_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  call AssertEqual(&
       message='IntLonLatRad_xyr with constant field',             &
       answer = 4*pi/3*ra**3,                                      &
       check = IntLonLatRad_xyr(xyr_Data),                         &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

 ! r^2 
  xyr_Data = xyr_Rad**2

  yr_Data = 2*pi* xyr_Rad(1,:,:)**2
  call AssertEqual(&
       message='yr_IntLon_xyr with r^2',                           &
       answer = yr_Data,                                           &
       check = yr_IntLon_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xr_Data = 2.0D0 * xyr_Data(:,1,:)
  call AssertEqual(&
       message='xr_IntLat_xyr with r^2',                           &
       answer = xr_Data,                                           &
       check = xr_IntLat_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xy_Data = 1/5.0D0*ra**5
  call AssertEqual(&
       message='xy_IntRad_xyr with r^2',                           &
       answer = xy_Data,                                           &
       check = xy_IntRad_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  r_Data = 4*pi * xyr_Data(1,1,:)
  call AssertEqual(&
       message='r_IntLonLat_xyr with r^2',                         &
       answer = r_Data,                                            &
       check = r_IntLonLat_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  x_Data = 2/5.0D0*ra**5
  call AssertEqual(&
       message='x_IntLatRad_xyr with r^2',                         &
       answer = x_Data,                                            &
       check = x_IntLatRad_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  y_Data = 2*pi/5.0D0*ra**5
  call AssertEqual(&
       message='y_IntLonRad_xyr with r^2',                         &
       answer = y_Data,                                            &
       check = y_IntLonRad_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  call AssertEqual(&
       message='IntLonLatRad_xyr with r^2',                        &
       answer = 4*pi/5*ra**5,                                      &
       check = IntLonLatRad_xyr(xyr_Data),                         &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  ! sin(phi)**2 
  xyr_Data = sin(xyr_Lat)**2

  yr_Data = 2*pi* xyr_Data(1,:,:)
  call AssertEqual(&
       message='yr_IntLon_xyr with sin(phi)**2',                   &
       answer = yr_Data,                                           &
       check = yr_IntLon_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xr_Data = 2.0D0/3.0D0
  call AssertEqual(&
       message='xr_IntLat_xyr with sin(phi)**2',                   &
       answer = xr_Data,                                           &
       check = xr_IntLat_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xy_Data = 1/3.0D0*ra**3 * xyr_Data(:,:,1)
  call AssertEqual(&
       message='xy_IntRad_xyr with sin(phi)**2',                   &
       answer = xy_Data,                                           &
       check = xy_IntRad_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  r_Data = 4*pi/3.0
  call AssertEqual(&
       message='r_IntLonLat_xyr with sin(phi)**2',                 &
       answer = r_Data,                                            &
       check = r_IntLonLat_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  x_Data = 2/9.0D0*ra**3
  call AssertEqual(&
       message='x_IntLatRad_xyr with sin(phi)**2',                 &
       answer = x_Data,                                            &
       check = x_IntLatRad_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  y_Data = 2*pi/3.0D0*ra**3 * xyr_Data(1,:,1)
  call AssertEqual(&
       message='y_IntLonRad_xyr with sin(phi)**2',                 &
       answer = y_Data,                                            &
       check = y_IntLonRad_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  call AssertEqual(&
       message='IntLonLatRad_xyr with sin(phi)**2',                &
       answer = 4*pi/9*ra**3,                                      &
       check = IntLonLatRad_xyr(xyr_Data),                         &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  !=============================== 平均 =================================

  call MessageNotify('M','wq_module_sjpack_integral_test', &
       'wq_module_sjpack averaging functions tests')

 ! 定数
  xyr_Data = 1.0

  yr_Data = 1.0D0
  call AssertEqual(&
       message='yr_AvrLon_xyr with constant field',                &
       answer = yr_Data,                                           &
       check = yr_AvrLon_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xr_Data = 1.0D0
  call AssertEqual(&
       message='xr_AvrLat_xyr with constant field',                &
       answer = xr_Data,                                           &
       check = xr_AvrLat_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xy_Data = 1.0D0
  call AssertEqual(&
       message='xy_AvrRad_xyr with constant field',                &
       answer = xy_Data,                                           &
       check = xy_AvrRad_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  r_Data = r_AvrLonLat_xyr(xyr_Data)
  call AssertEqual(&
       message='r_AvrLonLat_xyr with constant field',              &
       answer = r_Data,                                            &
       check = r_AvrLonLat_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  x_Data = 1.0D0
  call AssertEqual(&
       message='x_AvrLatRad_xyr with constant field',              &
       answer = x_Data,                                            &
       check = x_AvrLatRad_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  y_Data = 1.0D0
  call AssertEqual(&
       message='y_AvrLonRad_xyr with constant field',              &
       answer = y_Data,                                            &
       check = y_AvrLonRad_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  call AssertEqual(&
       message='AvrLonLatRad_xyr with constant field',             &
       answer = 1.0D0,                                             &
       check = AvrLonLatRad_xyr(xyr_Data),                         &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

 ! r 
  xyr_Data = xyr_Rad 

  yr_Data = xyr_Data(1,:,:)
  call AssertEqual(&
       message='yr_AvrLon_xyr with r',                             &
       answer = yr_Data,                                           &
       check = yr_AvrLon_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xr_Data = xyr_Data(:,1,:)
  call AssertEqual(&
       message='xr_AvrLat_xyr with r',                             &
       answer = xr_Data,                                           &
       check = xr_AvrLat_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xy_Data = 1.0D0/4.0D0*ra**4/(1.0D0/3.0D0*ra**3)
  call AssertEqual(&
       message='xy_AvrRad_xyr with r',                             &
       answer = xy_Data,                                           &
       check = xy_AvrRad_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  r_Data = xyr_Data(1,1,:)
  call AssertEqual(&
       message='r_AvrLonLat_xyr with r',                           &
       answer = r_Data,                                            &
       check = r_AvrLonLat_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  x_Data = 1/2.0D0*ra**4/(2/3.0D0*ra**3)
  call AssertEqual(&
       message='x_AvrLatRad_xyr with r',                           &
       answer = x_Data,                                            &
       check = x_AvrLatRad_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  y_Data = pi/2.0D0*ra**4/(2*pi/3.0D0*ra**3)
  call AssertEqual(&
       message='y_AvrLonRad_xyr with r',                           &
       answer = y_Data,                                            &
       check = y_AvrLonRad_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  call AssertEqual(&
       message='AvrLonLatRad_xyr with r',                          &
       answer = pi*ra**4/(4*pi/3*ra**3),                           &
       check = AvrLonLatRad_xyr(xyr_Data),                         &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  ! sin(phi)**2
  xyr_Data = sin(xyr_Lat)**2

  yr_Data = xyr_Data(1,:,:)
  call AssertEqual(&
       message='yr_AvrLon_xyr with sin(phi)**2',                   &
       answer = yr_Data,                                           &
       check = yr_AvrLon_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xr_Data = 1.0D0/3.0D0
  call AssertEqual(&
       message='xr_AvrLat_xyr with sin(phi)**2',                   &
       answer = xr_Data,                                           &
       check = xr_AvrLat_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xy_Data = xyr_Data(:,:,1)
  call AssertEqual(&
       message='xy_AvrRad_xyr with sin(phi)**2',                   &
       answer = xy_Data,                                           &
       check = xy_AvrRad_xyr(xyr_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  r_Data = 1.0D0/3.0D0
  call AssertEqual(&
       message='r_AvrLonLat_xyr with sin(phi)**2',                 &
       answer = r_Data,                                            &
       check = r_AvrLonLat_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  x_Data = 1.0D0/3.0D0
  call AssertEqual(&
       message='x_AvrLatRad_xyr with sin(phi)**2',                 &
       answer = x_Data,                                            &
       check = x_AvrLatRad_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  y_Data = xyr_Data(1,:,1)
  call AssertEqual(&
       message='y_AvrLonRad_xyr with sin(phi)**2',                 &
       answer = y_Data,                                            &
       check = y_AvrLonRad_xyr(xyr_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  call AssertEqual(&
       message='AvrLonLatRad_xyr with sin(phi)**2',                &
       answer = 1.0D0/3D0,                                         &
       check = AvrLonLatRad_xyr(xyr_Data),                         &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  call MessageNotify('M','wq_module_sjpack_integral_test', &
       'wq_module integration/averaging function tests succeeded!')

end program wq_module_sjpack_integral_test
