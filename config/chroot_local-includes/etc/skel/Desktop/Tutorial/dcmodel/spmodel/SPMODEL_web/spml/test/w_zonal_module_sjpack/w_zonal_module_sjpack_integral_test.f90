!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  w_zonal_module_sjpack �ƥ��ȥץ���� :: ��ʬ��ʿ�Ѵؿ��Υƥ���
!
!����  2009/09/22  �ݹ�����
!
program w_zonal_module_sjpack_integral_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use w_zonal_module_sjpack
  implicit none

  integer, parameter :: im=1, jm=64, nm=42

  real(8), dimension(0:im-1,1:jm) ::  xy_data            ! ���δؿ�
  real(8), dimension(0:im-1)      ::  x_idata            ! ��ʬ��ʿ�Ѥ�����
  real(8), dimension(1:jm)        ::  y_idata            ! ��ʬ��ʿ�Ѥ�����
  real(8)                         ::  idata              ! ��ʬ��ʿ�Ѥ�����

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','w_module_sjpack_integral_test', &
                         'w_module_sjpack integral function tests') 

  call w_Initial( nm, im, jm )

  !---- ��ʬ�Υƥ��� ----
  xy_data = sin(xy_Lat)**2

  idata = 4*pi/3                     ! IntLonLat_xy
  call AssertEqual(&
    message='IntLonLat_xy',                                       &
    answer = idata,                                               &
    check = IntLonLat_xy(xy_data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_idata = 2*pi*sin(y_Lat)**2       ! y_IntLon_xy
  call AssertEqual(&
    message='IntLon_xy',                                          &
    answer = y_idata,                                             &
    check = y_IntLon_xy(xy_data),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='IntLat_y',                                           &
    answer = idata,                                               &
    check = IntLat_y(y_idata),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_idata = 2.0D0/3                  ! x_IntLat_xy

  call AssertEqual(&
    message='IntLon_xy',                                          &
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

  !---- ʿ�ѤΥƥ��� ----
  xy_data = sin(xy_Lat)**2

  idata = 1.0D0/3       ! AvrLonLat_xy
  call AssertEqual(&
    message='AvrLonLat_xy',                                       &
    answer = idata,                                               &
    check = AvrLonLat_xy(xy_data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_idata = sin(y_Lat)**2       ! y_AvrLon_xy
  call AssertEqual(&
    message='AvrLon_xy',                                          &
    answer = y_idata,                                             &
    check = y_AvrLon_xy(xy_data),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='AvrLat_y',                                           &
    answer = idata,                                               &
    check = AvrLat_y(y_idata),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_idata = 1.0D0/3             ! x_AvrLat_xy

  call AssertEqual(&
    message='AvrLon_xy',                                          &
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


  call MessageNotify('M','w_module_sjpack_integral_test', &
                         'w_module_sjpack integral function tests') 

end program w_zonal_module_sjpack_integral_test
