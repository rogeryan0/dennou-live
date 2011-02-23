!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  la_module_sjpack �ƥ��ȥץ���� :: ��ʬ��ʿ�Ѵؿ��Υƥ���
!
!����  2009/09/22  �ݹ�����
!
program la_module_sjpack_integral_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use la_module_sjpack
  use dc_trace, only : SetDebug
  implicit none

  integer, parameter :: jm=64, nm=42, km=2

  real(8), dimension(1:jm,km)        ::  ya_data            ! ���δؿ�
  real(8), dimension(km)             ::  a_idata            ! ��ʬ��ʿ�Ѥ�����
  real(8), parameter                 ::  eps = 1.0D-10
  real(8) :: pi

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','la_module_sjpack_integral_test', &
                         'la_module_sjpack integral/average function tests') 

  call la_Initial( nm, jm )

  !---- ��ʬ�Υƥ��� ----
  ya_data(:,1) = pi*sin(y_Lat)**2
  ya_data(:,2) = pi*sin(y_Lat)**4

  a_idata(1) = 2*pi/3       ! a_IntLat_ya
  a_idata(2) = 2*pi/5       ! a_IntLat_ya

  call AssertEqual(&
    message='Test of a_IntLat_ya',                                &
    answer = a_idata,                                             &
    check = a_IntLat_ya(ya_data),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- ʿ�ѤΥƥ��� ----
  ya_data(:,1) = 0.5*sin(y_Lat)**2
  ya_data(:,2) = 0.5*sin(y_Lat)**4

  a_idata(1) = 1.0D0/6       ! a_AvrLonLat_ya
  a_idata(2) = 1.0D0/10      ! a_AvrLonLat_ya

  call AssertEqual(&
    message='Test of a_AvrLat_ya',                                &
    answer = a_idata,                                             &
    check = a_AvrLat_ya(ya_data),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','la_module_sjpack_integral_test', &
       'la_module_sjpack integral/average function tests succeeded!') 

end program la_module_sjpack_integral_test
