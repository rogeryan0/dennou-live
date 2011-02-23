!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ef_module �ƥ��ȥץ���� (��ʬ)
!
!����  2009/12/11  �ݹ�����
!
program ef_module_integral_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ef_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! �ʻҥǡ���
  real(8)            :: x_Data(0:im-1)            ! �ʻҥǡ���
  real(8)            :: y_Data(0:jm-1)            ! �ʻҥǡ���

  integer            :: k=2,l=1

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: yrad = 2.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! Ƚ�������
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  call MessageNotify('M','ef_module_integeral_test', &
       'ef_module integral function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call ef_initial(im,jm,km,lm,xmin,xmax,yrad)    ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  yx_Data = sin(k*pi*yx_X)**2 * exp(-yx_Y**2)

  y_Data = (xmax-xmin)/2 * exp(-y_Y**2)
  call AssertEqual(&
    message='y_IntX_yx with sin(k*pi*X)*exp(-Y**2)',              &
    answer = y_Data,                                              &
    check = y_IntX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Data = sqrt(PI)*sin(k*pi*x_X)**2
  call AssertEqual(&
    message='x_IntY_yx with sin(k*pi*X)*exp(-Y**2)',              &
    answer = x_Data,                                              &
    check = x_IntY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='IntYX_yx with sin(k*pi*X)*exp(-Y**2)',              &
    answer = (xmax-xmin)/2*sqrt(PI),                             &
    check = IntYX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore    &
    )

  call AssertEqual(&
    message='IntX_x with sqrt(PI)*sin(k*pi*X)**2',               &
    answer = (xmax-xmin)/2*sqrt(PI),                             &
    check = IntX_x(x_Data),                                      &
    significant_digits = check_digits, ignore_digits = ignore    &
    )

  call AssertEqual(&
    message='IntY_y with (xmax-xmin)/2*exp(-Y**2)',              &
    answer = (xmax-xmin)/2*sqrt(PI),                             &
    check = IntY_y(y_Data),                                      &
    significant_digits = check_digits, ignore_digits = ignore    &
    )

  yx_Data = cos(k*pi*yx_X)**2 * (sin(l*pi*yx_Y)+1.0D0)

  y_Data = 0.5D0 * (sin(l*pi*y_Y)+1.0D0)
  call AssertEqual(&
    message='y_AvrX_yx with cos(k*pi*X)**2*(sin(l*pi*Y)+1)',      &
    answer = y_Data,                                              &
    check = y_AvrX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Data = cos(k*pi*x_X)**2
  call AssertEqual(&
    message='x_AvrY_yx with cos(k*pi*X)**2*(sin(l*pi*Y)+1)',      &
    answer = x_Data,                                              &
    check = x_AvrY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='AvrYX_yx with cos(k*pi*X)**2*(sin(l*pi*Y)+1)',      &
    answer = 0.5D0,                                              &
    check = AvrYX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore    &
    )

  call AssertEqual(&
    message='AvrX_x with cos(k*pi*X)**2',                        &
    answer = 0.5D0,                                              &
    check = AvrX_x(x_Data),                                      &
    significant_digits = check_digits, ignore_digits = ignore    &
    )

  call AssertEqual(&
    message='AvrY_y with 0.5*(sin(l*pi*Y)+1)',                   &
    answer = 0.5D0,                                              &
    check = AvrY_y(y_Data),                                      &
    significant_digits = check_digits, ignore_digits = ignore    &
    )

  call MessageNotify('M','ef_module_integeral_test', &
       'ef_module integral function tests succeeded!')

end program ef_module_integral_test

