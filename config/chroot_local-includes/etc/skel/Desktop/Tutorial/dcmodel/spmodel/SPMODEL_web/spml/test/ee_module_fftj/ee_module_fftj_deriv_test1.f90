!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ee_module_fftj �ƥ��ȥץ���� (��ʬ�׻�)
!
!����  2009/09/09  �ݹ�����  ee_test_derivative.f90 ��� ee_module_fftj �Ѥ˲�¤
!
program ee_module_fftj_deriv_test1

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ee_module_fftj
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! �ʻҥǡ���
  real(8)            :: yx_Deriv(0:jm-1,0:im-1)   ! �ʻҥǡ���

  integer            :: k=2, l=3

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','ee_module_fftj_deriv_test1', &
       'ee_module_fftj derivative function tests')

  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

 !---------------- ��ɸ�ͤ����� ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

 !------------------- �ؿ������å� ----------------------
  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)

  yx_Deriv = k*pi*cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dx_ee with sin(k*pi*X)*sin(l*pi*Y)',              &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dx_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = l*pi*sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dy_ee with sin(k*pi*X)*sin(l*pi*Y)',              &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dy_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Lapla_ee with sin(k*pi*X)*sin(l*pi*Y)',           &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Lapla_ee(ee_yx(yx_Data))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_LaplaInv_ee with sin(k*pi*X)*sin(l*pi*Y)',        &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data))),                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(k*pi*yx_X) * cos(l*pi*yx_Y)

  yx_Deriv = -k*pi*sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dx_ee with cos(k*pi*X)*cos(l*pi*Y)',              &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dx_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -l*pi*cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dy_ee with cos(k*pi*X)*cos(l*pi*Y)',              &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dy_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Lapla_ee with cos(k*pi*X)*cos(l*pi*Y)',           &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Lapla_ee(ee_yx(yx_Data))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_LaplaInv_ee with cos(k*pi*X)*cos(l*pi*Y)',        &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data))),                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = sin(k*pi*yx_X) * cos(l*pi*yx_Y)

  yx_Deriv = k*pi*cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dx_ee with sin(k*pi*X)*cos(l*pi*Y)',              &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dx_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -l*pi*sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dy_ee with sin(k*pi*X)*cos(l*pi*Y)',              &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dy_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Lapla_ee with sin(k*pi*X)*cos(l*pi*Y)',           &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Lapla_ee(ee_yx(yx_Data))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_LaplaInv_ee with sin(k*pi*X)*cos(l*pi*Y)',        &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data))),                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(k*pi*yx_X) * sin(l*pi*yx_Y)

  yx_Deriv = -k*pi*sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dx_ee with cos(k*pi*X)*sin(l*pi*Y)',              &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dx_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = l*pi*cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dy_ee with cos(k*pi*X)*sin(l*pi*Y)',              &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dy_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Lapla_ee with cos(k*pi*X)*sin(l*pi*Y)',           &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Lapla_ee(ee_yx(yx_Data))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_LaplaInv_ee with cos(k*pi*X)*sin(l*pi*Y)',        &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data))),                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ee_module_fftj_deriv_test1', &
       'ee_module_fftj derivative function tests succeeded!')

end program ee_module_fftj_deriv_test1
