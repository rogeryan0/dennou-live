!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ee_module_fftj �ƥ��ȥץ���� (��ʬ�׻�)
!
!����  2009/09/09  �ݹ�����  ee_test_transform2.f90 ��� ee_module_fftj �Ѥ˲�¤
!
program ee_module_fftj_base_test2

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ee_module_fftj
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32              ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10              ! �����ȿ�������(X,Y)

  integer, parameter :: im2=64, jm2=64            ! �ʻ���������(X,Y)
  integer, parameter :: km2=21, lm2=21            ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! �ʻҥǡ���
  real(8)            :: yx_Deriv(0:jm-1,0:im-1)   ! �ʻҥǡ���

  real(8)            :: yx_Data2(0:jm2-1,0:im2-1)  ! �ʻҥǡ���
  real(8)            :: yx_Deriv2(0:jm2-1,0:im2-1) ! �ʻҥǡ���

  integer            :: k=2, l=3

  integer            :: id1, id2

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  call MessageNotify('M','ee_module_fftj_deriv_test2', &
       'ee_module_fftj derivative function tests for multiple domains and resolutions')

 !---------------- ��ɸ�ͤ����� ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax,id1)        ! ���ڥ��ȥ�����
  call ee_initial(im2,jm2,km2,lm2,xmin,xmax,ymin,ymax,id2)    ! ���ڥ��ȥ�����

 !------------------- �ؿ������å� ----------------------

  !---------- id1 ---------
  call ee_ChangeResolutionDomain(id1)

  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)

  yx_Deriv = k*pi*cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dx_ee with sin(k*pi*X)*sin(l*pi*Y) for id=1',     &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dx_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = l*pi*sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dy_ee with sin(k*pi*X)*sin(l*pi*Y) for id=1',     &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dy_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Lapla_ee with sin(k*pi*X)*sin(l*pi*Y) for id=1',  &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Lapla_ee(ee_yx(yx_Data))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_LaplaInv_ee with sin(k*pi*X)*sin(l*pi*Y) for id=1',&
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data))),                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(k*pi*yx_X) * cos(l*pi*yx_Y)

  yx_Deriv = -k*pi*sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dx_ee with cos(k*pi*X)*cos(l*pi*Y) for id=1',     &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dx_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -l*pi*cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dy_ee with cos(k*pi*X)*cos(l*pi*Y) for id=1',     &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dy_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Lapla_ee with cos(k*pi*X)*cos(l*pi*Y) for id=1',  &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Lapla_ee(ee_yx(yx_Data))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_LaplaInv_ee with cos(k*pi*X)*cos(l*pi*Y) for id=1',&
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data))),                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = sin(k*pi*yx_X) * cos(l*pi*yx_Y)

  yx_Deriv = k*pi*cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dx_ee with sin(k*pi*X)*cos(l*pi*Y) for id=1',     &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dx_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -l*pi*sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dy_ee with sin(k*pi*X)*cos(l*pi*Y) for id=1',     &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dy_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Lapla_ee with sin(k*pi*X)*cos(l*pi*Y) for id=1',  &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Lapla_ee(ee_yx(yx_Data))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_LaplaInv_ee with sin(k*pi*X)*cos(l*pi*Y) for id=1',&
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data))),                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(k*pi*yx_X) * sin(l*pi*yx_Y)

  yx_Deriv = -k*pi*sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dx_ee with cos(k*pi*X)*sin(l*pi*Y) for id=1',     &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dx_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = l*pi*cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dy_ee with cos(k*pi*X)*sin(l*pi*Y) for id=1',     &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Dy_ee(ee_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Lapla_ee with cos(k*pi*X)*sin(l*pi*Y) for id=1',  &
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_Lapla_ee(ee_yx(yx_Data))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_LaplaInv_ee with cos(k*pi*X)*sin(l*pi*Y) for id=1',&
    answer = yx_Deriv,                                            &
    check = yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data))),                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---------- id2 ----------
  write(*,*) 'for id2'
  call ee_ChangeResolutionDomain(id2)

  yx_Data2 = sin(k*pi*yx_X) * sin(l*pi*yx_Y)

  yx_Deriv2 = k*pi*cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dx_ee with sin(k*pi*X)*sin(l*pi*Y) for id=2',     &
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_Dx_ee(ee_yx(yx_Data2))),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv2 = l*pi*sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dy_ee with sin(k*pi*X)*sin(l*pi*Y) for id=2',     &
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_Dy_ee(ee_yx(yx_Data2))),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv2 = -((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Lapla_ee with sin(k*pi*X)*sin(l*pi*Y) for id=2',  &
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_Lapla_ee(ee_yx(yx_Data2))),                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv2 = -1.0/((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_LaplaInv_ee with sin(k*pi*X)*sin(l*pi*Y) for id=2',&
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data2))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data2 = cos(k*pi*yx_X) * cos(l*pi*yx_Y)

  yx_Deriv2 = -k*pi*sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dx_ee with cos(k*pi*X)*cos(l*pi*Y) for id=2',     &
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_Dx_ee(ee_yx(yx_Data2))),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv2 = -l*pi*cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dy_ee with cos(k*pi*X)*cos(l*pi*Y) for id=2',     &
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_Dy_ee(ee_yx(yx_Data2))),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv2 = -((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Lapla_ee with cos(k*pi*X)*cos(l*pi*Y) for id=2',  &
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_Lapla_ee(ee_yx(yx_Data2))),                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv2 = -1.0/((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_LaplaInv_ee with cos(k*pi*X)*cos(l*pi*Y) for id=2',&
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data2))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data2 = sin(k*pi*yx_X) * cos(l*pi*yx_Y)

  yx_Deriv2 = k*pi*cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dx_ee with sin(k*pi*X)*cos(l*pi*Y) for id=2',     &
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_Dx_ee(ee_yx(yx_Data2))),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv2 = -l*pi*sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dy_ee with sin(k*pi*X)*cos(l*pi*Y) for id=2',     &
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_Dy_ee(ee_yx(yx_Data2))),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv2 = -((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Lapla_ee with sin(k*pi*X)*cos(l*pi*Y) for id=2',  &
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_Lapla_ee(ee_yx(yx_Data2))),                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv2 = -1.0/((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_LaplaInv_ee with sin(k*pi*X)*cos(l*pi*Y) for id=2',&
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data2))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data2 = cos(k*pi*yx_X) * sin(l*pi*yx_Y)

  yx_Deriv2 = -k*pi*sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dx_ee with cos(k*pi*X)*sin(l*pi*Y) for id=2',     &
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_Dx_ee(ee_yx(yx_Data2))),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv2 = l*pi*cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Dy_ee with cos(k*pi*X)*sin(l*pi*Y) for id=2',     &
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_Dy_ee(ee_yx(yx_Data2))),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv2 = -((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Lapla_ee with cos(k*pi*X)*sin(l*pi*Y) for id=2',  &
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_Lapla_ee(ee_yx(yx_Data2))),                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv2 = -1.0/((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_LaplaInv_ee with cos(k*pi*X)*sin(l*pi*Y) for id=2',&
    answer = yx_Deriv2,                                           &
    check = yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data2))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ee_module_fftj_deriv_test2', &
       'ee_module_fftj derivative function tests &
       &for multiple domains and resolutions succeeded!')

end program ee_module_fftj_base_test2

