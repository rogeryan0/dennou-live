!----------------------------------------------------------------------
!   Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  et_module_fftj �ƥ��ȥץ���� (��ʬ�׻�)
!
!����  2009/09/10  �ݹ�����  et_test_derivative.f90 �� fftj �Ѥ˲�¤
!
program et_module_fftj_deriv_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use et_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32              ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=32              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm,0:im-1)    ! �ʻҥǡ���
  real(8)            :: yx_Deriv(0:jm,0:im-1)   ! �ʻҥǡ���

  integer            :: k=2, l=3

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! Ƚ�������
  integer, parameter :: check_digits = 8
  integer, parameter :: ignore = -9

  call MessageNotify('M','et_module_fftj_deriv_test', &
       'et_module_fftj derivative functions tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

 !------------------- �ؿ������å� ----------------------
  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)

  yx_Deriv = k*pi*cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='et_Dx_et with sin(k*pi*X)*sin(l*pi*Y))',             &
    answer = yx_Deriv,                                            &
    check = yx_et(et_Dx_et(et_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = l*pi*sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='et_Dy_et with sin(k*pi*X)*sin(l*pi*Y))',             &
    answer = yx_Deriv,                                            &
    check = yx_et(et_Dy_et(et_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='et_Lapla_et with sin(k*pi*X)*sin(l*pi*Y))',          &
    answer = yx_Deriv,                                            &
    check = yx_et(et_Lapla_et(et_yx(yx_Data))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='et_LaplaInv_et with sin(k*pi*X)*sin(l*pi*Y))',       &
    answer = yx_Deriv,                                            &
    check = yx_et(et_LaplaInv_et(et_yx(yx_Data))),                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(k*pi*yx_X) * cos(l*pi*yx_Y)

  yx_Deriv = -k*pi*sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='et_Dx_et with cos(k*pi*X)*cos(l*pi*Y))',             &
    answer = yx_Deriv,                                            &
    check = yx_et(et_Dx_et(et_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -l*pi*cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='et_Dy_et with cos(k*pi*X)*cos(l*pi*Y))',             &
    answer = yx_Deriv,                                            &
    check = yx_et(et_Dy_et(et_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='et_Lapla_et with cos(k*pi*X)*cos(l*pi*Y))',          &
    answer = yx_Deriv,                                            &
    check = yx_et(et_Lapla_et(et_yx(yx_Data))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

!!$  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * cos(l*pi*yx_Y)
!!$  call AssertEqual(&
!!$    message='et_LaplaInv_et with cos(k*pi*X)*cos(l*pi*Y))',       &
!!$    answer = yx_Deriv,                                            &
!!$    check = yx_et(et_LaplaInv_et(et_yx(yx_Data))),                &
!!$    significant_digits = check_digits, ignore_digits = ignore     &
!!$    )

  yx_Data = sin(k*pi*yx_X) * cos(l*pi*yx_Y)

  yx_Deriv = k*pi*cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='et_Dx_et with sin(k*pi*X)*cos(l*pi*Y))',             &
    answer = yx_Deriv,                                            &
    check = yx_et(et_Dx_et(et_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -l*pi*sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='et_Dy_et with sin(k*pi*X)*cos(l*pi*Y))',             &
    answer = yx_Deriv,                                            &
    check = yx_et(et_Dy_et(et_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='et_Lapla_et with sin(k*pi*X)*cos(l*pi*Y))',          &
    answer = yx_Deriv,                                            &
    check = yx_et(et_Lapla_et(et_yx(yx_Data))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

!!$  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * cos(l*pi*yx_Y)
!!$  call AssertEqual(&
!!$    message='et_LaplaInv_et with sin(k*pi*X)*cos(l*pi*Y))',       &
!!$    answer = yx_Deriv,                                            &
!!$    check = yx_et(et_LaplaInv_et(et_yx(yx_Data))),                &
!!$    significant_digits = check_digits, ignore_digits = ignore     &
!!$    )

  yx_Data = cos(k*pi*yx_X) * sin(l*pi*yx_Y)

  yx_Deriv = -k*pi*sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='et_Dx_et with cos(k*pi*X)*sin(l*pi*Y))',             &
    answer = yx_Deriv,                                            &
    check = yx_et(et_Dx_et(et_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = l*pi*cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call AssertEqual(&
    message='et_Dy_et with cos(k*pi*X)*sin(l*pi*Y))',             &
    answer = yx_Deriv,                                            &
    check = yx_et(et_Dy_et(et_yx(yx_Data))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='et_Lapla_et with cos(k*pi*X)*sin(l*pi*Y))',          &
    answer = yx_Deriv,                                            &
    check = yx_et(et_Lapla_et(et_yx(yx_Data))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call AssertEqual(&
    message='et_LaplaInv_et with cos(k*pi*X)*sin(l*pi*Y))',       &
    answer = yx_Deriv,                                            &
    check = yx_et(et_LaplaInv_et(et_yx(yx_Data))),                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','et_module_fftj_deriv_test', &
       'et_module_fftj derivative functions tests succeeded!')

end program et_module_fftj_deriv_test
