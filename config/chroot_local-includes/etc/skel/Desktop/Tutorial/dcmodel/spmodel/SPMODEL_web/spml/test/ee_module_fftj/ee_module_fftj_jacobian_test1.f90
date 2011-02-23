!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ee_module_fftj �ƥ��ȥץ���� (�䥳�ӥ���׻�)
!
!����  2009/09/09  �ݹ�����  ee_test_jacobian.f90 ��� ee_module_fftj �Ѥ˲�¤
!
program ee_module_fftj_jacobian_test1

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ee_module_fftj
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=16, jm=16            ! �ʻ���������(X,Y)
  integer, parameter :: km=5, lm=5              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_A(0:jm-1,0:im-1)     ! �ʻҥǡ���
  real(8)            :: yx_B(0:jm-1,0:im-1)     ! �ʻҥǡ���
  real(8)            :: yx_Jacob(0:jm-1,0:im-1)  ! �ʻҥǡ���

  integer            :: k=4,l=2

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','ee_module_fftj_jacobian_test1', &
       'ee_module_fftj jacobian function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  yx_A = sin(k*pi*yx_X)
  yx_B = sin(l*pi*yx_Y)
  yx_Jacob = k*pi*cos(k*pi*yx_X) * l*pi*cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with sin(k*pi*X) and sin(l*pi*Y)', &
    answer = yx_Jacob,                                            &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A = cos(k*pi*yx_X)
  yx_B = cos(l*pi*yx_Y)
  yx_Jacob = k*pi*sin(k*pi*yx_X) * l*pi*sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with cos(k*pi*X) and cos(l*pi*Y)', &
    answer = yx_Jacob,                                            &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A = sin(k*pi*yx_X)
  yx_B = cos(l*pi*yx_Y)
  yx_Jacob = - k*pi*cos(k*pi*yx_X) * l*pi*sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with cos(k*pi*X) and sin(l*pi*Y)', &
    answer = yx_Jacob,                                            &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A = cos(k*pi*yx_X)
  yx_B = sin(l*pi*yx_Y)
  yx_Jacob = - k*pi*sin(k*pi*yx_X) * l*pi*cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with cos(k*pi*X) and sin(l*pi*Y)', &
    answer = yx_Jacob,                                            &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ee_module_fftj_jacobian_test1', &
       'ee_module_fftj jacobian function tests succeeded!')

end program ee_module_fftj_jacobian_test1
