!----------------------------------------------------------------------
!   Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  et_module_fftj �ƥ��ȥץ���� (�����Ѵ�)
!
!����  2009/09/10  �ݹ�����  et_test_transform.f90 �� fftj �Ѥ˲�¤
!      
program et_module_fftj_base_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use et_module_fftj
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm,0:im-1)    ! �ʻҥǡ���
  real(8)            :: et_Data(-km:km,0:lm)    ! ���ڥ��ȥ�ǡ���
  real(8)            :: yx_xi(0:jm,0:im-1)      ! �����ӥ����պ�ɸ

  integer            :: k=2

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin =  0.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','et_module_fftj_base_test', &
       'et_module_fftj basic functions tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

  yx_xi = (yx_Y - (ymin+ymax)/2 )*2/(ymax-ymin)

 !------------------- �ؿ������å� ----------------------
  yx_Data = sin(k*pi*yx_X) * yx_xi
  et_Data = 0.0 ; et_Data(-k,1) = -0.5
  call AssertEqual(&
    message='et_yx with sin(k*pi*X)*T_1(Y)',                      &
    answer = et_Data,                                             &
    check = et_yx(yx_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_et with sin(k*pi*X)*T_1(Y)',                      &
    answer = yx_Data,                                             &
    check = yx_et(et_yx(yx_Data)),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(k*pi*yx_X) * (2*yx_xi**2 -1)
  et_Data = 0.0 ; et_Data(k,2) = 0.5
  call AssertEqual(&
    message='et_yx with cos(k*pi*X)*T_2(Y)',                      &
    answer = et_Data,                                             &
    check = et_yx(yx_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_et with cos(k*pi*X)*T_2(Y)',                      &
    answer = yx_Data,                                             &
    check = yx_et(et_yx(yx_Data)),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = sin(k*pi*yx_X) 
  et_Data = 0.0 ; et_Data(-k,0) = -1.0
  call AssertEqual(&
    message='et_yx with sin(k*pi*X)*T_0(Y)',                      &
    answer = et_Data,                                             &
    check = et_yx(yx_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_et with sin(k*pi*X)*T_0(Y)',                      &
    answer = yx_Data,                                             &
    check = yx_et(et_yx(yx_Data)),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(k*pi*yx_X) * (4*yx_xi**3 - 3*yx_xi)
  et_Data = 0.0 ; et_Data(k,3) = 0.5
  call AssertEqual(&
    message='et_yx with cos(k*pi*X)*T_3(Y)',                      &
    answer = et_Data,                                             &
    check = et_yx(yx_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_et with cos(k*pi*X)*T_3(Y)',                      &
    answer = yx_Data,                                             &
    check = yx_et(et_yx(yx_Data)),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','et_module_fftj_base_test', &
       'et_module_fftj basic functions tests succeeded!')

end program et_module_fftj_base_test
