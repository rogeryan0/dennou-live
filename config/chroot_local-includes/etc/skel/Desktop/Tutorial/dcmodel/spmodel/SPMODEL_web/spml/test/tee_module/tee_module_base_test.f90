!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  tee_module �ƥ��ȥץ���� (�����Ѵ�)
!
!����  2009/12/15  �ݹ�����
!
program tee_module_base_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use tee_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32, km=16       ! �ʻ���������(X,Y)
  integer, parameter :: lm=10, mm=10, nm=10       ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: zyx_Data(0:km,0:jm-1,0:im-1)    ! �ʻҥǡ���
  real(8)            :: tee_Data(0:nm,-mm:mm,-lm:lm)    ! ���ڥ��ȥ�ǡ���

  integer            :: l=2, m=5

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=2.0d0
  real(8), parameter :: zmin = -1.0d0, zmax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','tee_module_base_test', &
       'tee_module basic transform function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  ! ���ڥ��ȥ�����
  call tee_initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax) 

 !------------------- �ؿ������å� ----------------------
  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  tee_Data = 0.0 ; tee_Data(1,m,l) = -0.25 ; ; tee_Data(1,-m,l) = 0.25

  call AssertEqual(&
    message='tee_zyx with sin(k*pi*X)*sin(l*pi*Y)*Z',             &
    answer = tee_Data,                                            &
    check = tee_zyx(zyx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='zyx_tee with sin(k*pi*X)*sin(l*pi*Y)*Z',             &
    answer = zyx_Data,                                            &
    check = zyx_tee(tee_zyx(zyx_Data)),                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  tee_Data = 0.0 ; tee_Data(2,m,l) = 0.25  ; tee_Data(2,-m,l) = 0.25  

  call AssertEqual(&
    message='ee_yx with cos(k*pi*X)*cos(l*pi*Y)*(2*Z**2-1)',      &
    answer = tee_Data,                                            &
    check = tee_zyx(zyx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_ee with cos(k*pi*X)*cos(l*pi*Y)*(2*Z**2-1)',      &
    answer = zyx_Data,                                            &
    check = zyx_tee(tee_zyx(zyx_Data)),                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = sin(l*pi*zyx_X) * cos(m*pi*zyx_Y)/2.0D0
  tee_Data = 0.0 ; tee_Data(0,-m,-l) = -0.25  ; tee_Data(0,m,-l) = - 0.25  

  call AssertEqual(&
    message='tee_zyx with sin(k*pi*X)*cos(l*pi*Y)/2',             &
    answer = tee_Data,                                            &
    check = tee_zyx(zyx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='zyx_tee with sin(k*pi*X)*cos(l*pi*Y)/2',             &
    answer = zyx_Data,                                            &
    check = zyx_tee(tee_zyx(zyx_Data)),                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = cos(l*pi*zyx_X) * sin(m*pi*zyx_Y)*(4*zyx_Z**3-3*zyx_Z)
  tee_Data = 0.0 ; tee_Data(3,-m,-l) = -0.25  ; tee_Data(3,m,-l) = 0.25  

  call AssertEqual(&
    message='tee_zyx with cos(k*pi*X)*sin(l*pi*Y)*(4Z**3-3Z)',    &
    answer = tee_Data,                                            &
    check = tee_zyx(zyx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='zyx_tee with cos(k*pi*X)*sin(l*pi*Y)*(4Z**3-3Z)',    &
    answer = zyx_Data,                                            &
    check = zyx_tee(tee_zyx(zyx_Data)),                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','tee_module_base_test', &
       'tee_module basic transform functions tests succeeded!')

end program tee_module_base_test
