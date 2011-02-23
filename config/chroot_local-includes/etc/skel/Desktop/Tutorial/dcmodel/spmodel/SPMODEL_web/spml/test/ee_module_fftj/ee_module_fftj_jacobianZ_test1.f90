!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ee_module_fftj �ƥ��ȥץ���� (������������ή��䥳�ӥ���׻�)
!
!����  2009/09/09  �ݹ�����  ee_test_jacobianZ.f90 ��� ee_module_fftj �Ѥ˲�¤
!
program ee_module_fftj_jacobianZ_test1

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ee_module_fftj
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=16, jm=16            ! �ʻ���������(X,Y)
  integer, parameter :: km=5, lm=5              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! �ʻҥǡ���
  real(8)            :: yx_JacobZ(0:jm-1,0:im-1)  ! �ʻҥǡ���

  integer            :: k=3,l=1

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','ee_module_fftj_jacobianZ_test1', &
       'ee_module_fftj jacobianZ function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

 !------------------- �ؿ������å� ----------------------
  yx_Data = sin(k*pi*yx_X) + cos(l*pi*yx_Y)
  yx_JacobZ = (dble(k)/l-dble(l)/k)*cos(k*pi*yx_X)*sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_JacobianZ_ee with sin(k*pi*X)+cos(l*pi*Y)',       &
    answer = yx_JacobZ,                                           &
    check = yx_ee(ee_JacobianZ_ee(ee_yx(yx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ee_module_fftj_jacobianZ_test1', &
       'ee_module_fftj jacobianZ function tests succeeded!')

end program ee_module_fftj_jacobianZ_test1
