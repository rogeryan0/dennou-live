!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ef_module �ƥ��ȥץ���� (��ִؿ�)
!
!����  2009/12/11  �ݹ�����
!
program ef_module_interpolate_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ef_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! �ʻҥǡ���
  real(8)            :: ef_Data(-lm:lm,-km:km)    ! ���ڥ��ȥ�ǡ���

  integer            :: k=2
  real(8)            :: x=0.8D0
  real(8)            :: y=2.0D0

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: yrad = 2.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! Ƚ�������
  integer, parameter :: check_digits = 3
  integer, parameter :: ignore = -4

  call MessageNotify('M','ef_module_interpolate_test', &
       'ef_module interpolate function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call ef_initial(im,jm,km,lm,xmin,xmax,yrad)    ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  yx_Data = sin(k*pi*yx_X) * exp(-yx_Y**2)

  call AssertEqual(&
    message='Interpolate_ef with sin(k*pi*X)*exp(-Y**2)',         &
    answer = Interpolate_ef(ef_yx(yx_Data),x,y),                  &
    check = sin(k*pi*X) * exp(-Y**2),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ef_module_interpolate_test', &
       'ef_module interpolate function tests succeeded!')

end program ef_module_interpolate_test

