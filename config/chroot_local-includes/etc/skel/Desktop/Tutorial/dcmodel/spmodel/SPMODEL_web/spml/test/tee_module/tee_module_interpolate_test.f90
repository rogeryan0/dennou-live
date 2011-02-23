!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  tee_module �ƥ��ȥץ���� (��ַ׻�)
!
!����  2009/12/19  �ݹ�����
!
program tee_module_interpolate_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use tee_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32, km=16       ! �ʻ���������(X,Y)
  integer, parameter :: lm=10, mm=10, nm=10       ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: zyx_Data(0:km,0:jm-1,0:im-1)      ! �ʻҥǡ���
  real(8)            :: x=0.2, y=0.7, z=0.4
  real(8)            :: Ans

  integer            :: l=2, m=5

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=2.0d0
  real(8), parameter :: zmin = -1.0d0, zmax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','tee_module_interpolate_test', &
       'tee_module interpolate function test')

 !---------------- ��ɸ�ͤ����� ---------------------
  ! ���ڥ��ȥ�����
  call tee_initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax) 

 !-------------------BC-DD(Grid) ----------------------
  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z

  Ans = sin(l*pi*X) * sin(m*pi*Y) * Z

  call AssertEqual(&
    message='Interpolate_tee',&
    answer = Ans,                                                 &
    check = Interpolate_tee(tee_zyx(zyx_Data),x,y,z),             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','tee_module_interpolate_test', &
       'tee_module interpolate function test succeeded!')


end program tee_module_interpolate_test
