!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  tee_module �ƥ��ȥץ���� (����������, �ȥ�����®�٥ݥƥ󥷥��)
!
!����  2009/12/19  �ݹ�����
!
program tee_module_torvelbcGrid_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use tee_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32, km=16       ! �ʻ���������(X,Y)
  integer, parameter :: lm=10, mm=10, nm=16       ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: zyx_Torvel(0:km,0:jm-1,0:im-1)      ! �ʻҥǡ���
  real(8)            :: zyx_Torvel_orig(0:km,0:jm-1,0:im-1) ! �ʻҥǡ���
  real(8)            :: zyx_DTorvel(0:km,0:jm-1,0:im-1)     ! �ʻҥǡ���
  real(8)            :: tee_Torvel(0:nm,-mm:mm,-lm:lm)      ! ���ڥ��ȥ�ǡ���

  real(8)            :: zyx_Null(0:km,0:jm-1,0:im-1) =0.0D0 ! �ʻҥǡ���

  integer            :: l=2, m=5

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=2.0d0
  real(8), parameter :: zmin = -1.0d0, zmax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','tee_module_TorvelbcGrid_test', &
       'tee_module toroidal velocity potential B.C. function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  ! ���ڥ��ȥ�����
  call tee_initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax) 

 !-------------------BC-FF(Grid) ----------------------
  zyx_Torvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  zyx_Torvel_orig = zyx_Torvel

  tee_Torvel = tee_zyx(zyx_Torvel)
 
  call tee_TorBoundariesGrid(tee_Torvel,cond='FF',new=.true.)
  zyx_Torvel = zyx_tee(tee_Torvel)

  call AssertEqual(&
    message='tee_TorBoundariesGrid [FF,internal]',                &
    answer = zyx_Torvel_orig(1:km-1,:,:),                         &
    check = zyx_Torvel(1:km-1,:,:),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DTorvel = zyx_tee(tee_Dz_tee(tee_Torvel))
  call AssertEqual(&
    message='tee_TorBoundariesGrid [FF,Top]',                     &
    answer = zyx_DTorvel(0,:,:),                                  &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_TorBoundariesGrid [FF,Bottom]',                  &
    answer = zyx_DTorvel(km,:,:),                                 &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-RR(Grid) ----------------------
  zyx_Torvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  zyx_Torvel_orig = zyx_Torvel

  tee_Torvel = tee_zyx(zyx_Torvel)
  call tee_TorBoundariesGrid(tee_Torvel,cond='RR',new=.true.)
  zyx_Torvel = zyx_tee(tee_Torvel)

  call AssertEqual(&
    message='tee_TorBoundariesGrid [RR,internal]',                &
    answer = zyx_Torvel_orig(1:km-1,:,:),                         &
    check = zyx_Torvel(1:km-1,:,:),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DTorvel = zyx_tee(tee_Torvel)
  call AssertEqual(&
    message='tee_TorBoundariesGrid [RR,Top]',                     &
    answer = zyx_DTorvel(0,:,:),                                  &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='tee_TorBoundariesGrid [RR,Bottom]',                  &
    answer = zyx_DTorvel(km,:,:),                                 &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-RF(Grid) ----------------------
  zyx_Torvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  zyx_Torvel_orig = zyx_Torvel

  tee_Torvel = tee_zyx(zyx_Torvel)
  call tee_TorBoundariesGrid(tee_Torvel,cond='RF',new=.true.)
  zyx_Torvel = zyx_tee(tee_Torvel)

  call AssertEqual(&
    message='tee_TorBoundariesGrid [RF,internal]',                &
    answer = zyx_Torvel_orig(1:km-1,:,:),                         &
    check = zyx_Torvel(1:km-1,:,:),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DTorvel = zyx_tee(tee_Torvel)
  call AssertEqual(&
    message='tee_TorBoundariesGrid [RF,Top]',                     &
    answer = zyx_DTorvel(0,:,:),                                  &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DTorvel = zyx_tee(tee_Dz_tee(tee_Torvel))
  call AssertEqual(&
    message='tee_TorBoundariesGrid [RF,Bottom]',                  &
    answer = zyx_DTorvel(km,:,:),                                 &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-FR(Grid) ----------------------
  zyx_Torvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  zyx_Torvel_orig = zyx_Torvel

  tee_Torvel = tee_zyx(zyx_Torvel)
  call tee_TorBoundariesGrid(tee_Torvel,cond='FR',new=.true.)
  zyx_Torvel = zyx_tee(tee_Torvel)

  call AssertEqual(&
    message='tee_TorBoundariesGrid [FR,internal]',                &
    answer = zyx_Torvel_orig(1:km-1,:,:),                         &
    check = zyx_Torvel(1:km-1,:,:),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DTorvel = zyx_tee(tee_Dz_tee(tee_Torvel))
  call AssertEqual(&
    message='tee_TorBoundariesGrid [FR,Top]',                     &
    answer = zyx_DTorvel(0,:,:),                                  &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DTorvel = zyx_tee(tee_Torvel)
  call AssertEqual(&
    message='tee_TorBoundariesGrid [RF,Bottom]',                  &
    answer = zyx_DTorvel(km,:,:),                                 &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','tee_module_TorvelbcGrid_test', &
       'tee_module toroidal velocity potential B.C. function tests succeeded!')

end program tee_module_torvelbcGrid_test

