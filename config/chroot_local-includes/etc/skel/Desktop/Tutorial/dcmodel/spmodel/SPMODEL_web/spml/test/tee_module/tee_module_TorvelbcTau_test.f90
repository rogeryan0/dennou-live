!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  tee_module �ƥ��ȥץ���� (����������, �ȥ�����®�٥ݥƥ󥷥��)
!
!����  2009/12/19  �ݹ�����
!
program tee_module_torvelbcTau_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use tee_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32, km=16       ! �ʻ���������(X,Y)
  integer, parameter :: lm=10, mm=10, nm=10       ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: zyx_Torvel(0:km,0:jm-1,0:im-1)      ! �ʻҥǡ���
  real(8)            :: zyx_DTorvel(0:km,0:jm-1,0:im-1)     ! �ʻҥǡ���

  real(8)            :: tee_Torvel(0:nm,-mm:mm,-lm:lm)      ! ���ڥ��ȥ�ǡ���
  real(8)            :: tee_Torvel_orig(0:nm,-mm:mm,-lm:lm) ! ���ڥ��ȥ�ǡ���

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

  call MessageNotify('M','tee_module_TorvelbcTau_test', &
       'tee_module toroidal velocity potential B.C. function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  ! ���ڥ��ȥ�����
  call tee_initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax) 

 !-------------------BC-FF(Tau) ----------------------
  zyx_Torvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)

  tee_Torvel = tee_zyx(zyx_Torvel)
  tee_Torvel_orig = tee_Torvel
 
  call tee_TorBoundariesTau(tee_Torvel,cond='FF',new=.true.)
  zyx_Torvel = zyx_tee(tee_Torvel)

  call AssertEqual(&
    message='tee_TorBoundariesTau [FF,internal]',                &
    answer = tee_Torvel_orig(0:nm-2,:,:),                         &
    check = tee_Torvel(0:nm-2,:,:),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DTorvel = zyx_tee(tee_Dz_tee(tee_Torvel))
  call AssertEqual(&
    message='tee_TorBoundariesTau [FF,Top]',                     &
    answer = zyx_DTorvel(0,:,:),                                  &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_TorBoundariesTau [FF,Bottom]',                  &
    answer = zyx_DTorvel(km,:,:),                                 &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-RR(Tau) ----------------------
  zyx_Torvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)

  tee_Torvel = tee_zyx(zyx_Torvel)
  tee_Torvel_orig = tee_Torvel

  call tee_TorBoundariesTau(tee_Torvel,cond='RR',new=.true.)
  zyx_Torvel = zyx_tee(tee_Torvel)

  call AssertEqual(&
    message='tee_TorBoundariesTau [RR,internal]',                &
    answer = tee_Torvel_orig(0:nm-2,:,:),                         &
    check = tee_Torvel(0:nm-2,:,:),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DTorvel = zyx_tee(tee_Torvel)
  call AssertEqual(&
    message='tee_TorBoundariesTau [RR,Top]',                     &
    answer = zyx_DTorvel(0,:,:),                                  &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='tee_TorBoundariesTau [RR,Bottom]',                  &
    answer = zyx_DTorvel(km,:,:),                                 &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-RF(Tau) ----------------------
  zyx_Torvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)

  tee_Torvel = tee_zyx(zyx_Torvel)
  tee_Torvel_orig = tee_Torvel

  call tee_TorBoundariesTau(tee_Torvel,cond='RF',new=.true.)
  zyx_Torvel = zyx_tee(tee_Torvel)

  call AssertEqual(&
    message='tee_TorBoundariesTau [RF,internal]',                &
    answer = tee_Torvel_orig(0:nm-2,:,:),                         &
    check = tee_Torvel(0:nm-2,:,:),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DTorvel = zyx_tee(tee_Torvel)
  call AssertEqual(&
    message='tee_TorBoundariesTau [RF,Top]',                     &
    answer = zyx_DTorvel(0,:,:),                                  &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DTorvel = zyx_tee(tee_Dz_tee(tee_Torvel))
  call AssertEqual(&
    message='tee_TorBoundariesTau [RF,Bottom]',                  &
    answer = zyx_DTorvel(km,:,:),                                 &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-FR(Tau) ----------------------
  zyx_Torvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)

  tee_Torvel = tee_zyx(zyx_Torvel)
  tee_Torvel_orig = tee_Torvel

  call tee_TorBoundariesTau(tee_Torvel,cond='FR',new=.true.)
  zyx_Torvel = zyx_tee(tee_Torvel)

  call AssertEqual(&
    message='tee_TorBoundariesTau [FR,internal]',                &
    answer = tee_Torvel_orig(0:nm-2,:,:),                         &
    check = tee_Torvel(0:nm-2,:,:),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DTorvel = zyx_tee(tee_Dz_tee(tee_Torvel))
  call AssertEqual(&
    message='tee_TorBoundariesTau [FR,Top]',                     &
    answer = zyx_DTorvel(0,:,:),                                  &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DTorvel = zyx_tee(tee_Torvel)
  call AssertEqual(&
    message='tee_TorBoundariesTau [RF,Bottom]',                  &
    answer = zyx_DTorvel(km,:,:),                                 &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','tee_module_TorvelbcTau_test', &
       'tee_module toroidal velocity potential B.C. function tests succeeded!')

end program tee_module_torvelbcTau_test

