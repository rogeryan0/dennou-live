!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ee_module_fftj �ƥ��ȥץ���� (�ѥ���ڥ��ȥ�׻�)
!
!����  2009/09/09  �ݹ�����  ee_test_spectrum2.f90 ��� ee_module_fftj �Ѥ˲�¤
!
program ee_module_fftj_spectrum_test2

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ee_module_fftj
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10              ! �����ȿ�������(X,Y)

  integer, parameter :: im2=64, jm2=64            ! �ʻ���������(X,Y)
  integer, parameter :: km2=21, lm2=21            ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! �ʻҥǡ���
  real(8)            :: ee_Data(-lm:lm,-km:km)    ! ���ڥ��ȥ�ǡ���

  real(8)            :: yx_Data2(0:jm2-1,0:im2-1)      ! �ʻҥǡ���
  real(8)            :: ee_Data2(-lm2:lm2,-km2:km2)    ! ���ڥ��ȥ�ǡ���

  integer            :: k=2, l=3

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  integer            :: id1, id2

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','ee_module_fftj_spectrum_test1', &
       'ee_module_fftj spectrum functions tests &
       &for multiple domains and resoulutions')

 !---------------- ��ɸ�ͤ����� ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax,id1)        ! ���ڥ��ȥ�����
  call ee_initial(im2,jm2,km2,lm2,xmin,xmax,ymin,ymax,id2)    ! ���ڥ��ȥ�����

 !------------------- �ؿ������å� ----------------------

  !---------- id1 ----------
  write(*,*) 'for id1'
  call ee_ChangeResolutionDomain(id1)

  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = 0
  ee_Data(l,k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data(-l,k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data(l,-k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data(-l,-k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2

  call AssertEqual(&
    message='ee_EnergyFromStreamfunc_ee with sin(k*pi*X)*sin(l*pi*Y) for id=1',&
    answer = ee_Data,                                             &
    check = ee_EnergyFromStreamfunc_ee(ee_yx(yx_Data)),           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='total energy with sin(k*pi*X)*sin(l*pi*Y) for id=1', &
    answer = ((k*pi)**2 + (l*pi)**2)/2,                           &
    check = sum(ee_EnergyFromStreamfunc_ee(ee_yx(yx_Data)))       &
           *(xmax-xmin)*(ymax-ymin),                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  ee_Data = 0
  ee_Data(l,k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data(-l,k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data(l,-k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data(-l,-k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2

  call AssertEqual(&
    message='ee_EnstrophyFromStreamfunc_ee with sin(k*pi*X)*sin(l*pi*Y) for id=1',&
    answer = ee_Data,                                             &
    check = ee_EnstrophyFromStreamfunc_ee(ee_yx(yx_Data)),        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='total enstrophy with sin(k*pi*X)*sin(l*pi*Y) for id=1',&
    answer = ((k*pi)**2 + (l*pi)**2)**2/2,                        &
    check = sum(ee_EnstrophyFromStreamfunc_ee(ee_yx(yx_Data)))    &
           *(xmax-xmin)*(ymax-ymin),                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---------- id2 ---------
  call ee_ChangeResolutionDomain(id2)

  yx_Data2 = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data2 = 0
  ee_Data2(l,k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data2(-l,k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data2(l,-k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data2(-l,-k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2

  call AssertEqual(&
    message='ee_EnergyFromStreamfunc_ee with sin(k*pi*X)*sin(l*pi*Y) for id=2',&
    answer = ee_Data2,                                             &
    check = ee_EnergyFromStreamfunc_ee(ee_yx(yx_Data2)),           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='total energy with sin(k*pi*X)*sin(l*pi*Y) for id=2', &
    answer = ((k*pi)**2 + (l*pi)**2)/2,                           &
    check = sum(ee_EnergyFromStreamfunc_ee(ee_yx(yx_Data2)))       &
           *(xmax-xmin)*(ymax-ymin),                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  ee_Data2 = 0
  ee_Data2(l,k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data2(-l,k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data2(l,-k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data2(-l,-k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2

  call AssertEqual(&
    message='ee_EnstrophyFromStreamfunc_ee with sin(k*pi*X)*sin(l*pi*Y) for id=2',&
    answer = ee_Data2,                                             &
    check = ee_EnstrophyFromStreamfunc_ee(ee_yx(yx_Data2)),        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='total enstrophy with sin(k*pi*X)*sin(l*pi*Y) for id=2',&
    answer = ((k*pi)**2 + (l*pi)**2)**2/2,                        &
    check = sum(ee_EnstrophyFromStreamfunc_ee(ee_yx(yx_Data2)))    &
           *(xmax-xmin)*(ymax-ymin),                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ee_module_fftj_spectrum_test1', &
       'ee_module_fftj spectrum functions tests &
       &for multiple domains and resoulutions succeeded!')

end program ee_module_fftj_spectrum_test2
