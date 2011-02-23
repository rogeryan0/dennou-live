!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_zonal_module_sjpack �ƥ��ȥץ����
!
!      ����ȥ�����ݥƥ󥷥��ζ���������
!
!����  2009/09/26  �ݹ�����   wt_module_sjpack_tormagbc_test.f90 ����¤
!
program wt_zonal_module_sjpack_tormagbc_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wt_zonal_module_sjpack

  implicit none

  integer,parameter  :: im=1, jm=16, km=16  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=16        ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ri=0.5, ro=1.5      ! �⳰Ⱦ��

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_TORMAG
  real(8), dimension(nm+1,0:lm)            :: wt_TORMAG

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Zero = 0.0D0

  ! Ƚ�������
  integer, parameter :: check_digits = 9
  integer, parameter :: ignore = -10

  real(8), parameter  :: pi=3.1415926535897932385D0

  call MessageNotify('M','wt_zonal_module_sjpack_tormagbc_test', &
       'wt_zonal_module_sjpack wt_TormagBoundaries subroutine test')

  call wt_initial(im,jm,km,nm,lm,ri,ro)

 !==================== wt_TormagBoundaries =========================
  ! P_10
  xyz_TORMAG = sin(xyz_lat) * cos( pi*(xyz_rad-ri)/(ro-ri) )

  wt_TORMAG = wt_xyz(xyz_TORMAG)
  call wt_TormagBoundaries(wt_TORMAG)
  xyz_TORMAG = xyz_wt(wt_TORMAG)

  call AssertEqual(&
       message='wt_TormagBoundaries (Top B.C.)',                     &
       answer = xyz_Zero(:,:,0),                                     &
       check = xyz_TORMAG(:,:,0),                                    &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  call AssertEqual(&
       message='wt_TormagBoundaries (Bottom B.C.)',                  &
       answer = xyz_Zero(:,:,km),                                    &
       check = xyz_TORMAG(:,:,km),                                   &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

 !==================== wt_TormagBoundariesGrid =========================
  xyz_TORMAG = 2*sin(xyz_lat)**2 * cos( pi*(xyz_rad-ri)/(ro-ri) )

  wt_TORMAG = wt_xyz(xyz_TORMAG)
  call wt_TormagBoundariesGrid(wt_TORMAG)
  xyz_TORMAG = xyz_wt(wt_TORMAG)


  call AssertEqual(&
       message='wt_TormagBoundariesGrid (Top B.C.)',                 &
       answer = xyz_Zero(:,:,0),                                     &
       check = xyz_TORMAG(:,:,0),                                    &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  call AssertEqual(&
       message='wt_TormagBoundariesGrid (Bottom B.C.)',              &
       answer = xyz_Zero(:,:,km),                                    &
       check = xyz_TORMAG(:,:,km),                                   &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call MessageNotify('M','wt_zonal_module_sjpack_tormagbc_test', &
       'wt_zonal_module_sjpack wt_TormagBoundaries subroutine test succeeded!')

end program wt_zonal_module_sjpack_tormagbc_test

