!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_zonal_module_sjpack �ƥ��ȥץ����
!
!      �ݥ�����ݥƥ󥷥��ζ���������
!
!����  2009/09/26  �ݹ�����   wt_module_sjpack_polvelbc_test.f90 ����¤
!
program wt_zonal_module_sjpack_polvelbc_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wt_zonal_module_sjpack

  implicit none

  integer,parameter  :: im=1, jm=16, km=16  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=16        ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ri=0.5, ro=1.5      ! �⳰Ⱦ��

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Poloidal
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_LaplaPol
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_LaplaPol1
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_True
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Zero
  character(len=2), dimension(4), parameter :: BCond=(/'FF','FR','RF','RR'/)

  ! Ƚ�������
  integer, parameter :: check_digits = 9
  integer, parameter :: ignore = -10

  real(8), parameter  :: pi=3.1415926535897932385D0

  integer :: l

  call MessageNotify('M','wt_zonal_module_sjpack_olvelbc_test', &
       'wt_zonal_module_sjpack wt_LaplaPol2polGrid_wt function tests')

  call wt_initial(im,jm,km,nm,lm,ri,ro)

  do l=1,4

     ! P_10
     xyz_Poloidal = sin(xyz_Lat) * sin( pi*(xyz_Rad-ri)/(ro-ri) )
     xyz_LaplaPol = xyz_wt(wt_Lapla_wt(wt_xyz(xyz_Poloidal)))

     xyz_Poloidal = xyz_wt(wt_LaplaPol2PolGrid_wt(wt_xyz(xyz_LaplaPol),BCond(l),new=.true.))

     xyz_LaplaPol1 = xyz_wt(wt_Lapla_wt(wt_xyz(xyz_Poloidal)))
     xyz_Zero = 0.0D0

     !---------------- ���������å� -----------------------
     call AssertEqual(&
          message='wt_LaplaPol2polGrid_wt (intenal value)',             &
          answer = xyz_LaplaPol1(:,:,2:km-2),                           &
          check = xyz_LaplaPol(:,:,2:km-2),                             &
          significant_digits = check_digits, ignore_digits = ignore     &
          )

     !--------- ��ü���������å� ----------

     !----- ��=0 at the top ---------
     call AssertEqual(&
          message='wt_LaplaPol2polGrid_wt (Top B.C. ��=0)',             &
          answer = xyz_Zero(:,:,0),                                     &
          check = xyz_Poloidal(:,:,0),                                  &
          significant_digits = check_digits, ignore_digits = ignore     &
          )

     !----- d��/dr=0, d^2��/dr^2 at the top ---------
     if( BCond(l)(1:1) == 'F' ) then
        xyz_True = xyz_wt(wt_DRad_wt(wt_DRad_wt(wt_xyz(xyz_Poloidal))))
     else
        xyz_True = xyz_wt(wt_DRad_wt(wt_xyz(xyz_Poloidal)))
     endif

     call AssertEqual(&
          message='wt_LaplaPol2polGrid_wt ('//BCond(l)//'-Top B.C)',    &
          answer = xyz_Zero(:,:,0),                                     &
          check = xyz_True(:,:,0),                                      &
          significant_digits = check_digits, ignore_digits = ignore     &
          )

     !--------- ��ü���������å� ----------

     !----- ��=0 at the bottom ---------
     call AssertEqual(&
          message='wt_LaplaPol2polGrid_wt (Bottom B.C. ��=0)',          &
          answer = xyz_Zero(:,:,km),                                    &
          check = xyz_Poloidal(:,:,km),                                 &
          significant_digits = check_digits, ignore_digits = ignore     &
          )

     !----- d��/dr=0, d^2��/dr^2 at the bottom ---------
     if( BCond(l)(2:2) == 'F' ) then
        xyz_True = xyz_wt(wt_DRad_wt(wt_DRad_wt(wt_xyz(xyz_Poloidal))))
     else
        xyz_True = xyz_wt(wt_DRad_wt(wt_xyz(xyz_Poloidal)))
     endif

     call AssertEqual(&
          message='wt_LaplaPol2polGrid_wt ('//BCond(l)//'-Bottom B.C)', &
          answer = xyz_Zero(:,:,km),                                    &
          check = xyz_True(:,:,km),                                     &
          significant_digits = check_digits, ignore_digits = ignore     &
          )

  end do

  call MessageNotify('M','wt_zonal_module_sjpack_olvelbc_test', &
       'wt_zonal_module_sjpack wt_LaplaPol2polGrid_wt function tests suceeded!')

end program wt_zonal_module_sjpack_polvelbc_test
