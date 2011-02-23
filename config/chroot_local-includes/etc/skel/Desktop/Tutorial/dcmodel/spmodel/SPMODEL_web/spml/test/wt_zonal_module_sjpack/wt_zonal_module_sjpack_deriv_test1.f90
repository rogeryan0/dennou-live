!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!É½Âê  wt_zonal_module_sjpack ¥Æ¥¹¥È¥×¥í¥°¥é¥à
!
!      ÈùÊ¬¤Î¥Æ¥¹¥È
!  
!ÍúÎò  2009/09/26  ÃÝ¹­¿¿°ì  wt_module_sjpack_deriv_test1.f90 ¤ò²þÂ¤
!
program wt_zonal_module_sjpack_deriv_test1

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wt_zonal_module_sjpack
  implicit none

  integer,parameter  :: im=1, jm=16, km=16   ! ³Ê»ÒÅÀ¤ÎÀßÄê(·ÐÅÙ, °ÞÅÙ, Æ°·Â)
  integer,parameter  :: nm=10, lm=16         ! ÀÚÃÇÇÈ¿ô¤ÎÀßÄê(¿åÊ¿, Æ°·Â)
  real(8),parameter  :: ri=0.5, ro=1.5       ! Æâ³°È¾·Â

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Data
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_DData

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_GradLon
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_GradLat
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_DivLon
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_DivLat
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VLon
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VLat
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VRad
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Div
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Psi


  ! È½Äê¸íº¹ÀßÄê
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  integer, parameter :: nmin=1, nmax=9

  integer :: n

  call MessageNotify('M','wt_zonal_module_sjpack_deriv1_test', &
       'wt_zonal_module_sjpack derivative function test #1')

  call wt_Initial(im,jm,km,nm,lm,ri,ro)

  !==================== wt_DivRad_wt ====================
  do n=nmin,nmax
     write(6,*) 'n=',n
     xyz_Data = xyz_Rad**n
     xyz_DData = (n+2)*xyz_Rad**(n-1)

     call AssertEqual(&
       message='wt_Divrat_wt with r^n',                              &
       answer = xyz_DData,                                           &
       check = xyz_wt(wt_DivRad_wt(wt_xyz(xyz_Data))),               &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  enddo

  !========== xyz_GradLon_wt, xyz_GradLat_wt, wt_Div_xyz_xyz_xyz ==========

  ! ------ r^2 Y_1^0 Toridal field -----
  n = 2
  xyz_VRad = 0.0D0
  xyz_Psi = xyz_Rad**n * sin(xyz_Lat)   ! r**2 P_1^0

  xyz_GradLon =  0.0D0
  call AssertEqual(&
       message='xyz_GradLon_wt with r^2 Y_1^0',                     &
       answer = xyz_GradLon,                                         &
       check = xyz_GradLon_wt(wt_xyz(xyz_Psi)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  xyz_GradLat =  xyz_Rad**(n-1)*cos(xyz_Lat)
  call AssertEqual(&
       message='xyz_GradLat_wt with r^2 Y_1^0',                     &
       answer = xyz_GradLat,                                         &
       check = xyz_GradLat_wt(wt_xyz(xyz_Psi)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  xyz_VLon =  xyz_GradLon_wt(wt_xyz(xyz_Psi))
  xyz_VLat =  xyz_GradLat_wt(wt_xyz(xyz_Psi))
  xyz_VRad = 0.0D0

  xyz_Div = - 2.0D0* xyz_Psi/xyz_Rad**2.0D0

  call AssertEqual(&
       message='wt_Div_xyz_xyz_xyz_wt with Toroiral r^2 Y_1^0',     &
       answer = xyz_Div,                                             &
       check = xyz_wt(wt_Div_xyz_xyz_xyz(xyz_VLon,xyz_VLat,xyz_VRad)),&
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! ------ r^2Y_2^0 Toridal field -----
  xyz_Psi = xyz_Rad**n * (3.0D0/2.0D0*sin(xyz_Lat)**2 - 1.0D0/2.0D0)  ! P_2^0

  xyz_GradLon =  0.0D0
  call AssertEqual(&
       message='xyz_GradLon_wt with r^2 Y_2^0',                      &
       answer = xyz_GradLon,                                         &
       check =  xyz_GradLon_wt(wt_xyz(xyz_Psi)),                     &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  xyz_GradLat =  xyz_Rad**(n-1)*3*sin(xyz_Lat)*cos(xyz_Lat)
  call AssertEqual(&
       message='xyz_GradLat_wt with r^2 Y_2^0',                      &
       answer = xyz_GradLat,                                         &
       check = xyz_GradLat_wt(wt_xyz(xyz_Psi)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  xyz_VLon =  xyz_GradLon_wt(wt_xyz(xyz_Psi))
  xyz_VLat =  xyz_GradLat_wt(wt_xyz(xyz_Psi))
  xyz_VRad = 0.0D0

  xyz_Div = - 6* xyz_Psi/xyz_Rad**2

  call AssertEqual(&
       message='wt_Div_xyz_xyz_xyz_wt with Toroiral r2 Y_1^0',       &
       answer = xyz_Div,                                             &
       check = xyz_wt(wt_Div_xyz_xyz_xyz(xyz_VLon,xyz_VLat,xyz_VRad)),&
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  !==================== wt_DivLon_xyz, wt_DivLat_xyz ====================
  n=2
  write(6,*)'n =',n

  xyz_VLon   = xyz_rad**n * sin(xyz_Lat)*cos(xyz_Lat)
  xyz_DivLon = 0.0D0

  xyz_VLat   = xyz_rad**n * sin(xyz_Lat)*cos(xyz_Lat)
  xyz_DivLat = xyz_rad**(n-1)*(cos(xyz_Lat)**2-2*sin(xyz_Lat)**2)

  call AssertEqual(&
       message='wt_DivLon_xyz with r^n cos¦Õ sin¦Õ',              &
       answer = xyz_DivLon,                                          &
       check = xyz_wt(wt_DivLon_xyz(xyz_VLon)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='wt_DivLat_xyz with r^n cos¦Õ sin¦Õ',              &
       answer = xyz_DivLat,                                          &
       check = xyz_wt(wt_DivLat_xyz(xyz_VLat)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call MessageNotify('M','wt_zonal_module_sjpack_deriv1_test', &
       'wt_zonal_module_sjpack derivative function test #1 succeeded!')

end program wt_zonal_module_sjpack_deriv_test1

