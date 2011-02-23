!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!É½Âê  wt_module_sjpack ¥Æ¥¹¥È¥×¥í¥°¥é¥à
!      ÀÑÊ¬¡¦Ê¿¶Ñ·×»»
!
!ÍúÎò  2009/09/06  ÃÝ¹­¿¿°ì  wt_test_intavr.f90 ¤è¤ê SJPACK ÍÑ¤Ë²þÂ¤
!
program wt_module_sjpack_integral_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wt_module_sjpack

  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! ³Ê»ÒÅÀ¤ÎÀßÄê(·ÐÅÙ, °ÞÅÙ, Æ°·Â)
  integer,parameter  :: nm=10, lm=16         ! ÀÚÃÇÇÈ¿ô¤ÎÀßÄê(¿åÊ¿, Æ°·Â)
  real(8),parameter  :: ri=0.5, ro=1.5       ! Æâ³°È¾·Â
  real(8), dimension(0:im-1,1:jm,0:km)       :: xyz_Data

  real(8), dimension(0:im-1,1:jm)            :: xy_Data
  real(8), dimension(1:jm,0:km)              :: yz_Data
  real(8), dimension(0:im-1,0:km)            :: xz_Data
  real(8), dimension(0:im-1)                 :: x_Data
  real(8), dimension(1:jm)                   :: y_Data
  real(8), dimension(0:km)                   :: z_Data
  real(8)                                    :: Data

  ! È½Äê¸íº¹ÀßÄê
  integer, parameter :: check_digits = 8
  integer, parameter :: ignore = -9

  real(8), parameter  :: pi=3.1415926535897932385D0


  !=============================== ÀÑÊ¬ =================================

  call MessageNotify('M','wt_module_sjpack_integral_test', &
       'wt_module_sjpack integration functions tests')

  call wt_Initial(im,jm,km,nm,lm,ri,ro)

 !---------- Äê¿ô ----------
  xyz_Data = 1.0D0
  yz_Data = 2*pi
  call AssertEqual(&
       message='yz_IntLon_xyz with constant field',                &
       answer = yz_Data,                                           &
       check = yz_IntLon_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xz_Data = 2.0D0
  call AssertEqual(&
       message='xz_IntLat_xyz with constant field',                &
       answer = xz_Data,                                           &
       check = xz_IntLat_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xy_Data = 1/3.0D0*(ro**3-ri**3)
  call AssertEqual(&
       message='xy_IntRad_xyz with constant field',                &
       answer = xy_Data,                                           &
       check = xy_IntRad_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  z_Data = 4*pi
  call AssertEqual(&
       message='z_IntLonLat_xyz with constant field',              &
       answer = z_Data,                                            &
       check = z_IntLonLat_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  x_Data = 2/3.0D0*(ro**3-ri**3)
  call AssertEqual(&
       message='x_IntLatRad_xyz with constant field',              &
       answer = x_Data,                                            &
       check = x_IntLatRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  y_Data = 2*pi/3.0D0*(ro**3-ri**3)
  call AssertEqual(&
       message='y_IntLonRad_xyz with constant field',              &
       answer = y_Data,                                            &
       check = y_IntLonRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  Data = 4*pi/3*(ro**3-ri**3)
  call AssertEqual(&
       message='IntLonLatRad_xyz with constant field',              &
       answer = Data,                                            &
       check = IntLonLatRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )


  !------------------ r -----------------------
  xyz_Data = xyz_Rad 

  yz_Data = 2*pi* xyz_Rad(1,:,:)
  call AssertEqual(&
       message='yz_IntLon_xyz with r-field',                &
       answer = yz_Data,                                           &
       check = yz_IntLon_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xz_Data = 2.0D0 * xyz_Data(:,1,:)
  call AssertEqual(&
       message='xz_IntLat_xyz with r-field',                &
       answer = xz_Data,                                           &
       check = xz_IntLat_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xy_Data = 1/4.0D0*(ro**4-ri**4) 
  call AssertEqual(&
       message='xy_IntRad_xyz with r field',                &
       answer = xy_Data,                                           &
       check = xy_IntRad_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  z_Data = 4*pi * xyz_Data(1,1,:)
  call AssertEqual(&
       message='z_IntLonLat_xyz with r field',                     &
       answer = z_Data,                                            &
       check = z_IntLonLat_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  x_Data = 1/2.0D0*(ro**4-ri**4)
  call AssertEqual(&
       message='x_IntLatRad_xyz with r field',                     &
       answer = x_Data,                                            &
       check = x_IntLatRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  y_Data = pi/2.0D0*(ro**4-ri**4)
  call AssertEqual(&
       message='y_IntLonRad_xyz with constant field',              &
       answer = y_Data,                                            &
       check = y_IntLonRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  Data = pi*(ro**4-ri**4)
  call AssertEqual(&
       message='IntLonLatRad_xyz with constant field',              &
       answer = Data,                                            &
       check = IntLonLatRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  !--------- sin(phi)**2  ---------
  xyz_Data = sin(xyz_Lat)**2

  yz_Data = 2*pi* xyz_Data(1,:,:)
  call AssertEqual(&
       message='yz_IntLon_xyz with sin^2¦Õ field',                 &
       answer = yz_Data,                                           &
       check = yz_IntLon_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xz_Data = 2.0D0/3.0D0
  call AssertEqual(&
       message='xz_IntLat_xyz with sin^2¦Õ field',                 &
       answer = xz_Data,                                           &
       check = xz_IntLat_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xy_Data = 1/3.0D0*(ro**3-ri**3) * xyz_Data(:,:,1)
  call AssertEqual(&
       message='xy_IntRad_xyz with sin^2¦Õ field',                 &
       answer = xy_Data,                                           &
       check = xy_IntRad_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  z_Data = 4*pi/3.0
  call AssertEqual(&
       message='z_IntLonLat_xyz with sin^2¦Õ field',               &
       answer = z_Data,                                            &
       check = z_IntLonLat_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  x_Data = 2/9.0D0*(ro**3-ri**3)
  call AssertEqual(&
       message='x_IntLatRad_xyz with sin^2¦Õ field',               &
       answer = x_Data,                                            &
       check = x_IntLatRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  y_Data = 2*pi/3.0D0*(ro**3-ri**3) * xyz_Data(1,:,1)
  call AssertEqual(&
       message='y_IntLonRad_xyz with sin^2¦Õ field',               &
       answer = y_Data,                                            &
       check = y_IntLonRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  Data = 4*pi/9*(ro**3-ri**3)
  call AssertEqual(&
       message='IntLonLatRad_xyz with constant field',              &
       answer = Data,                                            &
       check = IntLonLatRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

!=============================== Ê¿¶Ñ =================================

 !---------- Äê¿ô ----------
  xyz_Data = 1.0D0

  yz_Data = 1.0D0
  call AssertEqual(&
       message='yz_AvrLon_xyz with constant field',                &
       answer = yz_Data,                                           &
       check = yz_AvrLon_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xz_Data = 1.0D0
  call AssertEqual(&
       message='xz_AvrLat_xyz with constant field',                &
       answer = xz_Data,                                           &
       check = xz_AvrLat_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xy_Data = 1.0D0
  call AssertEqual(&
       message='xy_AvrRad_xyz with constant field',                &
       answer = xy_Data,                                           &
       check = xy_AvrRad_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  z_Data = 1.0D0
  call AssertEqual(&
       message='z_AvrLonLat_xyz with constant field',              &
       answer = z_Data,                                            &
       check = z_AvrLonLat_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  x_Data = 1.0D0
  call AssertEqual(&
       message='x_AvrLatRad_xyz with constant field',              &
       answer = x_Data,                                            &
       check = x_AvrLatRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  y_Data = 1.0D0
  call AssertEqual(&
       message='y_AvrLonRad_xyz with constant field',              &
       answer = y_Data,                                            &
       check = y_AvrLonRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  Data = 1.0D0
  call AssertEqual(&
       message='AvrLonLatRad_xyz with constant field',              &
       answer = Data,                                            &
       check = AvrLonLatRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  !------------------ r -----------------------
  xyz_Data = xyz_Rad 

  yz_Data = xyz_Rad(1,:,:)
  call AssertEqual(&
       message='yz_AvrLon_xyz with r-field',                &
       answer = yz_Data,                                           &
       check = yz_AvrLon_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xz_Data = xyz_Data(:,1,:)
  call AssertEqual(&
       message='xz_AvrLat_xyz with r-field',                &
       answer = xz_Data,                                           &
       check = xz_AvrLat_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xy_Data = 1.0D0/4.0D0*(ro**4-ri**4)/(1/3.0D0*(ro**3-ri**3))
  call AssertEqual(&
       message='xy_AvrRad_xyz with r field',                &
       answer = xy_Data,                                           &
       check = xy_AvrRad_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  z_Data = xyz_Data(1,1,:)
  call AssertEqual(&
       message='z_AvrLonLat_xyz with r field',                     &
       answer = z_Data,                                            &
       check = z_AvrLonLat_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  x_Data = 1/2.0D0*(ro**4-ri**4)/(2/3.0D0*(ro**3-ri**3))
  call AssertEqual(&
       message='x_AvrLatRad_xyz with r field',                     &
       answer = x_Data,                                            &
       check = x_AvrLatRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  y_Data = pi/2.0D0*(ro**4-ri**4)/(2*pi/3.0D0*(ro**3-ri**3))
  call AssertEqual(&
       message='y_AvrLonRad_xyz with constant field',              &
       answer = y_Data,                                            &
       check = y_AvrLonRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  Data = pi*(ro**4-ri**4)/(4*pi/3*(ro**3-ri**3))
  call AssertEqual(&
       message='AvrLonLatRad_xyz with constant field',              &
       answer = Data,                                            &
       check = AvrLonLatRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )


  !--------- sin(phi)**2  ---------
  xyz_Data = sin(xyz_Lat)**2

  yz_Data = xyz_Data(1,:,:)
  call AssertEqual(&
       message='yz_AvrLon_xyz with sin^2¦Õ field',                 &
       answer = yz_Data,                                           &
       check = yz_AvrLon_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xz_Data = 1.0D0/3.0D0
  call AssertEqual(&
       message='xz_AvrLat_xyz with sin^2¦Õ field',                 &
       answer = xz_Data,                                           &
       check = xz_AvrLat_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  xy_Data = xyz_Data(:,:,1)
  call AssertEqual(&
       message='xy_AvrRad_xyz with sin^2¦Õ field',                 &
       answer = xy_Data,                                           &
       check = xy_AvrRad_xyz(xyz_Data),                            &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  z_Data = 1.0D0/3.0D0
  call AssertEqual(&
       message='z_AvrLonLat_xyz with sin^2¦Õ field',               &
       answer = z_Data,                                            &
       check = z_AvrLonLat_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  x_Data = 1.0D0/3.0D0
  call AssertEqual(&
       message='x_AvrLatRad_xyz with sin^2¦Õ field',               &
       answer = x_Data,                                            &
       check = x_AvrLatRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  y_Data = xyz_Data(1,:,1)
  call AssertEqual(&
       message='y_AvrLonRad_xyz with sin^2¦Õ field',               &
       answer = y_Data,                                            &
       check = y_AvrLonRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  Data = 1.0D0/3D0
  call AssertEqual(&
       message='AvrLonLatRad_xyz with constant field',              &
       answer = Data,                                            &
       check = AvrLonLatRad_xyz(xyz_Data),                          &
       significant_digits = check_digits, ignore_digits = ignore   &
       )

  call MessageNotify('M','wt_test_intavr', &
       'wt_module integration/averaging function tests succeeded!')

end program wt_module_sjpack_integral_test
