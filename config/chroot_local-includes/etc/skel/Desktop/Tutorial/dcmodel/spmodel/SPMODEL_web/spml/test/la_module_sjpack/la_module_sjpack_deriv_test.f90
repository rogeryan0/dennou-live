!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!É½Âê  la_module_sjpack ¥Æ¥¹¥È¥×¥í¥°¥é¥à :: ÈùÊ¬´Ø¿ô¤Î¥Æ¥¹¥È
!
!ÍúÎò  2009/09/22  ÃÝ¹­¿¿°ì
!
program la_module_sjpack_deriv_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use la_module_sjpack
  implicit none

  integer, parameter :: jm=16, nm=10, km=2

  real(8), dimension(1:jm,km)     ::  ya_data               ! ¸µ¤Î´Ø¿ô
  real(8), dimension(1:jm,km)     ::  ya_ddata              ! ÈùÊ¬¤ÎÀµ²ò
  real(8), dimension(1:jm)        ::  y_mu                  ! ¦Ì=sin¦Õ
  real(8), parameter              ::  eps = 1.0D-10

  ! È½Äê¸íº¹ÀßÄê
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','la_module_sjpack_deriv_test', &
                         'la_module_sjpack derivative function tests') 

  call la_Initial( nm, jm )

  !---- P_1 P_2 ¤Î¥Æ¥¹¥È ----
  ya_data(:,1) = sqrt(3.0D0)*sin(y_Lat)                   ! P_1
  ya_data(:,2) = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)  ! P_2

  ya_ddata(:,1) = -2*sqrt(3.0D0)*sin(y_Lat)                   ! wa_Lapla_wa
  ya_ddata(:,2) = -6*sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)  ! 

  call AssertEqual(&
    message='P_1, P_2 Test of la_Lapla_la',                       &
    answer = ya_ddata,                                            &
    check = ya_la(la_Lapla_la(la_ya(ya_data))),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  ya_ddata(:,1) = -1.0D0/2.0*sqrt(3.0D0)*sin(y_Lat)      ! wa_LaplaInv_wa
  ya_ddata(:,2) = -1.0D0/6.0*sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)

  call AssertEqual(&
    message='P_1, P_2 Test of la_LaplaInv_la',                    &
    answer = ya_ddata,                                            &
    check = ya_la(la_LaplaInv_la(la_ya(ya_data))),                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  ya_ddata(:,1) = sqrt(3.0D0)*cos(y_Lat)         ! xya_GradLat_wa
  ya_ddata(:,2) = sqrt(5.0D0)*3.0*sin(y_Lat)*cos(y_Lat)

  call AssertEqual(&
    message='P_1, P_2 Test of ya_GradLat_la',                     &
    answer = ya_ddata,                                            &
    check = ya_GradLat_la(la_ya(ya_data)),                        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- P_1 cos¦Õ, P_2 cos¦Õ ¤Î¥Æ¥¹¥È ----
  ya_data(:,1) = sqrt(3.0D0)*sin(y_Lat)*cos(y_Lat)        ! P_1 cos¦Õ
  ya_data(:,2) = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)*cos(y_Lat) ! P_2 cos¦Õ

  ya_ddata(:,1) = sqrt(3.0D0)*(cos(y_Lat)**2-2*sin(y_Lat)**2)  ! wa_DivLat_xy
  ya_ddata(:,2) = sqrt(5.0D0)*(3.0*(sin(y_Lat)*cos(y_Lat)**2-sin(y_Lat)**3) &
                               +sin(y_Lat))                    ! wa_DivLat_xy

  call AssertEqual(&
    message='P_1 cos¦Õ, P_2 cos¦Õ Test of la_DivLat_ya',          &
    answer = ya_ddata,                                            &
    check = ya_la(la_DivLat_ya(ya_data)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !============== ÈùÊ¬·×»» (¦Ë,¦ÌºÂÉ¸·ÏÍÑ) ¤Î¥Æ¥¹¥È ==============
  y_mu = sin(y_Lat)

  !----- P_2, P_1 ¤Î¥Æ¥¹¥È -----
  ya_data(:,1) = 3*y_mu**2-1                              ! P_2
  ya_data(:,2) = 5*y_mu**3/2 -3*y_mu/2                    ! P_3

  ya_ddata(:,1) = 6*y_mu*(1-y_mu**2)
  ya_ddata(:,2) = (15*y_mu**2/2 -3/2.0D0)*(1-y_mu**2)

  call AssertEqual(&
    message='P_2,P_3 Test of ya_GradMu_la',                       &
    answer = ya_ddata,                                            &
    check = ya_GradMu_la(la_ya(ya_data)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !----- P_2 (1-¦Ì^2), P_1 (1-¦Ì^2) ¤Î¥Æ¥¹¥È -----
  ya_data(:,1) = (3*y_mu**2-1)*(1-y_mu**2)           ! P_2 (1-¦Ì^2)
  ya_data(:,2) = (5*y_mu**3/2 -3*y_mu/2)*(1-y_mu**2) ! P_3 (1-¦Ì^2)

  ya_ddata(:,1) = (2-3*y_mu**2)*4*y_mu
  ya_ddata(:,2) = (15*y_mu**2/2 -3.0D0/2)*(1-y_mu**2) &
                - (5*y_mu**3/2 -3*y_mu/2)*2*y_mu

  call AssertEqual(&
    message='P_2 (1-¦Ì^2), P_3 (1-¦Ì^2) Test of la_DivMu_ya',     &
    answer = ya_ddata,                                            &
    check = ya_la(la_DivMu_ya(ya_data)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','la_module_sjpack_deriv_test', &
                         'la_module_sjpack derivative function tests succeeded!') 

end program la_module_sjpack_deriv_test
