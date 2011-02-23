!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!É½Âê  w_deriv_module_sjpack ¥Æ¥¹¥È¥×¥í¥°¥é¥à :: ÈùÊ¬´Ø¿ô¤Î¥Æ¥¹¥È
!
!ÍúÎò  2009/09/06  ÃÝ¹­¿¿°ì wa_deriv_module ¤è¤ê SJPACK ÍÑ¤Ë²þÂ¤
!
program wa_deriv_module_sjpack_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wa_module_sjpack
  implicit none

  integer, parameter :: im=32, jm=16, nm=10, km=2

  real(8), dimension(0:im-1,1:jm,km)     ::  xya_data1              ! ¸µ¤Î´Ø¿ô
  real(8), dimension(0:im-1,1:jm,km)     ::  xya_data2              ! ¸µ¤Î´Ø¿ô
  real(8), dimension(0:im-1,1:jm,km)     ::  xya_ddata              ! ÈùÊ¬¤ÎÀµ²ò
  real(8), dimension(0:im-1,1:jm)        ::  xy_mu                  ! ¦Ì=sin¦Õ

  ! È½Äê¸íº¹ÀßÄê
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  call MessageNotify('M','wa_deriv_module_sjpack_test', &
                         'wa_deriv_module_sjpack function tests') 

  call wa_Initial( nm, im, jm, km )

  !---- Y_1^-1 Y_2^1 ¤Î¥Æ¥¹¥È ----
  xya_data1(:,:,1) = -cos(xy_Lat)*sin(xy_Lon)             ! Y_1^{-1}
  xya_data1(:,:,2)  = sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon) ! Y_2^1

  xya_ddata(:,:,1) = 2*cos(xy_Lat)*sin(xy_Lon)             ! wa_Lapla_wa
  xya_ddata(:,:,2) = -6*sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon) ! Y_2^1
  call AssertEqual(&
    message='wa_Lapla_wa with Y_1^-1 and Y^2_1',                  &
    answer = xya_wa(wa_Lapla_wa(wa_xya(xya_data1))),              &
    check = xya_ddata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = 1.0/2.0*cos(xy_Lat)*sin(xy_Lon)      ! wa_LaplaInv_wa
  xya_ddata(:,:,2) = -1.0D0/6.0*sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)
  call AssertEqual(&
    message='wa_LaplaInv_wa with Y_1^-1 and Y^2_1',               &
    answer = xya_wa(wa_LaplaInv_wa(wa_xya(xya_data1))),           &
    check = xya_ddata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = -cos(xy_Lat)*cos(xy_Lon)         ! wa_DLon_wa
  xya_ddata(:,:,2) = -sin(xy_Lat)*cos(xy_Lat) * sin(xy_Lon)
  call AssertEqual(&
    message='wa_DLon_wa with Y_1^-1 and Y^2_1',                   &
    answer = xya_wa(wa_DLon_wa(wa_xya(xya_data1))),               &
    check = xya_ddata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = -cos(xy_Lon)                     ! xya_GradLon_wa
  xya_ddata(:,:,2) = -sin(xy_Lat) * sin(xy_Lon)
  call AssertEqual(&
    message='xya_GradLon_wa with Y_1^-1 and Y^2_1',               &
    answer = xya_GradLon_wa(wa_xya(xya_data1)),                   &
    check = xya_ddata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = sin(xy_Lat)*sin(xy_Lon)         ! xya_GradLat_wa
  xya_ddata(:,:,2) = cos(2*xy_Lat) * cos(xy_Lon)
  call AssertEqual(&
    message='xya_GradLat_wa with Y_1^-1 and Y^2_1',               &
    answer = xya_GradLat_wa(wa_xya(xya_data1)),                   &
    check = xya_ddata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_1^-1 cos¦Õ, Y_2^1 cos¦Õ ¤Î¥Æ¥¹¥È ----
  xya_data1(:,:,1) = -cos(xy_Lat)**2*sin(xy_Lon)              ! Y_1^-1 cos¦Õ
  xya_data1(:,:,2) = sin(xy_Lat)*cos(xy_Lat)**2 * cos(xy_Lon) ! Y_2^1 cos¦Õ

  xya_ddata(:,:,1) = -cos(xy_Lat)*cos(xy_Lon)                ! wa_DivLon_xy
  xya_ddata(:,:,2) = -sin(xy_Lat)*cos(xy_Lat)*sin(xy_Lon)    ! wa_DivLon_xy
  call AssertEqual(&
    message='wa_DivLon_xya with Y_1^-1 cos¦Õ and Y^2_1 cos¦Õ',    &
    answer = xya_wa(wa_DivLon_xya(xya_data1)),                    &
    check = xya_ddata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = 3*sin(xy_Lat)*cos(xy_Lat)*sin(xy_Lon)        !wa_DivLat_wa
  xya_ddata(:,:,2) = cos(xy_Lat)*(1-4*sin(xy_Lat)**2)*cos(xy_Lon) !wa_DivLat_wa
  call AssertEqual(&
    message='wa_DivLat_xya with Y_1^-1 cos¦Õ and Y^2_1 cos¦Õ',    &
    answer = xya_wa(wa_DivLat_xya(xya_data1)),                    &
    check = xya_ddata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Jacobian ¤Î¥Æ¥¹¥È ----
  xya_data1(:,:,1) = -cos(xy_Lat)*sin(xy_Lon)                ! Y_1^{-1}
  xya_data1(:,:,2) = -cos(xy_Lat)*sin(xy_Lon)                ! Y_1^{-1}
  xya_data2(:,:,1) = -cos(xy_Lat)*sin(xy_Lon)                ! Y_1^{-1}
  xya_data2(:,:,2) = sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)   ! Y_2^1

  xya_ddata(:,:,1) = 0.0
  xya_ddata(:,:,2) = sin(xy_Lat)**2 - cos(xy_Lat)**2*cos(xy_Lon)**2
   call AssertEqual(&
    message='wa_Jacobian_wa_wa Y_1^-1 and Y^2_1',                 &
    answer = xya_wa(wa_Jacobian_wa_wa(wa_xya(xya_data1),wa_xya(xya_data2))), &
    check = xya_ddata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )


  !============== ÈùÊ¬·×»» (¦Ë,¦ÌºÂÉ¸·ÏÍÑ) ¤Î¥Æ¥¹¥È ==============
  xy_mu = sin(xy_Lat)

  !----- Y_2^0, Y_1^1 ¤Î¥Æ¥¹¥È -----
  xya_data1(:,:,1) = 3*xy_mu**2-1                              ! Y_2^0
  xya_data1(:,:,2) = sqrt(1-xy_mu**2)*cos(xy_Lon)                  ! Y_1^1

  xya_ddata(:,:,1) = 0.0
  xya_ddata(:,:,2) = -sqrt(1-xy_mu**2)*sin(xy_Lon)
  call AssertEqual(&
    message='xya_GradLambda_wa with Y_2^0 and Y^1_1',             &
    answer = xya_GradLambda_wa(wa_xya(xya_data1)),                &
    check = xya_ddata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = 6*xy_mu*(1-xy_mu**2)
  xya_ddata(:,:,2) = -xy_mu*sqrt(1-xy_mu**2)*cos(xy_Lon)
  call AssertEqual(&
    message='xya_GradMu_wa with Y_2^0 and Y^1_1',                 &
    answer = xya_GradMu_wa(wa_xya(xya_data1)),                    &
    check = xya_ddata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !----- Y_2^0(1-¦Ì^2), Y_1^1 (1-¦Ì^2) ¤Î¥Æ¥¹¥È -----
  xya_data1(:,:,1) = (3*xy_mu**2-1)*(1-xy_mu**2)       ! Y_2^0 (1-¦Ì^2)
  xya_data1(:,:,2) = (1-xy_mu**2)**(3.0/2)*cos(xy_Lon) ! Y_1^1 (1-¦Ì^2)

  xya_ddata(:,:,1) = 0.0
  xya_ddata(:,:,2) = -(1-xy_mu**2)**(1.0d0/2)*sin(xy_Lon)
  call AssertEqual(&
    message='wa_DivLambda_xya with Y_2^0 (1-¦Ì^2) and Y^1_1 ¢å(1-¦Ì^2)',&
    answer = xya_wa(wa_DivLambda_xya(xya_data1)),                 &
    check = xya_ddata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = (2-3*xy_mu**2)*4*xy_mu
  xya_ddata(:,:,2) = -3.0D0*xy_mu*(1-xy_mu**2)**(1.0D0/2)*cos(xy_Lon)
  call AssertEqual(&
    message='wa_DivMu_xya with Y_2^0 (1-¦Ì^2) and Y^1_1 ¢å(1-¦Ì^2)',&
    answer = xya_wa(wa_DivMu_xya(xya_data1)),                     &
    check = xya_ddata,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wa_deriv_module_sjpack_test', &
                         'wa_deriv_module_sjpack function tests succeeded!') 

end program wa_deriv_module_sjpack_test

