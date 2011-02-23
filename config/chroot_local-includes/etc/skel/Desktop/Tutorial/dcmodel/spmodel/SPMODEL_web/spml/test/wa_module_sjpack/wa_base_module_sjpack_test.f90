!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_base_module テストプログラム :: 基本変換関数のテスト
!
!履歴  2009/09/05  竹広真一
!
program wa_base_module_sjpack_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wa_module_sjpack
  implicit none

  integer, parameter :: imm=32, jmm=16, nmm=10, kmm=3

  real(8), dimension((nmm+1)**2,kmm)    ::  wa_data
  real(8), dimension(0:imm-1,1:jmm,kmm) ::  xya_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','wa_test_base', &
                         'wa_base_module functions tests') 

  call wa_Initial( nmm, imm, jmm, kmm )
!!$  call w_Initial( nmm, imm, jmm )

  !---- Y_1^* のテスト ----
  xya_data(:,:,1) = sqrt(3.0D0)*sin(xy_Lat)                   ! Y_1^0
  wa_data= 0.0D0 ; wa_data(l_nm(1,0),1)=1.0D0
  xya_data(:,:,2) = sqrt(3.0D0/2)*cos(xy_Lat)*cos(xy_Lon)     ! Y_1^1
  wa_data(l_nm(1,1),2)=1.0D0/2.0D0
  xya_data(:,:,3) = -sqrt(3.0D0/2)*cos(xy_Lat)*sin(xy_Lon)    ! Y_1^{-1}
  wa_data(l_nm(1,-1),3)=1.0D0/2.0D0

  call AssertEqual(&
    message='wa_xya with Y_1^*',                                  &
    answer = wa_xya(xya_data),                                    &
    check = wa_data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='xya_wa with Y_1^*',                                  &
    answer = xya_wa(wa_xya(xya_data)),                            &
    check = xya_data,                                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_2^* のテスト ----
  xya_data(:,:,1) = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)           ! Y_2^0
  wa_data= 0.0D0 ; wa_data(l_nm(2,0),1)=1.0D0
  xya_data(:,:,2) = sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat)*cos(xy_Lon)!Y_2^1
  wa_data(l_nm(2,1),2)=1.0D0/2.0D0
  xya_data(:,:,3) = -sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*sin(2*xy_Lon)   ! Y_2^-2
  wa_data(l_nm(2,-2),3)=1.0D0/2.0D0

  call AssertEqual(&
    message='wa_xya with Y_2^*',                                  &
    answer = wa_xya(xya_data),                                    &
    check = wa_data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='xya_wa with Y_2^*',                                  &
    answer = xya_wa(wa_xya(xya_data)),                            &
    check = xya_data,                                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 一般的関数のテスト ----
  xya_data(:,:,1) = cos(2*xy_Lon-pi/3) &
       *(sin(xy_Lat)-1)**2*(sin(xy_Lat)-0.5)*(sin(xy_Lat)+1)
  xya_data(:,:,2) = sin(2*xy_Lon+pi/2) &
       *(sin(xy_Lat)-1)**2*(sin(xy_Lat)+1)**2
  xya_data(:,:,3) = cos(2*xy_Lon+pi/3) &
       *(sin(xy_Lat)-1)*(sin(xy_Lat)-0.5)**2*(sin(xy_Lat)+1)

  call AssertEqual(&
    message='wa_xya, xya_wa with a general function',             &
    answer = xya_wa(wa_xya(xya_data)),                            &
    check = xya_data,                                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wa_base_module_sjpack', &
                         'wa_base_module_sjpack functions tests succeeded!') 

end program wa_base_module_sjpack_test
