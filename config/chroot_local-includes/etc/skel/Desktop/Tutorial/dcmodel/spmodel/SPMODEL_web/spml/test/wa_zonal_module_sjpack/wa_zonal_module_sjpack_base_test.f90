!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wa_zonal_module_sjpack テストプログラム :: 基本変換関数のテスト
!
!履歴  2009/09/23  竹広真一
!
program wa_zonal_module_sjpack_base_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wa_zonal_module_sjpack
  implicit none

  integer, parameter :: im=1, jm=16, nm=10, km=3

  real(8), dimension(nm+1,km)         ::  wa_data
  real(8), dimension(0:im-1,1:jm,km)  ::  xya_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','wa_zonal_module_sjpack_base_test', &
                         'wa_zonal_module_sjpack basic functions tests') 

  call wa_Initial( nm, im, jm, km )

  !---- Y_*^0 のテスト ----
  wa_data= 0.0D0 
  xya_data(:,:,1) = sqrt(3.0D0)*sin(xy_Lat)                   ! Y_1^0
  wa_data(l_nm(1,0),1)=1.0D0

  xya_data(:,:,2) = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)  ! Y_2^0
  wa_data(l_nm(2,0),2)=1.0D0

  xya_data(:,:,3) = sqrt(7.0D0)*(5.0/2*sin(xy_Lat)**3-3/2.0*sin(xy_Lat))! Y_3^0
  wa_data(l_nm(3,0),3)=1.0D0

  call AssertEqual(&
    message='Y_*^0 Test of wa_xya(xya_data)',                     &
    answer = wa_data,                                             &
    check = wa_xya(xya_data),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_*^0 Test of xya_wa(wa_xya(xya_data))',             &
    answer = xya_data,                                            &
    check = xya_wa(wa_xya(xya_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 一般的関数のテスト ----
  xya_data(:,:,1) = (sin(xy_Lat)-1)**2*(sin(xy_Lat)-0.5)*(sin(xy_Lat)+1)
  xya_data(:,:,2) = (sin(xy_Lat)-1)**2*(sin(xy_Lat)+1)**2
  xya_data(:,:,3) = (sin(xy_Lat)-1)*(sin(xy_Lat)-0.5)**2*(sin(xy_Lat)+1)

  call AssertEqual(&
    message='general function Test of xya_wa(wa_xya(xya_data))',  &
    answer = xya_data,                                            &
    check = xya_wa(wa_xya(xya_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wa_zonal_module_sjpack_base_test', &
                         'wa_zonal_module_sjpack basic functions tests succeeded!') 

end program wa_zonal_module_sjpack_base_test
