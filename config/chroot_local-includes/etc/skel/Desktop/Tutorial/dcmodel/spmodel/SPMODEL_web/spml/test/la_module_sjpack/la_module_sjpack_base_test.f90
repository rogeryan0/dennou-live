!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  la_module_sjpack テストプログラム :: 基本変換関数のテスト
!
!履歴  2009/09/22  竹広真一
!
program la_module_sjpack_base_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use la_module_sjpack
  implicit none

  integer, parameter :: jm=16, nm=10, km=3

  real(8), dimension(0:nm,km)  ::  la_data
  real(8), dimension(1:jm,km)  ::  ya_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','la_module_sjpack_base_test', &
                         'la_module_sjpack basic functions tests') 

  call la_Initial( nm, jm )

  !---- P_* のテスト ----
  la_data= 0.0D0 
  ya_data(:,1) = sqrt(3.0D0)*sin(y_Lat)                   ! P_1
  la_data(1,1)=1.0D0
  ya_data(:,2) = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)  ! P_2
  la_data(2,2)=1.0D0
  ya_data(:,3) = sqrt(7.0D0)*(5.0/2*sin(y_Lat)**3-3/2.0*sin(y_Lat))  ! P_3
  la_data(3,3)=1.0D0

  call AssertEqual(&
    message='P_* Test of la_ya(ya_data)',                         &
    answer = la_data,                                             &
    check = la_ya(ya_data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='P_* Test of ya_la(la_ya(ya_data))',                  &
    answer = ya_data,                                             &
    check = ya_la(la_ya(ya_data)),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 一般的関数のテスト ----
  ya_data(:,1) = (sin(y_Lat)-1)**2*(sin(y_Lat)-0.5)*(sin(y_Lat)+1)
  ya_data(:,2) = (sin(y_Lat)-1)**2*(sin(y_Lat)+1)**2
  ya_data(:,3) = (sin(y_Lat)-1)*(sin(y_Lat)-0.5)**2*(sin(y_Lat)+1)

  call AssertEqual(&
    message='general function Test of ya_la(la_ya(ya_data))',     &
    answer = ya_data,                                             &
    check = ya_la(la_ya(ya_data)),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','la_module_sjpack_base_test', &
                         'la_module_sjpack basic functions tests succeeded!') 

end program la_module_sjpack_base_test
