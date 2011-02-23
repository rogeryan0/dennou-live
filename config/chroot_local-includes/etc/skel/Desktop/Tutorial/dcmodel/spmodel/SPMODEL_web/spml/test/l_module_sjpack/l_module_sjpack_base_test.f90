!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  l_module_sjpack テストプログラム :: 基本変換関数のテスト
!
!履歴  2009/09/22  竹広真一
!
program l_module_sjpack_base_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use l_module_sjpack
  implicit none

  integer, parameter :: jm=16, nm=10

  real(8), dimension(0:nm) :: l_data
  real(8), dimension(1:jm) :: y_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','l_module_sjpack_base_test', &
                         'l_module_sjpack basic functions tests') 

  call l_Initial( nm, jm )

  !---- P_1 のテスト ----
  y_data = sqrt(3.0D0)*sin(y_Lat)
  l_data= 0.0D0 ; l_data(1)=1.0D0

  call AssertEqual(&
    message='P_1 Test of l_y(y_data)',                            &
    answer = l_data,                                              &
    check = l_y(y_data),                                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='P_1 Test of y_l(l_y(y_data))',                       &
    answer = y_data,                                              &
    check = y_l(l_y(y_data)),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- P_2 のテスト ----
  y_data = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)                 ! P_2
  l_data= 0.0D0 ; l_data(2)=1.0D0

  call AssertEqual(&
    message='P_2 Test of l_y(y_data)',                            &
    answer = l_data,                                              &
    check = l_y(y_data),                                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='P_2 Test of y_l(l_y(y_data))',                       &
    answer = y_data,                                              &
    check = y_l(l_y(y_data)),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 一般的関数のテスト ----
  y_data = (sin(y_Lat)-1)**2*(sin(y_Lat)-0.5)*(sin(y_Lat)+1)

  call AssertEqual(&
    message='General func test of y_l(l_y(y_data))',              &
    answer = y_data,                                              &
    check = y_l(l_y(y_data)),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','l_module_sjpack_base_test', &
                         'l_module_sjpack basic functions tests succeeded!') 

end program l_module_sjpack_base_test

