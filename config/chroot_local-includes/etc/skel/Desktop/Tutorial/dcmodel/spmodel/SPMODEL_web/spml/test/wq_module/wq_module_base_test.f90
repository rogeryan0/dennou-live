!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wq_module テストプログラム :: 基本変換関数のテスト
!
!履歴  2009/12/11  竹広真一
!
program wq_module_base_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wq_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=8    ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=15          ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ra=2.0                ! 球半径

  real(8), dimension(0:im-1,1:jm,km)       ::  xyr_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','wq_module_base_test', &
                         'wq_module basic transformation functions tests') 

  call wq_Initial(im,jm,km,nm,lm,ra)

  !---- Y_1^* のテスト ----
  xyr_data = sqrt(3.0D0)*sin(xyr_Lat)*xyr_Rad        ! r Y_1^0 
  call AssertEqual(&
    message='xyr_wr with r Y_1^0',                                &
    answer = xyr_data,                                            &
    check = xyr_wq(wq_xyr(xyr_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xyr_data = sqrt(3.0D0/2)*cos(xyr_Lat)*cos(xyr_Lon)*xyr_Rad**3   ! r^3Y_1^1 
  call AssertEqual(&
    message='xyr_wq with r^3 Y_1^1',                              &
    answer = xyr_data,                                            &
    check = xyr_wq(wq_xyr(xyr_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xyr_data = -sqrt(3.0D0/2)*cos(xyr_Lat)*sin(xyr_Lon) * xyr_Rad**5  !r^5Y_1^{-1}
  call AssertEqual(&
    message='xyr_wq with r^5 Y_1^-1',                             &
    answer = xyr_data,                                            &
    check = xyr_wq(wq_xyr(xyr_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_2^* のテスト ----
  !r^2 Y_2^0
  xyr_data = sqrt(5.0D0)*(3.0/2*sin(xyr_Lat)**2-1/2.0) * xyr_Rad**2
  call AssertEqual(&
    message='xyr_wq with r^2 Y_2^0',                              &
    answer = xyr_data,                                            &
    check = xyr_wq(wq_xyr(xyr_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !r^4 Y_2^1 
  xyr_data = sqrt(5.0D0/6)*3.0*sin(xyr_Lat)*cos(xyr_Lat)*cos(xyr_Lon) &
            * xyr_Rad**4
  call AssertEqual(&
    message='xyr_wq with r^4 Y_2^1',                              &
    answer = xyr_data,                                            &
    check = xyr_wq(wq_xyr(xyr_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !r^6 Y_2^-2
  xyr_data = -sqrt(5.0D0/24)*3.0*cos(xyr_Lat)**2*sin(2*xyr_Lon)* xyr_Rad**6
  call AssertEqual(&
    message='xyr_wq with r^6 Y_2^-2',                             &
    answer = xyr_data,                                            &
    check = xyr_wq(wq_xyr(xyr_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wq_module_base_test_base', &
                         'wq_module basic functions tests succeeded!') 

end program wq_module_base_test
