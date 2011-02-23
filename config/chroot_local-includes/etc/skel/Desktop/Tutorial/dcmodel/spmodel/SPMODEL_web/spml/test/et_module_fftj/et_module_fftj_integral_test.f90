!----------------------------------------------------------------------
!   Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  et_module_fftj テストプログラム (積分・平均計算)
!
!履歴  2009/09/11  竹広真一  et_test_intavr.f90 を fftj 用に改造
!
program et_module_fftj_integral_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use et_module_fftj
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32            ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=32             ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_Data(0:jm,0:im-1)    ! 格子データ
  real(8)            :: x_Data(0:im-1)          ! 格子データ
  real(8)            :: y_Data(0:jm)            ! 格子データ

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 8
  integer, parameter :: ignore = -9

  call MessageNotify('M','et_module_fftj_integral_test', &
       'et_module_fftj integral and averaging functions tests')

 !---------------- 座標値の設定 ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

 !------------------- 関数チェック ----------------------
  yx_Data = sin(pi*yx_X) * yx_Y**2

  call AssertEqual(&
    message='IntYX_yx with sin(pi*yx_X) * yx_Y**2',               &
    answer = 0.0D0,                                               &
    check = IntYX_yx(yx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='AvrYX_yx with sin(pi*yx_X) * yx_Y**2',               &
    answer = 0.0D0,                                               &
    check = AvrYX_yx(yx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Data = 0.0D0
  call AssertEqual(&
    message='y_IntX_yx with sin(pi*yx_X) * yx_Y**2',              &
    answer = y_Data,                                              &
    check = y_IntX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='y_AvrX_yx with sin(pi*yx_X) * yx_Y**2',              &
    answer = y_Data,                                              &
    check = y_AvrX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Data = 2/3.0d0 * sin(pi*x_X)
  call AssertEqual(&
    message='x_IntY_yx with sin(pi*yx_X) * yx_Y**2',              &
    answer = x_Data,                                              &
    check = x_IntY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  x_Data = 1/3.0d0 * sin(pi*x_X)
  call AssertEqual(&
    message='x_AvrY_yx with sin(pi*yx_X) * yx_Y**2',              &
    answer = x_Data,                                              &
    check = x_AvrY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = (1 - sin(pi*yx_X)) * cos(pi*yx_Y/2)

  call AssertEqual(&
    message='IntYX_yx with (1-sin(pi*X))*cos(pi*Y/2)',            &
    answer = 8/pi,                                                &
    check = IntYX_yx(yx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='AvrYX_yx with (1-sin(pi*X))*cos(pi*Y/2)',            &
    answer = 2/pi,                                                &
    check = AvrYX_yx(yx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Data = 2*cos(pi*y_Y/2)
  call AssertEqual(&
    message='y_IntX_yx with (1-sin(pi*X))*cos(pi*Y/2)',           &
    answer = y_Data,                                              &
    check = y_IntX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  y_Data = cos(pi*y_Y/2)
  call AssertEqual(&
    message='y_AvrX_yx with (1-sin(pi*X))*cos(pi*Y/2)',           &
    answer = y_Data,                                              &
    check = y_AvrX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Data = 4/pi * (1 - sin(pi*x_X))
  call AssertEqual(&
    message='x_IntY_yx with (1-sin(pi*X))*cos(pi*Y/2)',           &
    answer = x_Data,                                              &
    check = x_IntY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  x_Data = 2/pi * (1 - sin(pi*x_X))
  call AssertEqual(&
    message='x_AvrY_yx with (1-sin(pi*X))*cos(pi*Y/2)',           &
    answer = x_Data,                                              &
    check = x_AvrY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Data = 1 - sin(2*pi*x_X)

  call AssertEqual(&
    message='IntX_x with 1-sin(2*pi*X)',                          &
    answer = 2.0D0,                                               &
    check = IntX_x(x_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='AvrX_x with 1-sin(2*pi*X)',                          &
    answer = 1.0D0,                                               &
    check = AvrX_x(x_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Data = cos(pi*y_Y/2)
  write(6,*) 'f = '

  call AssertEqual(&
    message='IntY_y with cos(pi*Y/2)',                            &
    answer = 4/pi,                                                &
    check = IntY_y(y_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='AvrY_y with cos(pi*Y/2)',                            &
    answer = 2/pi,                                                &
    check = AvrY_y(y_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Data = cos(pi*y_Y)**2
  write(6,*)
  write(6,*) 'f = cos(pi*Y)**2'

  call AssertEqual(&
    message='IntY_y with cos(pi*Y)**2',                           &
    answer = 1.0D0,                                               &
    check = IntY_y(y_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='AvrY_y with cos(pi*Y)**2',                           &
    answer = 0.5D0,                                               &
    check = AvrY_y(y_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','et_module_fftj_integral_test', &
       'et_module_fftj integral and averaging functions tests succeeded!')

end program et_module_fftj_integral_test

