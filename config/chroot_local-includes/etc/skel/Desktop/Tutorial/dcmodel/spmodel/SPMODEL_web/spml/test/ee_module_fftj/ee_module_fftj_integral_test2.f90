!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ee_module_fftj テストプログラム (積分・平均計算)
!
!履歴  2009/09/09  竹広真一  ee_test_intavr2.f90 より ee_module_fftj 用に改造
!
program ee_module_fftj_integral_test2

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ee_module_fftj
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=16, jm=16            ! 格子点の設定(X,Y)
  integer, parameter :: km=5, lm=5              ! 切断波数の設定(X,Y)

  integer, parameter :: im2=64, jm2=64            ! 格子点の設定(X,Y)
  integer, parameter :: km2=21, lm2=21            ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: x_Data(0:im-1)            ! 格子データ
  real(8)            :: y_Data(0:jm-1)            ! 格子データ

  real(8)            :: x_Ans(0:im-1)            ! 格子データ
  real(8)            :: y_Ans(0:jm-1)            ! 格子データ
  real(8)            :: Ans                      ! 

  real(8)            :: yx_Data2(0:jm2-1,0:im2-1) ! 格子データ
  real(8)            :: x_Data2(0:im2-1)          ! 格子データ
  real(8)            :: y_Data2(0:jm2-1)          ! 格子データ

  real(8)            :: x_Ans2(0:im2-1)            ! 格子データ
  real(8)            :: y_Ans2(0:jm2-1)            ! 格子データ

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  integer            :: id1, id2

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','ee_module_fftj_integral_test1', &
       'ee_module_fftj integral and average functions tests &
       &for multiple domains and resoulutions')


 !---------------- 座標値の設定 ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax,id1)        ! スペクトル初期化
  call ee_initial(im2,jm2,km2,lm2,xmin,xmax,ymin,ymax,id2)    ! スペクトル初期化

 !------------------- 関数チェック ----------------------

  !---------- id1 ----------
  write(*,*) 'for id1'
  call ee_ChangeResolutionDomain(id1)

  yx_Data = sin(pi*yx_X) * cos(pi*yx_Y)

  Ans = 0.0D0
  call AssertEqual(&
    message='IntYX_yx with sin(pi*X)*cos(pi*Y) for id=1',         &
    answer = Ans,                                                 &
    check = IntYX_yx(yx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='AvrYX_yx with sin(pi*X)*cos(pi*Y) for id=1',         &
    answer = Ans,                                                 &
    check = AvrYX_yx(yx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Ans = 0.0D0
  call AssertEqual(&
    message='y_IntX_yx with sin(pi*X)*cos(pi*Y) for id=1',        &
    answer = y_Ans,                                               &
    check = y_IntX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='y_AvrX_yx with sin(pi*X)*cos(pi*Y) for id=1',        &
    answer = y_Ans,                                               &
    check = y_AvrX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Ans = 0.0D0
  call AssertEqual(&
    message='x_IntY_yx with sin(pi*X)*cos(pi*Y) for id=1',        &
    answer = x_Ans,                                               &
    check = x_IntY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='x_AvrY_yx with sin(pi*X)*cos(pi*Y) for id=1',        &
    answer = x_Ans,                                               &
    check = x_AvrY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = (1 - sin(pi*yx_X)) * cos(pi*yx_Y)**2

  Ans = 2.0D0
  call AssertEqual(&
    message='IntYX_yx with (1-sin(pi*X)) * cos(pi*Y)**2 for id=1',&
    answer = Ans,                                                 &
    check = IntYX_yx(yx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 0.5D0
  call AssertEqual(&
    message='AvrYX_yx with (1-sin(pi*X)) * cos(pi*Y)**2 for id=1',&
    answer = Ans,                                                 &
    check = AvrYX_yx(yx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Ans = 2*cos(pi*y_Y)**2
  call AssertEqual(&
    message='y_IntX_yx with (1-sin(pi*X)) * cos(pi*Y)**2 for id=1',&
    answer = y_Ans,                                               &
    check = y_IntX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  y_Ans = cos(pi*y_Y)**2
  call AssertEqual(&
    message='y_AvrX_yx with (1-sin(pi*X)) * cos(pi*Y)**2 for id=1',&
    answer = y_Ans,                                               &
    check = y_AvrX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Ans = 1 - sin(pi*x_X)
  call AssertEqual(&
    message='x_IntY_yx with (1-sin(pi*X)) * cos(pi*Y)**2 for id=1',&
    answer = x_Ans,                                               &
    check = x_IntY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  x_Ans = 0.5*(1 - sin(pi*x_X))
  call AssertEqual(&
    message='x_AvrY_yx with (1-sin(pi*X)) * cos(pi*Y)**2 for id=1',&
    answer = x_Ans,                                               &
    check = x_AvrY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Data = 1 - sin(2*pi*x_X)

  Ans = 2.0D0
  call AssertEqual(&
    message='IntX_x with 1-sin(2*pi*X) for id=1',                 &
    answer = Ans,                                                 &
    check = IntX_x(x_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 1.0D0
  call AssertEqual(&
    message='AvrX_x with 1-sin(2*pi*X) for id=1',                 &
    answer = Ans,                                                 &
    check = AvrX_x(x_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Data = cos(pi*x_X)**2

  Ans = 1.0D0
  call AssertEqual(&
    message='IntY_y with cos(pi*X)**2 for id=1',                  &
    answer = Ans,                                                 &
    check = IntY_y(y_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 0.5D0
  call AssertEqual(&
    message='AvrY_y with cos(pi*X)**2 for id=1',                  &
    answer = Ans,                                                 &
    check = AvrY_y(y_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---------- id2 -----------

  call ee_ChangeResolutionDomain(id2)

  yx_Data2 = sin(pi*yx_X) * cos(pi*yx_Y)

  Ans = 0.0D0
  call AssertEqual(&
    message='IntYX_yx with sin(pi*X)*cos(pi*Y) for id=2',         &
    answer = Ans,                                                 &
    check = IntYX_yx(yx_Data2),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='AvrYX_yx with sin(pi*X)*cos(pi*Y) for id=2',         &
    answer = Ans,                                                 &
    check = AvrYX_yx(yx_Data2),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Ans2 = 0.0D0
  call AssertEqual(&
    message='y_IntX_yx with sin(pi*X)*cos(pi*Y) for id=2',        &
    answer = y_Ans2,                                              &
    check = y_IntX_yx(yx_Data2),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='y_AvrX_yx with sin(pi*X)*cos(pi*Y) for id=2',        &
    answer = y_Ans2,                                              &
    check = y_AvrX_yx(yx_Data2),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Ans2 = 0.0D0
  call AssertEqual(&
    message='x_IntY_yx with sin(pi*X)*cos(pi*Y) for id=2',        &
    answer = x_Ans2,                                              &
    check = x_IntY_yx(yx_Data2),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='x_AvrY_yx with sin(pi*X)*cos(pi*Y) for id=2',        &
    answer = x_Ans2,                                              &
    check = x_AvrY_yx(yx_Data2),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data2 = (1 - sin(pi*yx_X)) * cos(pi*yx_Y)**2

  Ans = 2.0D0
  call AssertEqual(&
    message='IntYX_yx with (1-sin(pi*X)) * cos(pi*Y)**2 for id=2',&
    answer = Ans,                                                 &
    check = IntYX_yx(yx_Data2),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 0.5D0
  call AssertEqual(&
    message='AvrYX_yx with (1-sin(pi*X)) * cos(pi*Y)**2 for id=2',&
    answer = Ans,                                                 &
    check = AvrYX_yx(yx_Data2),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Ans2 = 2*cos(pi*y_Y)**2
  call AssertEqual(&
    message='y_IntX_yx with (1-sin(pi*X)) * cos(pi*Y)**2 for id=2',&
    answer = y_Ans2,                                              &
    check = y_IntX_yx(yx_Data2),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  y_Ans2 = cos(pi*y_Y)**2
  call AssertEqual(&
    message='y_AvrX_yx with (1-sin(pi*X)) * cos(pi*Y)**2 for id=2',&
    answer = y_Ans2,                                              &
    check = y_AvrX_yx(yx_Data2),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Ans2 = 1 - sin(pi*x_X)
  call AssertEqual(&
    message='x_IntY_yx with (1-sin(pi*X)) * cos(pi*Y)**2 for id=2',&
    answer = x_Ans2,                                              &
    check = x_IntY_yx(yx_Data2),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  x_Ans2 = 0.5*(1 - sin(pi*x_X))
  call AssertEqual(&
    message='x_AvrY_yx with (1-sin(pi*X)) * cos(pi*Y)**2 for id=2',&
    answer = x_Ans2,                                              &
    check = x_AvrY_yx(yx_Data2),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Data2 = 1 - sin(2*pi*x_X)

  Ans = 2.0D0
  call AssertEqual(&
    message='IntX_x with 1-sin(2*pi*X) for id=2',                 &
    answer = Ans,                                                 &
    check = IntX_x(x_Data2),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 1.0D0
  call AssertEqual(&
    message='AvrX_x with 1-sin(2*pi*X) for id=2',                 &
    answer = Ans,                                                 &
    check = AvrX_x(x_Data2),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Data2 = cos(pi*x_X)**2

  Ans = 1.0D0
  call AssertEqual(&
    message='IntY_y with cos(pi*X)**2 for id=2',                  &
    answer = Ans,                                                 &
    check = IntY_y(y_Data2),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 0.5D0
  call AssertEqual(&
    message='AvrY_y with cos(pi*X)**2 for id=2',                  &
    answer = Ans,                                                 &
    check = AvrY_y(y_Data2),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ee_module_fftj_integral_test1', &
       'ee_module_fftj integral and average functions tests &
       &for multiple domains and resoulutions succeded!')

end program ee_module_fftj_integral_test2
