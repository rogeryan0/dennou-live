!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ee_module_fftj テストプログラム (ヤコビアン計算)
!
!履歴  2009/09/09  竹広真一  ee_test_jacobian2.f90 より ee_module_fftj 用に改造
!
program ee_module_fftj_jacobian_test2

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
  real(8)            :: yx_A(0:jm-1,0:im-1)     ! 格子データ
  real(8)            :: yx_B(0:jm-1,0:im-1)     ! 格子データ
  real(8)            :: yx_Jacob(0:jm-1,0:im-1)  ! 格子データ

  real(8)            :: yx_A2(0:jm2-1,0:im2-1)     ! 格子データ
  real(8)            :: yx_B2(0:jm2-1,0:im2-1)     ! 格子データ
  real(8)            :: yx_Jacob2(0:jm2-1,0:im2-1)  ! 格子データ

  integer            :: k=4,l=2

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  integer            :: id1, id2

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','ee_module_fftj_jacobian_test2', &
       'ee_module_fftj jacobian function tests for multiple domains and resolutions')

 !---------------- 座標値の設定 ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax,id1)        ! スペクトル初期化
  call ee_initial(im2,jm2,km2,lm2,xmin,xmax,ymin,ymax,id2)    ! スペクトル初期化

 !------------------- 初期値設定 ----------------------

  !---------- id1 ----------
  call ee_ChangeResolutionDomain(id1)

  yx_A = sin(k*pi*yx_X)
  yx_B = sin(l*pi*yx_Y)
  yx_Jacob = k*pi*cos(k*pi*yx_X) * l*pi*cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with sin(k*pi*X) and sin(l*pi*Y) for id=1', &
    answer = yx_Jacob,                                            &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A = cos(k*pi*yx_X)
  yx_B = cos(l*pi*yx_Y)
  yx_Jacob = k*pi*sin(k*pi*yx_X) * l*pi*sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with cos(k*pi*X) and cos(l*pi*Y) for id=1', &
    answer = yx_Jacob,                                            &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A = sin(k*pi*yx_X)
  yx_B = cos(l*pi*yx_Y)
  yx_Jacob = - k*pi*cos(k*pi*yx_X) * l*pi*sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with cos(k*pi*X) and sin(l*pi*Y) for id=1', &
    answer = yx_Jacob,                                            &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A = cos(k*pi*yx_X)
  yx_B = sin(l*pi*yx_Y)
  yx_Jacob = - k*pi*sin(k*pi*yx_X) * l*pi*cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with cos(k*pi*X) and sin(l*pi*Y) for id=1', &
    answer = yx_Jacob,                                            &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---------- id2 ----------
  call ee_ChangeResolutionDomain(id2)

  yx_A2 = sin(k*pi*yx_X)
  yx_B2 = sin(l*pi*yx_Y)
  yx_Jacob2 = k*pi*cos(k*pi*yx_X) * l*pi*cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with sin(k*pi*X) and sin(l*pi*Y) for id=2', &
    answer = yx_Jacob2,                                           &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A2),ee_yx(yx_B2))),  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A2 = cos(k*pi*yx_X)
  yx_B2 = cos(l*pi*yx_Y)
  yx_Jacob2 = k*pi*sin(k*pi*yx_X) * l*pi*sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with cos(k*pi*X) and cos(l*pi*Y) for id=2', &
    answer = yx_Jacob2,                                           &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A2),ee_yx(yx_B2))),  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A2 = sin(k*pi*yx_X)
  yx_B2 = cos(l*pi*yx_Y)
  yx_Jacob2 = - k*pi*cos(k*pi*yx_X) * l*pi*sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with cos(k*pi*X) and sin(l*pi*Y) for id=2', &
    answer = yx_Jacob2,                                           &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A2),ee_yx(yx_B2))),  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A2 = cos(k*pi*yx_X)
  yx_B2 = sin(l*pi*yx_Y)
  yx_Jacob2 = - k*pi*sin(k*pi*yx_X) * l*pi*cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with cos(k*pi*X) and sin(l*pi*Y) for id=2', &
    answer = yx_Jacob2,                                           &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A2),ee_yx(yx_B2))),  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ee_module_fftj_jacobian_test2', &
       'ee_module_fftj jacobian function tests &
       &for multiple domains and resolutions succeeded!')

end program ee_module_fftj_jacobian_test2
