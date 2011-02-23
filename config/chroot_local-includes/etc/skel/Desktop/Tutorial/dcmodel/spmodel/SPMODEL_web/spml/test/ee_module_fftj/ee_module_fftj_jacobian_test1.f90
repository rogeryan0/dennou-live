!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ee_module_fftj テストプログラム (ヤコビアン計算)
!
!履歴  2009/09/09  竹広真一  ee_test_jacobian.f90 より ee_module_fftj 用に改造
!
program ee_module_fftj_jacobian_test1

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ee_module_fftj
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=16, jm=16            ! 格子点の設定(X,Y)
  integer, parameter :: km=5, lm=5              ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_A(0:jm-1,0:im-1)     ! 格子データ
  real(8)            :: yx_B(0:jm-1,0:im-1)     ! 格子データ
  real(8)            :: yx_Jacob(0:jm-1,0:im-1)  ! 格子データ

  integer            :: k=4,l=2

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','ee_module_fftj_jacobian_test1', &
       'ee_module_fftj jacobian function tests')

 !---------------- 座標値の設定 ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  yx_A = sin(k*pi*yx_X)
  yx_B = sin(l*pi*yx_Y)
  yx_Jacob = k*pi*cos(k*pi*yx_X) * l*pi*cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with sin(k*pi*X) and sin(l*pi*Y)', &
    answer = yx_Jacob,                                            &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A = cos(k*pi*yx_X)
  yx_B = cos(l*pi*yx_Y)
  yx_Jacob = k*pi*sin(k*pi*yx_X) * l*pi*sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with cos(k*pi*X) and cos(l*pi*Y)', &
    answer = yx_Jacob,                                            &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A = sin(k*pi*yx_X)
  yx_B = cos(l*pi*yx_Y)
  yx_Jacob = - k*pi*cos(k*pi*yx_X) * l*pi*sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with cos(k*pi*X) and sin(l*pi*Y)', &
    answer = yx_Jacob,                                            &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A = cos(k*pi*yx_X)
  yx_B = sin(l*pi*yx_Y)
  yx_Jacob = - k*pi*sin(k*pi*yx_X) * l*pi*cos(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_Jacobian_ee_ee with cos(k*pi*X) and sin(l*pi*Y)', &
    answer = yx_Jacob,                                            &
    check = yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ee_module_fftj_jacobian_test1', &
       'ee_module_fftj jacobian function tests succeeded!')

end program ee_module_fftj_jacobian_test1
