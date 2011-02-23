!----------------------------------------------------------------------
!   Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  et_module_fftj テストプログラム (ヤコビアン計算)
!
!履歴  2009/09/11  竹広真一  et_test_jacobian.f90 を fftj 用に改造
!
program et_module_fftj_jacobian_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use et_module_fftj
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32            ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=21              ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_A(0:jm,0:im-1)     ! 格子データ
  real(8)            :: yx_B(0:jm,0:im-1)     ! 格子データ
  real(8)            :: yx_Jacob(0:jm,0:im-1)  ! 格子データ

  integer            :: k=3,l=2

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 8
  integer, parameter :: ignore = -9

  call MessageNotify('M','et_module_fftj_jacobian_test', &
       'et_module_fftj jacobian functions tests')

 !---------------- 座標値の設定 ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

 !------------------- 関数チェック ----------------------
  yx_A = sin(k*pi*yx_Y)
  yx_B = sin(l*pi*yx_Y)
  yx_Jacob = 0.0

  call AssertEqual(&
    message='et_Jacobian_et_et with sin(k*pi*X) and sin(l*pi*Y)', &
    answer = yx_Jacob,                                            &
    check = yx_et(et_Jacobian_et_et(et_yx(yx_A),et_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A = sin(k*pi*yx_X)*yx_Y
  yx_B = cos(k*pi*yx_X)*yx_Y**2
  yx_Jacob = k*pi*(1+cos(k*pi*yx_X)**2) *yx_Y**2
  call AssertEqual(&
    message='et_Jacobian_et_et with sin(k*pi*X)*yx_Y and cos(k*pi*yx_X)*Y**2', &
    answer = yx_Jacob,                                            &
    check = yx_et(et_Jacobian_et_et(et_yx(yx_A),et_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A = cos(k*pi*yx_X)*yx_Y
  yx_B = yx_Y**3
  yx_Jacob = -3*k*pi* sin(k*pi*yx_X)*yx_Y**3
  call AssertEqual(&
    message='et_Jacobian_et_et with cos(k*pi*X)*Y and Y**3',      &
    answer = yx_Jacob,                                            &
    check = yx_et(et_Jacobian_et_et(et_yx(yx_A),et_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_A = yx_Y**4
  yx_B = sin(l*pi*yx_X)
  yx_Jacob = -4*l*pi * yx_Y**3*cos(l*pi*yx_X)
  call AssertEqual(&
    message='et_Jacobian_et_et with Y**4 and sin(l*pi*X) ',       &
    answer = yx_Jacob,                                            &
    check = yx_et(et_Jacobian_et_et(et_yx(yx_A),et_yx(yx_B))),    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','et_module_fftj_jacobian_test', &
       'et_module_fftj jacobian functions tests succeded!')

end program et_module_fftj_jacobian_test
