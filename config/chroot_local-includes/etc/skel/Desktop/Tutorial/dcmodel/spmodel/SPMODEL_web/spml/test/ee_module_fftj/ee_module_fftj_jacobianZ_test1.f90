!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ee_module_fftj テストプログラム (渦度方程式移流項ヤコビアン計算)
!
!履歴  2009/09/09  竹広真一  ee_test_jacobianZ.f90 より ee_module_fftj 用に改造
!
program ee_module_fftj_jacobianZ_test1

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ee_module_fftj
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=16, jm=16            ! 格子点の設定(X,Y)
  integer, parameter :: km=5, lm=5              ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: yx_JacobZ(0:jm-1,0:im-1)  ! 格子データ

  integer            :: k=3,l=1

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','ee_module_fftj_jacobianZ_test1', &
       'ee_module_fftj jacobianZ function tests')

 !---------------- 座標値の設定 ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

 !------------------- 関数チェック ----------------------
  yx_Data = sin(k*pi*yx_X) + cos(l*pi*yx_Y)
  yx_JacobZ = (dble(k)/l-dble(l)/k)*cos(k*pi*yx_X)*sin(l*pi*yx_Y)
  call AssertEqual(&
    message='ee_JacobianZ_ee with sin(k*pi*X)+cos(l*pi*Y)',       &
    answer = yx_JacobZ,                                           &
    check = yx_ee(ee_JacobianZ_ee(ee_yx(yx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ee_module_fftj_jacobianZ_test1', &
       'ee_module_fftj jacobianZ function tests succeeded!')

end program ee_module_fftj_jacobianZ_test1
