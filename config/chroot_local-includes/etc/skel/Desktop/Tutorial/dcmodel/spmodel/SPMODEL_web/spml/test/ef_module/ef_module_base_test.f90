!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ef_module テストプログラム (正逆変換)
!
!履歴  2009/12/11  竹広真一
!
program ef_module_base_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ef_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32            ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=10              ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: ef_Data(-lm:lm,-km:km)    ! スペクトルデータ

  integer            :: k=2,l=2

 !---- 座標変数など ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: yrad = 2.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 3
  integer, parameter :: ignore = -4

  call MessageNotify('M','ef_module_base_test', &
       'ef_module basic transform function tests')

 !---------------- 座標値の設定 ---------------------
  call ef_initial(im,jm,km,lm,xmin,xmax,yrad)    ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  yx_Data = sin(k*pi*yx_X) * exp(-yx_Y**2)

  call AssertEqual(&
    message='ef_yx, yx_ef with sin(k*pi*X)*exp(-Y**2)',           &
    answer = yx_Data,                                             &
    check = yx_ef(ef_yx(yx_Data)),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(k*pi*yx_X) * yx_Y/2 * exp(-yx_Y**2)

  call AssertEqual(&
    message='ef_yx, yx_ef with sin(k*pi*X) Y *exp(-Y**2)',&
    answer = yx_Data,                                             &
    check = yx_ef(ef_yx(yx_Data)),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ef_module_base_test', &
       'ef_module basic transform function tests succeeded!')

end program ef_module_base_test

