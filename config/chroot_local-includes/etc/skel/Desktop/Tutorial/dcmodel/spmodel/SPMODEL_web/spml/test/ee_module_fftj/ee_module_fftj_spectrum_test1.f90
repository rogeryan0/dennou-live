!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ee_module_fftj テストプログラム (パワースペクトル計算)
!
!履歴  2009/09/09  竹広真一  ee_test_spectrum.f90 より ee_module_fftj 用に改造
!
program ee_module_fftj_spectrum_test1

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ee_module_fftj
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32            ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=10              ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: ee_Data(-lm:lm,-km:km)    ! スペクトルデータ

  integer            :: k=2, l=3

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','ee_module_fftj_spectrum_test1', &
       'ee_module_fftj spectrum functions tests')

 !---------------- 座標値の設定 ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

 !------------------- 関数チェック ----------------------
  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = 0
  ee_Data(l,k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data(-l,k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data(l,-k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data(-l,-k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2

  call AssertEqual(&
    message='ee_EnergyFromStreamfunc_ee with sin(k*pi*X)*sin(l*pi*Y)',&
    answer = ee_Data,                                             &
    check = ee_EnergyFromStreamfunc_ee(ee_yx(yx_Data)),           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='total energy with sin(k*pi*X)*sin(l*pi*Y)',          &
    answer = ((k*pi)**2 + (l*pi)**2)/2,                           &
    check = sum(ee_EnergyFromStreamfunc_ee(ee_yx(yx_Data)))       &
           *(xmax-xmin)*(ymax-ymin),                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  ee_Data = 0
  ee_Data(l,k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data(-l,k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data(l,-k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data(-l,-k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2

  call AssertEqual(&
    message='ee_EnstrophyFromStreamfunc_ee with sin(k*pi*X)*sin(l*pi*Y)',&
    answer = ee_Data,                                             &
    check = ee_EnstrophyFromStreamfunc_ee(ee_yx(yx_Data)),        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='total enstrophy with sin(k*pi*X)*sin(l*pi*Y)',       &
    answer = ((k*pi)**2 + (l*pi)**2)**2/2,                        &
    check = sum(ee_EnstrophyFromStreamfunc_ee(ee_yx(yx_Data)))    &
           *(xmax-xmin)*(ymax-ymin),                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ee_module_fftj_spectrum_test1', &
       'ee_module_fftj spectrum functions tests succeeded!')

end program ee_module_fftj_spectrum_test1
