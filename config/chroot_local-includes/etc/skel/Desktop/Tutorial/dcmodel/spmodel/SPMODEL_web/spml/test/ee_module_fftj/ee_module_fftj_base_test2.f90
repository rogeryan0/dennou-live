!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ee_module_fftj テストプログラム (正逆変換)
!
!履歴  2009/09/09  竹広真一  ee_test_transform2.f90 より ee_module_fftj 用に改造
!
!備考  どうもスペクトル成分がすべて逆符号になっているように思える. 
!
program ee_module_fftj_base_test2

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ee_module_fftj
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32            ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=10              ! 切断波数の設定(X,Y)

  integer, parameter :: im2=64, jm2=64            ! 格子点の設定(X,Y)
  integer, parameter :: km2=21, lm2=21              ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: ee_Data(-lm:lm,-km:km)    ! スペクトルデータ

  real(8)            :: yx_Data2(0:jm2-1,0:im2-1)    ! 格子データ
  real(8)            :: ee_Data2(-lm2:lm2,-km2:km2)    ! スペクトルデータ

  real(8)            :: ee_Data3(-lm:lm,-km:km)    ! スペクトルデータ

  integer            :: k=2,l=5

  integer            :: id1, id2, id3

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  call MessageNotify('M','ee_module_fftj_base_test2', &
       'ee_module_fftj basic transform function tests (multiple domain/resolution)')

 !---------------- 座標値の設定 ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax,id1)    ! スペクトル初期化
  call ee_initial(im2,jm2,km2,lm2,xmin,xmax,ymin,ymax,id2)    ! スペクトル初期化
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax,id3)    ! スペクトル初期化
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)

 !------------------- 関数チェック ----------------------
  !---------- id1 ----------
  call ee_ChangeResolutionDomain(id1)
  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = 0.0 ; ee_Data(-l,k) = -0.25  ; ee_Data(l,k) = 0.25  

  call AssertEqual(&
    message='ee_yx with sin(k*pi*X)*sin(l*pi*Y) for id=1',        &
    answer = ee_Data,                                             &
    check = ee_yx(yx_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_ee with sin(k*pi*X)*sin(l*pi*Y) for id=1',        &
    answer = yx_Data,                                             &
    check = yx_ee(ee_yx(yx_Data)),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data = 0.0 ; ee_Data(l,k) = -0.25  ; ee_Data(-l,k) = -0.25  

  call AssertEqual(&
    message='ee_yx with cos(k*pi*X)*cos(l*pi*Y) for id=1',        &
    answer = ee_Data,                                             &
    check = ee_yx(yx_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_ee with cos(k*pi*X)*cos(l*pi*Y) for id=1',        &
    answer = yx_Data,                                             &
    check = yx_ee(ee_yx(yx_Data)),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data = 0.0 ; ee_Data(-l,-k) = 0.25  ; ee_Data(l,-k) = 0.25  

  call AssertEqual(&
    message='ee_yx with sin(k*pi*X)*cos(l*pi*Y) for id=1',        &
    answer = ee_Data,                                             &
    check = ee_yx(yx_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_ee with sin(k*pi*X)*cos(l*pi*Y) for id=1',        &
    answer = yx_Data,                                             &
    check = yx_ee(ee_yx(yx_Data)),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = 0.0 ; ee_Data(-l,-k) = 0.25  ; ee_Data(l,-k) = -0.25  

  call AssertEqual(&
    message='ee_yx with cos(k*pi*X)*sin(l*pi*Y) for id=1',        &
    answer = ee_Data,                                             &
    check = ee_yx(yx_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_ee with cos(k*pi*X)*sin(l*pi*Y) for id=1',        &
    answer = yx_Data,                                             &
    check = yx_ee(ee_yx(yx_Data)),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---------- id 2 ----------
  call ee_ChangeResolutionDomain(id2)
  yx_Data2 = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data2 = 0.0 ; ee_Data2(-l,k) = -0.25  ; ee_Data2(l,k) = 0.25  

  call AssertEqual(&
    message='ee_yx with sin(k*pi*X)*sin(l*pi*Y) for id=2',        &
    answer = ee_Data2,                                            &
    check = ee_yx(yx_Data2),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_ee with sin(k*pi*X)*sin(l*pi*Y) for id=2',        &
    answer = yx_Data2,                                            &
    check = yx_ee(ee_yx(yx_Data2)),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data2 = cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data2 = 0.0 ; ee_Data2(l,k) = -0.25  ; ee_Data2(-l,k) = -0.25  

  call AssertEqual(&
    message='ee_yx with cos(k*pi*X)*cos(l*pi*Y) for id=2',        &
    answer = ee_Data2,                                            &
    check = ee_yx(yx_Data2),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_ee with cos(k*pi*X)*cos(l*pi*Y) for id=2',        &
    answer = yx_Data2,                                            &
    check = yx_ee(ee_yx(yx_Data2)),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data2 = sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data2 = 0.0 ; ee_Data2(-l,-k) = 0.25  ; ee_Data2(l,-k) = 0.25  

  call AssertEqual(&
    message='ee_yx with sin(k*pi*X)*cos(l*pi*Y) for id=2',        &
    answer = ee_Data2,                                            &
    check = ee_yx(yx_Data2),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_ee with sin(k*pi*X)*cos(l*pi*Y) for id=2',        &
    answer = yx_Data2,                                            &
    check = yx_ee(ee_yx(yx_Data2)),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data2 = cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data2 = 0.0 ; ee_Data2(-l,-k) = 0.25  ; ee_Data2(l,-k) = -0.25  

  call AssertEqual(&
    message='ee_yx with cos(k*pi*X)*sin(l*pi*Y) for id=2',        &
    answer = ee_Data2,                                            &
    check = ee_yx(yx_Data2),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_ee with cos(k*pi*X)*sin(l*pi*Y) for id=2',        &
    answer = yx_Data2,                                            &
    check = yx_ee(ee_yx(yx_Data2)),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---------- id1 and id3 ----------
  write(*,*) 'for id1 and id3'
  call ee_ChangeResolutionDomain(id1)
  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  call ee_ChangeResolutionDomain(id3)
  yx_Data2 = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data3 = ee_yx(yx_Data2)

  call AssertEqual(&
    message='ee_yx with sin(k*pi*X)*sin(l*pi*Y) for id=1 and 3',  &
    answer = ee_Data3,                                            &
    check = ee_Data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call ee_ChangeResolutionDomain(id1)
  yx_Data = cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  call ee_ChangeResolutionDomain(id3)
  yx_Data2 = cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data3 = ee_yx(yx_Data2)

  call AssertEqual(&
    message='ee_yx with cos(k*pi*X)*cos(l*pi*Y) for id=1 and 3',  &
    answer = ee_Data3,                                            &
    check = ee_Data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call ee_ChangeResolutionDomain(id1)
  yx_Data = sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  call ee_ChangeResolutionDomain(id3)
  yx_Data2 = sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data3 = ee_yx(yx_Data2)

  call AssertEqual(&
    message='ee_yx with sin(k*pi*X)*cos(l*pi*Y) for id=1 and 3',  &
    answer = ee_Data3,                                            &
    check = ee_Data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call ee_ChangeResolutionDomain(id1)
  yx_Data = cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  call ee_ChangeResolutionDomain(id3)
  yx_Data2 = cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data3 = ee_yx(yx_Data2)

  call AssertEqual(&
    message='ee_yx with cos(k*pi*X)*sin(l*pi*Y) for id=1 and 3',  &
    answer = ee_Data3,                                            &
    check = ee_Data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ee_module_fftj_base_test2', &
       'ee_module_fftj basic transform function tests (multiple domain/resolution) succeeded!')

end program ee_module_fftj_base_test2
