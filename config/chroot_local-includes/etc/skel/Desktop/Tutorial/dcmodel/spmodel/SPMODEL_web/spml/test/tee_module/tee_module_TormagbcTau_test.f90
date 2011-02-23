!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  tee_module テストプログラム (境界値問題, トロイダル磁場, Tau 法)
!
!履歴  2009/12/19  竹広真一
!
program tee_module_tormagbctau_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use tee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32, km=16       ! 格子点の設定(X,Y)
  integer, parameter :: lm=10, mm=10, nm=10       ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: zyx_Tormag(0:km,0:jm-1,0:im-1)      ! 格子データ

  real(8)            :: yx_BoundaryTop(0:jm-1,0:im-1)     ! 格子データ
  real(8)            :: yx_BoundaryBtm(0:jm-1,0:im-1)     ! 格子データ

  real(8)            :: tee_Tormag(0:nm,-mm:mm,-lm:lm)      ! スペクトルデータ
  real(8)            :: tee_Tormag_orig(0:nm,-mm:mm,-lm:lm) ! スペクトルデータ

  integer            :: l=2, m=5

 !---- 座標変数など ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=2.0d0
  real(8), parameter :: zmin = -1.0d0, zmax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','tee_module_TormagbcTau_test', &
       'tee_module toroidal mag. field B.C. subruoutine tests')

 !---------------- 座標値の設定 ---------------------
  ! スペクトル初期化
  call tee_initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax) 

 !-------------------BC-DD(Tau) ----------------------
  zyx_Tormag = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  yx_BoundaryTop = 0.0D0
  yx_BoundaryBtm = 0.0D0

  tee_Tormag = tee_zyx(zyx_Tormag)
  tee_Tormag_orig = tee_Tormag

  call tee_TormagBoundariesTau(tee_Tormag)
  zyx_Tormag = zyx_tee(tee_Tormag)

  call AssertEqual(&
    message='tee_TormagBoundariesTau [internal]',&
    answer = tee_Tormag_orig(0:nm-2,:,:),                         &
    check = tee_Tormag(0:nm-2,:,:),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_TormagBoundariesTau [Top]',&
    answer = zyx_Tormag(0,:,:),                                   &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_TormagBoundariesTau [Bottom]', &
    answer = zyx_Tormag(km,:,:),                                  &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','tee_module_TormagbcTau_test', &
       'tee_module toroidal mag. field B.C. subruoutine tests succeeded!')

end program tee_module_tormagbctau_test
