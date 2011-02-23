!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  tee_module テストプログラム (境界値問題, ▽^2φ から φを求める)
!
!履歴  2009/12/19  竹広真一
!
program tee_module_polvelbcCheb_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use tee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32, km=16       ! 格子点の設定(X,Y)
  integer, parameter :: lm=10, mm=10, nm=16       ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: zyx_Polvel(0:km,0:jm-1,0:im-1)      ! 格子データ
  real(8)            :: zyx_LaplaPolvel(0:km,0:jm-1,0:im-1) ! 格子データ
  real(8)            :: zyx_LaplaPolvel_orig(0:km,0:jm-1,0:im-1) ! 格子データ
  real(8)            :: zyx_DPolvel(0:km,0:jm-1,0:im-1)     ! 格子データ
  real(8)            :: tee_Polvel(0:nm,-mm:mm,-lm:lm)      ! スペクトルデータ

  real(8)            :: zyx_Null(0:km,0:jm-1,0:im-1) =0.0D0 ! 格子データ

  integer            :: l=2, m=5

 !---- 座標変数など ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=2.0d0
  real(8), parameter :: zmin = -1.0d0, zmax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','tee_module_PolvelbcGrid_test', &
       'tee_module poloidal velocity potential B.C. function tests')

 !---------------- 座標値の設定 ---------------------
  ! スペクトル初期化
  call tee_initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax) 

 !-------------------BC-FF(Grid) ----------------------
  zyx_LaplaPolvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  zyx_LaplaPolvel_orig = zyx_LaplaPolvel

  tee_Polvel = tee_LaplaPol2PolGrid_tee(tee_zyx(zyx_LaplaPolvel),&
       cond='FF',new=.true.)

  zyx_Polvel = zyx_tee(tee_Polvel)
  zyx_LaplaPolvel = zyx_tee(tee_Lapla_tee(tee_Polvel))

  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [FF,internal]',             &
    answer = zyx_LaplaPolvel_orig(2:km-2,:,:),                    &
    check = zyx_LaplaPolvel(2:km-2,:,:),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [FF,Top]',                  &
    answer = zyx_Polvel(0,:,:),                                   &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [FF,Bottom]',               &
    answer = zyx_Polvel(km,:,:),                                  &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DPolvel = zyx_tee(tee_Dz_tee(tee_Dz_tee(tee_Polvel)))

  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [FF,Top,deriv]',            &
    answer = zyx_DPolvel(0,:,:),                                  &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [FF,Bottom,deriv]',         &
    answer = zyx_DPolvel(km,:,:),                                 &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-RR(Grid) ----------------------
  zyx_LaplaPolvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  zyx_LaplaPolvel_orig = zyx_LaplaPolvel

  tee_Polvel = tee_LaplaPol2PolGrid_tee(tee_zyx(zyx_LaplaPolvel),&
       cond='RR',new=.true.)

  zyx_Polvel = zyx_tee(tee_Polvel)
  zyx_LaplaPolvel = zyx_tee(tee_Lapla_tee(tee_Polvel))

  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [RR,internal]',             &
    answer = zyx_LaplaPolvel_orig(2:km-2,:,:),                    &
    check = zyx_LaplaPolvel(2:km-2,:,:),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [RR,Top]',                  &
    answer = zyx_Polvel(0,:,:),                                   &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [RR,Bottom]',               &
    answer = zyx_Polvel(km,:,:),                                  &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DPolvel = zyx_tee(tee_Dz_tee(tee_Polvel))

  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [RR,Top,deriv]',            &
    answer = zyx_DPolvel(0,:,:),                                  &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [RR,Bottom,deriv]',         &
    answer = zyx_DPolvel(km,:,:),                                 &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-RF(Grid) ----------------------
  zyx_LaplaPolvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  zyx_LaplaPolvel_orig = zyx_LaplaPolvel

  tee_Polvel = tee_LaplaPol2PolGrid_tee(tee_zyx(zyx_LaplaPolvel),&
       cond='RF',new=.true.)

  zyx_Polvel = zyx_tee(tee_Polvel)
  zyx_LaplaPolvel = zyx_tee(tee_Lapla_tee(tee_Polvel))

  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [RF,internal]',             &
    answer = zyx_LaplaPolvel_orig(2:km-2,:,:),                    &
    check = zyx_LaplaPolvel(2:km-2,:,:),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [RF,Top]',                  &
    answer = zyx_Polvel(0,:,:),                                   &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [RF,Bottom]',               &
    answer = zyx_Polvel(km,:,:),                                  &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DPolvel = zyx_tee(tee_Dz_tee(tee_Polvel))

  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [RF,Top,deriv]',            &
    answer = zyx_DPolvel(0,:,:),                                  &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DPolvel = zyx_tee(tee_Dz_tee(tee_Dz_tee(tee_Polvel)))

  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [RF,Bottom,deriv]',         &
    answer = zyx_DPolvel(km,:,:),                                 &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-FR(Grid) ----------------------
  zyx_LaplaPolvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  zyx_LaplaPolvel_orig = zyx_LaplaPolvel

  tee_Polvel = tee_LaplaPol2PolGrid_tee(tee_zyx(zyx_LaplaPolvel),&
       cond='FR',new=.true.)

  zyx_Polvel = zyx_tee(tee_Polvel)
  zyx_LaplaPolvel = zyx_tee(tee_Lapla_tee(tee_Polvel))

  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [FR,internal]',             &
    answer = zyx_LaplaPolvel_orig(2:km-2,:,:),                    &
    check = zyx_LaplaPolvel(2:km-2,:,:),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [FR,Top]',                  &
    answer = zyx_Polvel(0,:,:),                                   &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [FR,Bottom]',               &
    answer = zyx_Polvel(km,:,:),                                  &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DPolvel = zyx_tee(tee_Dz_tee(tee_Dz_tee(tee_Polvel)))

  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [FR,Top,deriv]',            &
    answer = zyx_DPolvel(0,:,:),                                  &
    check = zyx_Null(0,:,:),                                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_DPolvel = zyx_tee(tee_Dz_tee(tee_Polvel))

  call AssertEqual(&
    message='tee_LaplaPol2PolGrid_tee [RF,Bottom,deriv]',         &
    answer = zyx_DPolvel(km,:,:),                                 &
    check = zyx_Null(km,:,:),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','tee_module_PolvelbcGrid_test', &
       'tee_module poloidal velocity potential B.C. function tests succeeded!')

end program tee_module_polvelbcCheb_test
