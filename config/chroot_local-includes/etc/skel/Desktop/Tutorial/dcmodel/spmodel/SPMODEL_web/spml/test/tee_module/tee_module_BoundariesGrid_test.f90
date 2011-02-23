!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  tee_module テストプログラム (境界値問題,Grid)
!
!履歴  2009/12/16  竹広真一
!
program tee_module_boundariesgrid_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use tee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32, km=16       ! 格子点の設定(X,Y)
  integer, parameter :: lm=10, mm=10, nm=16       ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: zyx_Data(0:km,0:jm-1,0:im-1)      ! 格子データ
  real(8)            :: zyx_Data_orig(0:km,0:jm-1,0:im-1) ! 格子データ

  real(8)            :: yx_BoundaryTop(0:jm-1,0:im-1)     ! 格子データ
  real(8)            :: yx_BoundaryBtm(0:jm-1,0:im-1)     ! 格子データ

  real(8)            :: tee_Data(0:nm,-mm:mm,-lm:lm)      ! スペクトルデータ

  real(8)            :: aee_Boundaries(2,-mm:mm,-lm:lm)   ! スペクトルデータ

  integer            :: l=2, m=5

 !---- 座標変数など ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=2.0d0
  real(8), parameter :: zmin = -1.0d0, zmax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','tee_module_BoundariesGrid_test', &
       'tee_module boundary condition subruoutine tests')

 !---------------- 座標値の設定 ---------------------
  ! スペクトル初期化
  call tee_initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax) 

 !-------------------BC-DD(Grid) ----------------------
  zyx_Data = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  yx_BoundaryTop = 0.0D0
  yx_BoundaryBtm = 0.0D0

  zyx_Data_orig = zyx_Data
  tee_Data = tee_zyx(zyx_Data)

  call tee_BoundariesGrid(tee_Data)
  zyx_Data = zyx_tee(tee_Data)

  call AssertEqual(&
    message='tee_BoundariesGrid [DD,internal]',                   &
    answer = zyx_Data_orig(1:km-1,:,:),                           &
    check = zyx_Data(1:km-1,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesGrid [DD,Top]',                        &
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesGrid [DD,Bottom]',                     &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  yx_BoundaryTop = sin(m*pi*yx_X) * cos(l*pi*yx_Y)
  yx_BoundaryBtm = cos(m*pi*yx_X) * sin(l*pi*yx_Y)
  aee_Boundaries(1,:,:)=ee_yx(yx_Boundarytop)
  aee_Boundaries(2,:,:)=ee_yx(yx_BoundaryBtm)

  zyx_Data_orig = zyx_Data
  tee_Data = tee_zyx(zyx_Data)

  call tee_BoundariesGrid(tee_Data,values=aee_Boundaries)
  zyx_Data = zyx_tee(tee_Data)

  call AssertEqual(&
    message='tee_BoundariesGrid [DD,vaules,internal]',            &
    answer = zyx_Data_orig(1:km-1,:,:),                           &
    check = zyx_Data(1:km-1,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesGrid [DD,values,Top]',                 &
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesGrid [DD,values,Bottom]',              &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-NN(Grid) ----------------------
  zyx_Data = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  yx_BoundaryTop = 0.0D0
  yx_BoundaryBtm = 0.0D0

  zyx_Data_orig = zyx_Data
  tee_Data = tee_zyx(zyx_Data)

  call tee_BoundariesGrid(tee_Data,cond='NN')

  call AssertEqual(&
    message='tee_BoundariesGrid [NN,internal]',                   &
    answer = zyx_Data_orig(1:km-1,:,:),                           &
    check = zyx_Data(1:km-1,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Dz_tee(tee_Data))
  call AssertEqual(&
    message='tee_BoundariesGrid [NN,Top]',                        &
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesGrid [NN,Bottom]',                     &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  yx_BoundaryTop = sin(m*pi*yx_X) * cos(l*pi*yx_Y)
  yx_BoundaryBtm = cos(m*pi*yx_X) * sin(l*pi*yx_Y)
  aee_Boundaries(1,:,:)=ee_yx(yx_Boundarytop)
  aee_Boundaries(2,:,:)=ee_yx(yx_BoundaryBtm)

  zyx_Data_orig = zyx_Data
  tee_Data = tee_zyx(zyx_Data)

  call tee_BoundariesGrid(tee_Data,cond='NN',values=aee_Boundaries)

  call AssertEqual(&
    message='tee_BoundariesGrid [NN,vaules,internal]',            &
    answer = zyx_Data_orig(1:km-1,:,:),                           &
    check = zyx_Data(1:km-1,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Dz_tee(tee_Data))
  call AssertEqual(&
    message='tee_BoundariesGrid [NN,values,Top]',                 &
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesGrid [NN,values,Bottom]',              &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-DN(Grid) ----------------------
  zyx_Data = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  yx_BoundaryTop = 0.0D0
  yx_BoundaryBtm = 0.0D0

  zyx_Data_orig = zyx_Data
  tee_Data = tee_zyx(zyx_Data)

  call tee_BoundariesGrid(tee_Data,cond='DN')

  call AssertEqual(&
    message='tee_BoundariesGrid [DN,internal]',                   &
    answer = zyx_Data_orig(1:km-1,:,:),                           &
    check = zyx_Data(1:km-1,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Data)
  call AssertEqual(&
    message='tee_BoundariesGrid [DN,Top]',                        &
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Dz_tee(tee_Data))
  call AssertEqual(&
    message='tee_BoundariesGrid [DN,Bottom]',                     &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  yx_BoundaryTop = sin(m*pi*yx_X) * cos(l*pi*yx_Y)
  yx_BoundaryBtm = cos(m*pi*yx_X) * sin(l*pi*yx_Y)
  aee_Boundaries(1,:,:)=ee_yx(yx_Boundarytop)
  aee_Boundaries(2,:,:)=ee_yx(yx_BoundaryBtm)

  zyx_Data_orig = zyx_Data
  tee_Data = tee_zyx(zyx_Data)

  call tee_BoundariesGrid(tee_Data,cond='DN',values=aee_Boundaries)

  call AssertEqual(&
    message='tee_BoundariesGrid [DN,vaules,internal]',            &
    answer = zyx_Data_orig(1:km-1,:,:),                           &
    check = zyx_Data(1:km-1,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Data)
  call AssertEqual(&
    message='tee_BoundariesGrid [DN,values,Top]',                 &
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Dz_tee(tee_Data))
  call AssertEqual(&
    message='tee_BoundariesGrid [DN,values,Bottom]',              &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-ND(Grid) ----------------------
  zyx_Data = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  yx_BoundaryTop = 0.0D0
  yx_BoundaryBtm = 0.0D0

  zyx_Data_orig = zyx_Data
  tee_Data = tee_zyx(zyx_Data)

  call tee_BoundariesGrid(tee_Data,cond='ND')

  call AssertEqual(&
    message='tee_BoundariesGrid [ND,internal]',                   &
    answer = zyx_Data_orig(1:km-1,:,:),                           &
    check = zyx_Data(1:km-1,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Dz_tee(tee_Data))
  call AssertEqual(&
    message='tee_BoundariesGrid [ND,Top]',                        &
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Data)
  call AssertEqual(&
    message='tee_BoundariesGrid [ND,Bottom]',                     &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  yx_BoundaryTop = sin(m*pi*yx_X) * cos(l*pi*yx_Y)
  yx_BoundaryBtm = cos(m*pi*yx_X) * sin(l*pi*yx_Y)
  aee_Boundaries(1,:,:)=ee_yx(yx_Boundarytop)
  aee_Boundaries(2,:,:)=ee_yx(yx_BoundaryBtm)

  zyx_Data_orig = zyx_Data
  tee_Data = tee_zyx(zyx_Data)

  call tee_BoundariesGrid(tee_Data,cond='ND',values=aee_Boundaries)

  call AssertEqual(&
    message='tee_BoundariesGrid [ND,vaules,internal]',            &
    answer = zyx_Data_orig(1:km-1,:,:),                           &
    check = zyx_Data(1:km-1,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Dz_tee(tee_Data))
  call AssertEqual(&
    message='tee_BoundariesGrid [ND,values,Top]',                 &
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Data)
  call AssertEqual(&
    message='tee_BoundariesGrid [ND,values,Bottom]',              &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','tee_module_BoundariesGrid_test', &
       'tee_module boundary condition subroutine tests succeeded!')

end program tee_module_boundariesgrid_test
