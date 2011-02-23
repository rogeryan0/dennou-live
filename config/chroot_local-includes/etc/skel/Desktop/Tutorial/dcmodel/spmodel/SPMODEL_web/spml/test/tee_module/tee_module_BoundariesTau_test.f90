!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  tee_module テストプログラム (境界値問題,Tau)
!
!履歴  2009/12/16  竹広真一
!
program tee_module_boundariestau_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use tee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32, km=16       ! 格子点の設定(X,Y)
  integer, parameter :: lm=10, mm=10, nm=10       ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: zyx_Data(0:km,0:jm-1,0:im-1)      ! 格子データ

  real(8)            :: yx_BoundaryTop(0:jm-1,0:im-1)     ! 格子データ
  real(8)            :: yx_BoundaryBtm(0:jm-1,0:im-1)     ! 格子データ

  real(8)            :: tee_Data(0:nm,-mm:mm,-lm:lm)      ! スペクトルデータ
  real(8)            :: tee_Data_orig(0:nm,-mm:mm,-lm:lm) ! スペクトルデータ

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

  call MessageNotify('M','tee_module_BoundariesTau_test', &
       'tee_module boundary condition subruoutine tests')

 !---------------- 座標値の設定 ---------------------
  ! スペクトル初期化
  call tee_initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax) 

 !-------------------BC-DD(Tau) ----------------------
  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  yx_BoundaryTop = 0.0D0
  yx_BoundaryBtm = 0.0D0

  tee_Data = tee_zyx(zyx_Data)
  tee_Data_orig = tee_Data

  call tee_BoundariesTau(tee_Data)
  zyx_Data = zyx_tee(tee_Data)

  call AssertEqual(&
    message='tee_BoundariesTau [DD,internal]',&
    answer = tee_Data_orig(0:nm-2,:,:),                           &
    check = tee_Data(0:nm-2,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesTau [DD,Top]',&
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesTau [DD,Bottom]', &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  yx_BoundaryTop = sin(m*pi*yx_X) * cos(l*pi*yx_Y)
  yx_BoundaryBtm = cos(m*pi*yx_X) * sin(l*pi*yx_Y)
  aee_Boundaries(1,:,:)=ee_yx(yx_Boundarytop)
  aee_Boundaries(2,:,:)=ee_yx(yx_BoundaryBtm)

  tee_Data = tee_zyx(zyx_Data)
  tee_Data_orig = tee_Data

  call tee_BoundariesTau(tee_Data,values=aee_Boundaries)
  zyx_Data = zyx_tee(tee_Data)

  call AssertEqual(&
    message='tee_BoundariesTau [DD,vaules,internal]',&
    answer = tee_Data_orig(0:nm-2,:,:),                           &
    check = tee_Data(0:nm-2,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesTau [DD,values,Top]',&
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesTau [DD,values,Bottom]', &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-NN(Tau) ----------------------
  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  yx_BoundaryTop = 0.0D0
  yx_BoundaryBtm = 0.0D0

  tee_Data = tee_zyx(zyx_Data)
  tee_Data_orig = tee_Data

  call tee_BoundariesTau(tee_Data,cond='NN')
  zyx_Data = zyx_tee(tee_Dz_tee(tee_Data))

  call AssertEqual(&
    message='tee_BoundariesTau [NN,internal]',&
    answer = tee_Data_orig(0:nm-2,:,:),                           &
    check = tee_Data(0:nm-2,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesTau [NN,Top]',&
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesTau [NN,Bottom]', &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  yx_BoundaryTop = sin(m*pi*yx_X) * cos(l*pi*yx_Y)
  yx_BoundaryBtm = cos(m*pi*yx_X) * sin(l*pi*yx_Y)
  aee_Boundaries(1,:,:)=ee_yx(yx_Boundarytop)
  aee_Boundaries(2,:,:)=ee_yx(yx_BoundaryBtm)

  tee_Data = tee_zyx(zyx_Data)
  tee_Data_orig = tee_Data

  call tee_BoundariesTau(tee_Data,values=aee_Boundaries,cond='NN')
  zyx_Data = zyx_tee(tee_Dz_tee(tee_Data))

  call AssertEqual(&
    message='tee_BoundariesTau [NN,values,internal]',&
    answer = tee_Data_orig(0:nm-2,:,:),                           &
    check = tee_Data(0:nm-2,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesTau [NN,values,Top]',&
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='tee_BoundariesTau [NN,values,Bottom]', &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-DN(Tau) ----------------------
  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  yx_BoundaryTop = 0.0D0
  yx_BoundaryBtm = 0.0D0

  tee_Data = tee_zyx(zyx_Data)
  tee_Data_orig = tee_Data

  call tee_BoundariesTau(tee_Data,cond='DN')

  call AssertEqual(&
    message='tee_BoundariesTau [DN,internal]',&
    answer = tee_Data_orig(0:nm-2,:,:),                           &
    check = tee_Data(0:nm-2,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Data)
  call AssertEqual(&
    message='tee_BoundariesTau [DN,Top]',&
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Dz_tee(tee_Data))
  call AssertEqual(&
    message='tee_BoundariesTau [DN,Bottom]', &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  yx_BoundaryTop = sin(m*pi*yx_X) * cos(l*pi*yx_Y)
  yx_BoundaryBtm = cos(m*pi*yx_X) * sin(l*pi*yx_Y)
  aee_Boundaries(1,:,:)=ee_yx(yx_Boundarytop)
  aee_Boundaries(2,:,:)=ee_yx(yx_BoundaryBtm)

  tee_Data = tee_zyx(zyx_Data)
  tee_Data_orig = tee_Data

  call tee_BoundariesTau(tee_Data,values=aee_Boundaries,cond='DN')

  call AssertEqual(&
    message='tee_BoundariesTau [DN,values,internal]',&
    answer = tee_Data_orig(0:nm-2,:,:),                           &
    check = tee_Data(0:nm-2,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Data)
  call AssertEqual(&
    message='tee_BoundariesTau [DN,values,Top]',&
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Dz_tee(tee_Data))
  call AssertEqual(&
    message='tee_BoundariesTau [DN,values,Bottom]', &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !-------------------BC-ND(Tau) ----------------------
  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  yx_BoundaryTop = 0.0D0
  yx_BoundaryBtm = 0.0D0

  tee_Data = tee_zyx(zyx_Data)
  tee_Data_orig = tee_Data

  call tee_BoundariesTau(tee_Data,cond='ND')

  call AssertEqual(&
    message='tee_BoundariesTau [ND,internal]',&
    answer = tee_Data_orig(0:nm-2,:,:),                           &
    check = tee_Data(0:nm-2,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Dz_tee(tee_Data))
  call AssertEqual(&
    message='tee_BoundariesTau [ND,Top]',&
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Data)
  call AssertEqual(&
    message='tee_BoundariesTau [ND,Bottom]', &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  yx_BoundaryTop = sin(m*pi*yx_X) * cos(l*pi*yx_Y)
  yx_BoundaryBtm = cos(m*pi*yx_X) * sin(l*pi*yx_Y)
  aee_Boundaries(1,:,:)=ee_yx(yx_Boundarytop)
  aee_Boundaries(2,:,:)=ee_yx(yx_BoundaryBtm)

  tee_Data = tee_zyx(zyx_Data)
  tee_Data_orig = tee_Data

  call tee_BoundariesTau(tee_Data,values=aee_Boundaries,cond='ND')

  call AssertEqual(&
    message='tee_BoundariesTau [ND,values,internal]',&
    answer = tee_Data_orig(0:nm-2,:,:),                           &
    check = tee_Data(0:nm-2,:,:),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Dz_tee(tee_Data))
  call AssertEqual(&
    message='tee_BoundariesTau [ND,values,Top]',&
    answer = zyx_Data(0,:,:),                                     &
    check = yx_BoundaryTop,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zyx_Data = zyx_tee(tee_Data)
  call AssertEqual(&
    message='tee_BoundariesTau [ND,values,Bottom]', &
    answer = zyx_Data(km,:,:),                                    &
    check = yx_BoundaryBtm,                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )


  call MessageNotify('M','tee_module_boundariesTau_test', &
       'tee_module boundary condition subroutine tests succeeded!')

end program tee_module_boundariestau_test
