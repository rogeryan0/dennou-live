!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  tee_module テストプログラム (積分・平均計算)
!
!履歴  2009/12/15  竹広真一
!
program tee_module_integral_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use tee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32, km=32       ! 格子点の設定(X,Y)
  integer, parameter :: lm=10, mm=10, nm=21       ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: zyx_Data(0:km,0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: yx_Data(0:jm-1,0:im-1)          ! 格子データ
  real(8)            :: zx_Data(0:km,0:im-1)            ! 格子データ
  real(8)            :: zy_Data(0:km,0:jm-1)            ! 格子データ
  real(8)            :: x_Data(0:im-1)                  ! 格子データ
  real(8)            :: y_Data(0:jm-1)                  ! 格子データ
  real(8)            :: z_Data(0:km)                    ! 格子データ

  real(8)            :: yx_Ans(0:jm-1,0:im-1)           ! 格子データ
  real(8)            :: zy_Ans(0:km,0:jm-1)             ! 格子データ
  real(8)            :: zx_Ans(0:km,0:im-1)             ! 格子データ

  real(8)            :: x_Ans(0:im-1)                   ! 格子データ
  real(8)            :: y_Ans(0:jm-1)                   ! 格子データ
  real(8)            :: z_Ans(0:km)                     ! 格子データ

  real(8)            :: Ans                             ! 

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0
  real(8), parameter :: zmin = -1.0d0, zmax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','tee_module_integral_test', &
       'tee_module integral and average functions tests')

 !---------------- 座標値の設定 ---------------------
  call tee_initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax) 

 !------------------- 3 次元関数チェック ----------------------
  zyx_Data = sin(pi*zyx_X) * cos(pi*zyx_Y) * cos(pi*zyx_Z)

  Ans = 0.0D0
  call AssertEqual(&
    message='IntZYX_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',      &
    answer = Ans,                                                 &
    check = IntZYX_zyx(zyx_Data),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='AvrZYX_yx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',       &
    answer = Ans,                                                 &
    check = AvrZYX_zyx(zyx_Data),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Ans = 0.0D0
  call AssertEqual(&
    message='yx_IntZ_zyx with sin(pi*X)p*cos(pi*Y)*cos(pi*Z)',    &
    answer = yx_Ans,                                              &
    check = yx_IntZ_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='yx_AvrZ_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = yx_Ans,                                              &
    check = yx_AvrZ_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zy_Ans = 0.0D0
  call AssertEqual(&
    message='zy_IntX_zyx with sin(pi*X)p*cos(pi*Y)*cos(pi*Z)',    &
    answer = zy_Ans,                                              &
    check = zy_IntX_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='zy_AvrX_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = zy_Ans,                                              &
    check = zy_AvrX_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zx_Ans = 0.0D0
  call AssertEqual(&
    message='zx_IntY_zyx with sin(pi*X)p*cos(pi*Y)*cos(pi*Z)',    &
    answer = zx_Ans,                                              &
    check = zx_IntY_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='zx_AvrY_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = zx_Ans,                                              &
    check = zx_AvrY_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  z_Ans = 0.0D0
  call AssertEqual(&
    message='z_IntYX_zyx with sin(pi*X)p*cos(pi*Y)*cos(pi*Z)',    &
    answer = z_Ans,                                               &
    check = z_IntYX_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='z_AvrYX_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = z_Ans,                                               &
    check = z_AvrYX_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Ans = 0.0D0
  call AssertEqual(&
    message='y_IntZX_zyx with sin(pi*X)p*cos(pi*Y)*cos(pi*Z)',    &
    answer = y_Ans,                                               &
    check = y_IntZX_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='y_AvrZX_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = y_Ans,                                               &
    check = y_AvrZX_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Ans = 0.0D0
  call AssertEqual(&
    message='x_IntZY_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = x_Ans,                                               &
    check = x_IntZY_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='x_AvrZY_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = x_Ans,                                               &
    check = x_AvrZY_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = (1 - sin(pi*zyx_X)) * cos(pi*zyx_Y)**2 * sin(pi*zyx_Z)**2

  Ans = 2.0D0
  call AssertEqual(&
    message='IntZYX_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',      &
    answer = Ans,                                                 &
    check = IntZYX_zyx(zyx_Data),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 0.25D0
  call AssertEqual(&
    message='AvrZYX_yx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',       &
    answer = Ans,                                                 &
    check = AvrZYX_zyx(zyx_Data),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Ans = (1 - sin(pi*yx_X)) * cos(pi*yx_Y)**2 
  call AssertEqual(&
    message='yx_IntZ_zyx with sin(pi*X)p*cos(pi*Y)*cos(pi*Z)',    &
    answer = yx_Ans,                                              &
    check = yx_IntZ_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  yx_Ans = (1 - sin(pi*yx_X)) * cos(pi*yx_Y)**2/2.0D0
  call AssertEqual(&
    message='yx_AvrZ_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = yx_Ans,                                              &
    check = yx_AvrZ_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zy_Ans = 2*cos(pi*zy_Y)**2 * sin(pi*zy_Z)**2
  call AssertEqual(&
    message='zy_IntX_zyx with sin(pi*X)p*cos(pi*Y)*cos(pi*Z)',    &
    answer = zy_Ans,                                              &
    check = zy_IntX_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zy_Ans = cos(pi*zy_Y)**2 * sin(pi*zy_Z)**2
  call AssertEqual(&
    message='zy_AvrX_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = zy_Ans,                                              &
    check = zy_AvrX_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zx_Ans = (1 - sin(pi*zx_X)) *  sin(pi*zx_Z)**2
  call AssertEqual(&
    message='zx_IntY_zyx with sin(pi*X)p*cos(pi*Y)*cos(pi*Z)',    &
    answer = zx_Ans,                                              &
    check = zx_IntY_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zx_Ans = (1 - sin(pi*zx_X)) *  sin(pi*zx_Z)**2/2.0D0
  call AssertEqual(&
    message='zx_AvrY_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = zx_Ans,                                              &
    check = zx_AvrY_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  z_Ans = 2*sin(pi*z_Z)**2
  call AssertEqual(&
    message='z_IntYX_zyx with sin(pi*X)p*cos(pi*Y)*cos(pi*Z)',    &
    answer = z_Ans,                                               &
    check = z_IntYX_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  z_Ans = sin(pi*z_Z)**2/2.0D0
  call AssertEqual(&
    message='z_AvrYX_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = z_Ans,                                               &
    check = z_AvrYX_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Ans = 2*cos(pi*y_Y)**2
  call AssertEqual(&
    message='y_IntZX_zyx with sin(pi*X)p*cos(pi*Y)*cos(pi*Z)',    &
    answer = y_Ans,                                               &
    check = y_IntZX_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  y_Ans = cos(pi*y_Y)**2/2.0D0
  call AssertEqual(&
    message='y_AvrZX_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = y_Ans,                                               &
    check = y_AvrZX_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Ans = 1 - sin(pi*x_X)
  call AssertEqual(&
    message='x_IntZY_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = x_Ans,                                               &
    check = x_IntZY_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  x_Ans = (1 - sin(pi*x_X))/4.0D0
  call AssertEqual(&
    message='x_AvrZY_zyx with sin(pi*X)*cos(pi*Y)*cos(pi*Z)',     &
    answer = x_Ans,                                               &
    check = x_AvrZY_zyx(zyx_Data),                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !------------------- 2 次元関数チェック ----------------------
  ! YX データ

  yx_Data = sin(pi*yx_X) * cos(pi*yx_Y)

  Ans = 0.0D0
  call AssertEqual(&
    message='IntYX_yx with sin(pi*X)*cos(pi*Y)',                  &
    answer = Ans,                                                 &
    check = IntYX_yx(yx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='AvrYX_yx with sin(pi*X)*cos(pi*Y)',                  &
    answer = Ans,                                                 &
    check = AvrYX_yx(yx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Ans = 0.0D0
  call AssertEqual(&
    message='y_IntX_yx with sin(pi*X)*cos(pi*Y)',                 &
    answer = y_Ans,                                               &
    check = y_IntX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='y_AvrX_yx with sin(pi*X)*cos(pi*Y)',                 &
    answer = y_Ans,                                               &
    check = y_AvrX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Ans = 0.0D0
  call AssertEqual(&
    message='x_IntY_yx with sin(pi*X)*cos(pi*Y)',                 &
    answer = x_Ans,                                               &
    check = x_IntY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='x_AvrY_yx with sin(pi*X)*cos(pi*Y)',                 &
    answer = x_Ans,                                               &
    check = x_AvrY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = (1 - sin(pi*yx_X)) * cos(pi*yx_Y)**2

  Ans = 2.0D0
  call AssertEqual(&
    message='IntYX_yx with (1-sin(pi*X)) * cos(pi*Y)**2',         &
    answer = Ans,                                                 &
    check = IntYX_yx(yx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 0.5D0
  call AssertEqual(&
    message='AvrYX_yx with (1-sin(pi*X)) * cos(pi*Y)**2',         &
    answer = Ans,                                                 &
    check = AvrYX_yx(yx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Ans = 2*cos(pi*y_Y)**2
  call AssertEqual(&
    message='y_IntX_yx with (1-sin(pi*X)) * cos(pi*Y)**2',        &
    answer = y_Ans,                                               &
    check = y_IntX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  y_Ans = cos(pi*y_Y)**2
  call AssertEqual(&
    message='y_AvrX_yx with (1-sin(pi*X)) * cos(pi*Y)**2',        &
    answer = y_Ans,                                               &
    check = y_AvrX_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Ans = 1 - sin(pi*x_X)
  call AssertEqual(&
    message='x_IntY_yx with (1-sin(pi*X)) * cos(pi*Y)**2',        &
    answer = x_Ans,                                               &
    check = x_IntY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  x_Ans = 0.5*(1 - sin(pi*x_X))
  call AssertEqual(&
    message='x_AvrY_yx with (1-sin(pi*X)) * cos(pi*Y)**2',        &
    answer = x_Ans,                                               &
    check = x_AvrY_yx(yx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  ! ZY データ
  zy_Data = sin(pi*zy_Z) * cos(pi*zy_Y)

  Ans = 0.0D0
  call AssertEqual(&
    message='IntZY_zy with sin(pi*Z)*cos(pi*Y)',                  &
    answer = Ans,                                                 &
    check = IntZY_zy(zy_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='AvrZY_zy with sin(pi*Z)*cos(pi*Y)',                  &
    answer = Ans,                                                 &
    check = AvrZY_zy(zy_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Ans = 0.0D0
  call AssertEqual(&
    message='y_IntX_zy with sin(pi*Z)*cos(pi*Y)',                 &
    answer = y_Ans,                                               &
    check = y_IntZ_zy(zy_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='y_AvrZ_zy with sin(pi*Z)*cos(pi*Y)',                 &
    answer = y_Ans,                                               &
    check = y_AvrZ_zy(zy_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  z_Ans = 0.0D0
  call AssertEqual(&
    message='z_IntY_zy with sin(pi*Z)*cos(pi*Y)',                 &
    answer = z_Ans,                                               &
    check = z_IntY_zy(zy_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='z_AvrY_zy with sin(pi*Z)*cos(pi*Y)',                 &
    answer = z_Ans,                                               &
    check = z_AvrY_zy(zy_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zy_Data = (1 - sin(pi*zy_Z)) * cos(pi*zy_Y)**2

  Ans = 2.0D0
  call AssertEqual(&
    message='IntZY_zy with (1-sin(pi*Z)) * cos(pi*Y)**2',         &
    answer = Ans,                                                 &
    check = IntZY_zy(zy_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 0.5D0
  call AssertEqual(&
    message='AvrZY_zy with (1-sin(pi*Z)) * cos(pi*Y)**2',         &
    answer = Ans,                                                 &
    check = AvrZY_zy(zy_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Ans = 2*cos(pi*y_Y)**2
  call AssertEqual(&
    message='y_IntZ_zy with (1-sin(pi*Z)) * cos(pi*Y)**2',        &
    answer = y_Ans,                                               &
    check = y_IntZ_zy(zy_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  y_Ans = cos(pi*y_Y)**2
  call AssertEqual(&
    message='y_AvrZ_zy with (1-sin(pi*Z)) * cos(pi*Y)**2',        &
    answer = y_Ans,                                               &
    check = y_AvrZ_zy(zy_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  z_Ans = 1 - sin(pi*z_Z)
  call AssertEqual(&
    message='z_IntY_zy with (1-sin(pi*Z)) * cos(pi*Y)**2',        &
    answer = z_Ans,                                               &
    check = z_IntY_zy(zy_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  z_Ans = 0.5*(1 - sin(pi*z_Z))
  call AssertEqual(&
    message='z_AvrY_zy with (1-sin(pi*Z)) * cos(pi*Y)**2',        &
    answer = z_Ans,                                               &
    check = z_AvrY_zy(zy_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  ! ZX データ
  zx_Data = sin(pi*zx_Z) * cos(pi*zx_X)

  Ans = 0.0D0
  call AssertEqual(&
    message='IntZX_zx with sin(pi*Z)*cos(pi*X)',                  &
    answer = Ans,                                                 &
    check = IntZX_zx(zx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='AvrZX_zx with sin(pi*Z)*cos(pi*X)',                  &
    answer = Ans,                                                 &
    check = AvrZX_zx(zx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Ans = 0.0D0
  call AssertEqual(&
    message='x_IntZ_zx with sin(pi*Z)*cos(pi*X)',                 &
    answer = x_Ans,                                               &
    check = x_IntZ_zx(zx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='x_AvrZ_zx with sin(pi*Z)*cos(pi*X)',                 &
    answer = x_Ans,                                               &
    check = x_AvrZ_zx(zx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  z_Ans = 0.0D0
  call AssertEqual(&
    message='z_IntX_zx with sin(pi*Z)*cos(pi*X)',                 &
    answer = z_Ans,                                               &
    check = z_IntX_zx(zx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='z_AvrX_zx with sin(pi*Z)*cos(pi*X)',                 &
    answer = z_Ans,                                               &
    check = z_AvrX_zx(zx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zx_Data = (1 - sin(pi*zx_Z)) * cos(pi*zx_X)**2

  Ans = 2.0D0
  call AssertEqual(&
    message='IntZX_zx with (1-sin(pi*Z)) * cos(pi*X)**2',         &
    answer = Ans,                                                 &
    check = IntZX_zx(zx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 0.5D0
  call AssertEqual(&
    message='AvrZX_zx with (1-sin(pi*Z)) * cos(pi*X)**2',         &
    answer = Ans,                                                 &
    check = AvrZX_zx(zx_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  x_Ans = 2*cos(pi*x_X)**2
  call AssertEqual(&
    message='x_IntZ_zx with (1-sin(pi*Z)) * cos(pi*X)**2',        &
    answer = x_Ans,                                               &
    check = x_IntZ_zx(zx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  x_Ans = cos(pi*x_X)**2
  call AssertEqual(&
    message='x_AvrZ_zx with (1-sin(pi*Z)) * cos(pi*X)**2',        &
    answer = x_Ans,                                               &
    check = x_AvrZ_zx(zx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  z_Ans = 1 - sin(pi*z_Z)
  call AssertEqual(&
    message='z_IntX_zx with (1-sin(pi*Z)) * cos(pi*X)**2',        &
    answer = z_Ans,                                               &
    check = z_IntX_zx(zx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  z_Ans = 0.5*(1 - sin(pi*z_Z))
  call AssertEqual(&
    message='z_AvrX_zx with (1-sin(pi*Z)) * cos(pi*X)**2',        &
    answer = z_Ans,                                               &
    check = z_AvrX_zx(zx_Data),                                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !------------------- 1 次元関数チェック ----------------------
  x_Data = 1 - sin(2*pi*x_X)

  Ans = 2.0D0
  call AssertEqual(&
    message='IntX_x with 1-sin(2*pi*X)',                          &
    answer = Ans,                                                 &
    check = IntX_x(x_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 1.0D0
  call AssertEqual(&
    message='AvrX_x with 1-sin(2*pi*X)',                          &
    answer = Ans,                                                 &
    check = AvrX_x(x_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Data = cos(pi*y_Y)**2

  Ans = 1.0D0
  call AssertEqual(&
    message='IntY_y with cos(pi*X)**2',                           &
    answer = Ans,                                                 &
    check = IntY_y(y_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 0.5D0
  call AssertEqual(&
    message='AvrY_y with cos(pi*X)**2',                           &
    answer = Ans,                                                 &
    check = AvrY_y(y_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  z_Data = sin(pi*z_Z)**2

  Ans = 1.0D0
  call AssertEqual(&
    message='IntZ_z with sin(pi*Z)**2',                           &
    answer = Ans,                                                 &
    check = IntZ_z(z_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !------------------- 1 次元関数チェック ----------------------
  x_Data = 1 - sin(2*pi*x_X)

  Ans = 2.0D0
  call AssertEqual(&
    message='IntX_x with 1-sin(2*pi*X)',                          &
    answer = Ans,                                                 &
    check = IntX_x(x_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 1.0D0
  call AssertEqual(&
    message='AvrX_x with 1-sin(2*pi*X)',                          &
    answer = Ans,                                                 &
    check = AvrX_x(x_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_Data = cos(pi*y_Y)**2

  Ans = 1.0D0
  call AssertEqual(&
    message='IntY_y with cos(pi*X)**2',                           &
    answer = Ans,                                                 &
    check = IntY_y(y_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 0.5D0
  call AssertEqual(&
    message='AvrY_y with cos(pi*X)**2',                           &
    answer = Ans,                                                 &
    check = AvrY_y(y_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  z_Data = sin(pi*z_Z)**2

  Ans = 1.0D0
  call AssertEqual(&
    message='IntZ_z with sin(pi*Z)**2',                           &
    answer = Ans,                                                 &
    check = IntZ_z(z_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  Ans = 0.5D0
  call AssertEqual(&
    message='AvrZ_z with sin(pi*Z)**2',                           &
    answer = Ans,                                                 &
    check = AvrZ_z(z_Data),                                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','tee_module_integral_test', &
       'tee_module integral and average functions tests succeeded')

end program tee_module_integral_test
