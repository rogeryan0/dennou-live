!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  tee_module テストプログラム (微分演算)
!
!履歴  2009/12/15  竹広真一
!
program tee_module_deriv_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use tee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32, km=16       ! 格子点の設定(X,Y)
  integer, parameter :: lm=10, mm=10, nm=10       ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: zyx_Data(0:km,0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: zyx_Deriv(0:km,0:jm-1,0:im-1)   ! 格子データ

  integer            :: l=2, m=5

 !---- 座標変数など ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=2.0d0
  real(8), parameter :: zmin = -1.0d0, zmax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','tee_module_deriv_test', &
       'tee_module derivative function tests')

 !---------------- 座標値の設定 ---------------------
  ! スペクトル初期化
  call tee_initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax) 

 !------------------- 関数チェック ----------------------
  zyx_Data = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z

  zyx_Deriv = l*pi*cos(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  call AssertEqual(&
    message='tee_Dx_tee with sin(k*pi*X)*sin(l*pi*Y)*Z',          &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Dx_tee(tee_zyx(zyx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = m*pi*sin(l*pi*zyx_X) * cos(m*pi*zyx_Y) * zyx_Z
  call AssertEqual(&
    message='tee_Dy_tee with sin(k*pi*X)*sin(l*pi*Y)*Z',          &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Dy_tee(tee_zyx(zyx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) 
  call AssertEqual(&
    message='tee_Dz_tee with sin(k*pi*X)*sin(l*pi*Y)*Z',          &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Dz_tee(tee_zyx(zyx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -((l*pi)**2+(m*pi)**2)*sin(l*pi*zyx_X) * sin(m*pi*zyx_Y)*zyx_Z
  call AssertEqual(&
    message='tee_Lapla_tee with sin(k*pi*X)*sin(l*pi*Y)*Z',       &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Lapla_tee(tee_zyx(zyx_Data))),            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -((l*pi)**2+(m*pi)**2)*sin(l*pi*zyx_X) * sin(m*pi*zyx_Y)*zyx_Z
  call AssertEqual(&
    message='tee_LaplaH_tee with sin(k*pi*X)*sin(l*pi*Y)*Z',      &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_LaplaH_tee(tee_zyx(zyx_Data))),           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -sin(l*pi*zyx_X) * sin(m*pi*zyx_Y)*zyx_Z/((l*pi)**2+(m*pi)**2)
  call AssertEqual(&
    message='tee_LaplaH_tee with sin(k*pi*X)*sin(l*pi*Y)*Z',      &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_LaplaHInv_tee(tee_zyx(zyx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )


  zyx_Data = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)

  zyx_Deriv = -l*pi*sin(l*pi*zyx_X) *cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  call AssertEqual(&
    message='tee_Dx_tee with cos(k*pi*X)*cos(l*pi*Y)*(2Z**2-1)',  &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Dx_tee(tee_zyx(zyx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -m*pi*cos(l*pi*zyx_X) * sin(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  call AssertEqual(&
    message='tee_Dy_tee with cos(k*pi*X)*cos(l*pi*Y)*(2Z**2-1)',  &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Dy_tee(tee_zyx(zyx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * 4 * zyx_Z
  call AssertEqual(&
    message='tee_Dz_tee with cos(k*pi*X)*cos(l*pi*Y)*(2Z**2-1)',  &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Dz_tee(tee_zyx(zyx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -((l*pi)**2+(m*pi)**2) &
       * cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1) &
       + cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * 4
  call AssertEqual(&
    message='tee_Lapla_tee with cos(k*pi*X)*cos(l*pi*Y)*(2Z**2-1)',  &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Lapla_tee(tee_zyx(zyx_Data))),            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -((l*pi)**2+(m*pi)**2) &
       * cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  call AssertEqual(&
    message='tee_LaplaH_tee with cos(k*pi*X)*cos(l*pi*Y)*(2Z**2-1)',  &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_LaplaH_tee(tee_zyx(zyx_Data))),           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -1.0D0/((l*pi)**2+(m*pi)**2) &
       * cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  call AssertEqual(&
    message='tee_LaplaH_tee with cos(k*pi*X)*cos(l*pi*Y)*(2Z**2-1)',  &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_LaplaHInv_tee(tee_zyx(zyx_Data))),        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = sin(l*pi*zyx_X) * cos(m*pi*zyx_Y)/2.0D0

  zyx_Deriv = l*pi*cos(l*pi*zyx_X) * cos(m*pi*zyx_Y)/2.0D0
  call AssertEqual(&
    message='tee_Dx_tee with sin(k*pi*X)*cos(l*pi*Y)/2',          &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Dx_tee(tee_zyx(zyx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -m*pi*sin(l*pi*zyx_X) * sin(m*pi*zyx_Y)/2.0D0
  call AssertEqual(&
    message='tee_Dy_tee with sin(k*pi*X)*cos(l*pi*Y)/2',          &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Dy_tee(tee_zyx(zyx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = 0.0D0
  call AssertEqual(&
    message='tee_Dz_tee with sin(k*pi*X)*cos(l*pi*Y)/2',          &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Dz_tee(tee_zyx(zyx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -((l*pi)**2+(m*pi)**2)*sin(l*pi*zyx_X) * cos(m*pi*zyx_Y)/2.0D0
  call AssertEqual(&
    message='tee_Lapla_tee with sin(k*pi*X)*cos(l*pi*Y)/2',       &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Lapla_tee(tee_zyx(zyx_Data))),            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -((l*pi)**2+(m*pi)**2)*sin(l*pi*zyx_X) * cos(m*pi*zyx_Y)/2.0D0
  call AssertEqual(&
    message='tee_LaplaH_tee with sin(k*pi*X)*cos(l*pi*Y)/2',      &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_LaplaH_tee(tee_zyx(zyx_Data))),           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -1/((l*pi)**2+(m*pi)**2)*sin(l*pi*zyx_X) * cos(m*pi*zyx_Y)/2.0D0
  call AssertEqual(&
    message='tee_LaplaHInv_tee with sin(k*pi*X)*cos(l*pi*Y)/2',   &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_LaplaHInv_tee(tee_zyx(zyx_Data))),        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Data = cos(l*pi*zyx_X) * sin(m*pi*zyx_Y)*(4*zyx_Z**3-3*zyx_Z)

  zyx_Deriv = - l*pi*sin(l*pi*zyx_X) * sin(m*pi*zyx_Y)*(4*zyx_Z**3-3*zyx_Z)
  call AssertEqual(&
    message='tee_Dx_tee with cos(k*pi*X)*sin(l*pi*Y)*(4Z**3-3Z)', &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Dx_tee(tee_zyx(zyx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = m*pi*cos(l*pi*zyx_X) * cos(m*pi*zyx_Y)*(4*zyx_Z**3-3*zyx_Z)
  call AssertEqual(&
    message='tee_Dy_tee with cos(k*pi*X)*sin(l*pi*Y)*(4Z**3-3Z)', &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Dy_tee(tee_zyx(zyx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = cos(l*pi*zyx_X) * sin(m*pi*zyx_Y)*(12*zyx_Z**2-3)
  call AssertEqual(&
    message='tee_Dz_tee with cos(k*pi*X)*sin(l*pi*Y)*(4Z**3-3Z)', &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Dz_tee(tee_zyx(zyx_Data))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -((l*pi)**2+(m*pi)**2) &
       * cos(l*pi*zyx_X) * sin(m*pi*zyx_Y)*(4*zyx_Z**3-3*zyx_Z) &
       + cos(l*pi*zyx_X) * sin(m*pi*zyx_Y)*(24*zyx_Z)
  call AssertEqual(&
    message='tee_Lapla_tee with cos(k*pi*X)*sin(l*pi*Y)*(4Z**3-3Z)', &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_Lapla_tee(tee_zyx(zyx_Data))),            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -((l*pi)**2+(m*pi)**2) &
       * cos(l*pi*zyx_X) * sin(m*pi*zyx_Y)*(4*zyx_Z**3-3*zyx_Z)
  call AssertEqual(&
    message='tee_LaplaH_tee with cos(k*pi*X)*sin(l*pi*Y)*(4Z**3-3Z)', &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_LaplaH_tee(tee_zyx(zyx_Data))),           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Deriv = -1/((l*pi)**2+(m*pi)**2) &
       * cos(l*pi*zyx_X) * sin(m*pi*zyx_Y)*(4*zyx_Z**3-3*zyx_Z)
  call AssertEqual(&
    message='tee_LaplaHInv_tee with cos(k*pi*X)*sin(l*pi*Y)*(4Z**3-3Z)', &
    answer = zyx_Deriv,                                           &
    check = zyx_tee(tee_LaplaHInv_tee(tee_zyx(zyx_Data))),        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','tee_module_deriv_test', &
       'tee_module derivateiv functions tests succeeded!')

end program tee_module_deriv_test
