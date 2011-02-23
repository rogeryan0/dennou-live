!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  tee_module テストプログラム (トロイダルポロイダルポテンシャル)
!
!履歴  2009/12/19  竹広真一
!
program tee_module_torpol_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use tee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32, km=16       ! 格子点の設定(X,Y)
  integer, parameter :: lm=10, mm=10, nm=10       ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: zyx_Torvel(0:km,0:jm-1,0:im-1)  ! 格子データ
  real(8)            :: zyx_Polvel(0:km,0:jm-1,0:im-1)  ! 格子データ

  real(8)            :: tee_Torvel(0:nm,-mm:mm,-lm:lm)  ! スペクトルデータ
  real(8)            :: tee_Polvel(0:nm,-mm:mm,-lm:lm)  ! スペクトルデータ

  real(8)            :: zyx_VX(0:km,0:jm-1,0:im-1)      ! 格子データ
  real(8)            :: zyx_VY(0:km,0:jm-1,0:im-1)      ! 格子データ
  real(8)            :: zyx_VZ(0:km,0:jm-1,0:im-1)      ! 格子データ

  real(8)            :: zyx_VXans(0:km,0:jm-1,0:im-1)   ! 格子データ
  real(8)            :: zyx_VYans(0:km,0:jm-1,0:im-1)   ! 格子データ
  real(8)            :: zyx_VZans(0:km,0:jm-1,0:im-1)   ! 格子データ

  real(8)            :: zyx_Ans(0:km,0:jm-1,0:im-1)     ! 格子データ

  integer            :: l=2, m=5

 !---- 座標変数など ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=2.0d0
  real(8), parameter :: zmin = -1.0d0, zmax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 9
  integer, parameter :: ignore = -10

  call MessageNotify('M','tee_module_torpol_test', &
       'tee_module toroidal poloidal potential functions tests')

 !---------------- 座標値の設定 ---------------------
  ! スペクトル初期化
  call tee_initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax) 

 !------------- tee_ZRot_zyx_zyx, tee_ZRotRot_zyx_zyx_zyx ---------------
  zyx_VX = - m*pi* cos(l*pi*zyx_X) * sin(m*pi*zyx_Y) * (2*zyx_Z**2 -1) &
              - l*pi* sin(l*pi*zyx_X) * cos(m*pi*zyx_Y) * 4 * zyx_Z
  zyx_VY =   l*pi* sin(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1) &
              - m*pi* cos(l*pi*zyx_X) * sin(m*pi*zyx_Y) * 4 * zyx_Z
  zyx_VZ = ((l*pi)**2 +(m*pi)**2) &
       * cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)

  zyx_Ans = ((l*pi)**2 +(m*pi)**2) &
       * cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  
  call AssertEqual(&
    message='tee_ZRot_zyx_zyx',                                   &
    answer = zyx_Ans,                                             &
    check = zyx_tee(tee_ZRot_zyx_zyx(zyx_VX,zyx_VY)),             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zyx_Ans = -((l*pi)**2 +(m*pi)**2) &
       * (4 - ((l*pi)**2 +(m*pi)**2)*(2*zyx_Z**2 -1)) &
       * cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) 
  
  call AssertEqual(&
    message='tee_ZRotRot_zyx_zyx_zyx',                            &
    answer = zyx_Ans,                                             &
    check = zyx_tee(tee_ZRotRot_zyx_zyx_zyx(zyx_VX,zyx_VY,zyx_VZ)),&
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !------------------- Potential2Vector ----------------------
  zyx_Torvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  zyx_Polvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)

  zyx_VXans = - m*pi* cos(l*pi*zyx_X) * sin(m*pi*zyx_Y) * (2*zyx_Z**2 -1) &
              - l*pi* sin(l*pi*zyx_X) * cos(m*pi*zyx_Y) * 4 * zyx_Z
  zyx_VYans =   l*pi* sin(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1) &
              - m*pi* cos(l*pi*zyx_X) * sin(m*pi*zyx_Y) * 4 * zyx_Z
  zyx_VZans = ((l*pi)**2 +(m*pi)**2) &
       * cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)

  tee_Torvel = tee_zyx(zyx_Torvel)
  tee_Polvel = tee_zyx(zyx_Polvel)
 
  call tee_Potential2Vector(zyx_VX,zyx_VY,zyx_VZ,tee_Torvel,tee_Polvel)

  call AssertEqual(&
    message='tee_Potential2Vector[VX]',                           &
    answer = zyx_VXans,                                           &
    check = zyx_VX,                                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='tee_Potential2Vector[VZ]',                           &
    answer = zyx_VYans,                                           &
    check = zyx_VY,                                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='tee_Potential2Vector[VZ]',                           &
    answer = zyx_VZans,                                           &
    check = zyx_VZ,                                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !------------------- Potential2Rotation ----------------------
  zyx_Torvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  zyx_Polvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)

  zyx_VXans =  - m*pi*((l*pi)**2 +(m*pi)**2) &
       * cos(l*pi*zyx_X) * sin(m*pi*zyx_Y) * (2*zyx_Z**2 -1) &
     - l*pi* sin(l*pi*zyx_X) * cos(m*pi*zyx_Y) * 4*zyx_Z &
     + m*pi* cos(l*pi*zyx_X) * sin(m*pi*zyx_Y) * 4

  zyx_VYans = - m*pi* cos(l*pi*zyx_X) * sin(m*pi*zyx_Y) * 4*zyx_Z &
              - l*pi* sin(l*pi*zyx_X) * cos(m*pi*zyx_Y) * 4 &
         + l*pi*((l*pi)**2 +(m*pi)**2) &
             * sin(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)

  zyx_VZans = ((l*pi)**2 + (m*pi)**2) &
       * cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)

  tee_Torvel = tee_zyx(zyx_Torvel)
  tee_Polvel = tee_zyx(zyx_Polvel)
 
  call tee_Potential2Rotation(zyx_VX,zyx_VY,zyx_VZ,tee_Torvel,tee_Polvel)

  call AssertEqual(&
    message='tee_Potential2Rotation[VX]',                         &
    answer = zyx_VXans,                                           &
    check = zyx_VX,                                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='tee_Potential2Rotation[VY]',                         &
    answer = zyx_VYans,                                           &
    check = zyx_VY,                                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='tee_Potential2Rotation[VZ]',                         &
    answer = zyx_VZans,                                           &
    check = zyx_VZ,                                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','tee_module_torpol_test', &
       'tee_module toroidal poloidal potential functions tests suceeded!')

end program tee_module_torpol_test

