!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  tee_module テストプログラム 
!      (トロイダルポロイダルポテンシャル, スペクトル)
!
!履歴  2009/12/22  竹広真一
!
program tee_module_spectrum_test

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

  real(8)            :: zyx_VX(0:km,0:jm-1,0:im-1)      ! 格子データ
  real(8)            :: zyx_VY(0:km,0:jm-1,0:im-1)      ! 格子データ
  real(8)            :: zyx_VZ(0:km,0:jm-1,0:im-1)      ! 格子データ

  real(8)            :: zee_Spectrum(0:km,-mm:mm,-lm:lm)! スペクトルデータ
  real(8)            :: zee_Ans(0:km,-mm:mm,-lm:lm)     ! スペクトルデータ

  real(8)            :: Ans

  integer            :: l=2, m=5

 !---- 座標変数など ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=2.0d0
  real(8), parameter :: zmin = -1.0d0, zmax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 9
  integer, parameter :: ignore = -10

  call MessageNotify('M','tee_module_spectrum_test', &
       'tee_module toroidal poloidal potential spectrum functions tests')

 !---------------- 座標値の設定 ---------------------
  ! スペクトル初期化
  call tee_initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax) 

 !------------------- zee_ToroidalEnergySpectrum_tee ----------------------
  zyx_Torvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)
  zyx_Polvel = 0.0D0

  call tee_Potential2Vector(zyx_VX,zyx_VY,zyx_VZ,&
       tee_zyx(zyx_Torvel),tee_zyx(zyx_Polvel))

  zee_Ans= 0.0D0
  zee_Ans(:,m,l) = ((l*pi)**2+(m*pi)**2)*((2*z_Z**2 -1)*0.25)**2/2
  zee_Ans(:,-m,-l) = zee_Ans(:,m,l)
  zee_Ans(:,-m,l) = ((l*pi)**2+(m*pi)**2)*((2*z_Z**2 -1)*0.25)**2/2
  zee_Ans(:,m,-l) = zee_Ans(:,-m,l)

  Ans = 0.5*IntZYX_zyx(zyx_VX**2 + zyx_VY**2 + zyx_VZ**2)

  zee_Spectrum=zee_ToroidalEnergySpectrum_tee(tee_zyx(zyx_Torvel))

  call AssertEqual(&
    message='zee_ToroidalEnergySpectrum_tee',                     &
    answer = zee_Ans,                                             &
    check = zee_SPectrum,                                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='zee_ToroidalEnergySpectrum_tee(total)',              &
    answer = Ans,                                                 &
    check = (xmax-xmin)*(ymax-ymin)*IntZ_z(sum(sum(zee_Spectrum,3),2)),&
    significant_digits = check_digits, ignore_digits = ignore     &
    )

 !------------------- zee_PoloidalEnergySpectrum_tee ----------------------
  zyx_Torvel = 0.0D0
  zyx_Polvel = cos(l*pi*zyx_X) * cos(m*pi*zyx_Y) * (2*zyx_Z**2 -1)

  call tee_Potential2Vector(zyx_VX,zyx_VY,zyx_VZ,&
       tee_zyx(zyx_Torvel),tee_zyx(zyx_Polvel))

  zee_Ans= 0.0D0
  zee_Ans(:,m,l) = 0.5*( ((l*pi)**2+(m*pi)**2)*(4*z_Z*0.25)**2 &
                        +((l*pi)**2+(m*pi)**2)**2*((2*z_Z**2 -1)*0.25)**2 )
  zee_Ans(:,-m,-l) = zee_Ans(:,m,l)
                     
  zee_Ans(:,-m,l)= 0.5*( ((l*pi)**2+(m*pi)**2)*(4*z_Z*0.25)**2 &
                        +((l*pi)**2+(m*pi)**2)**2*((2*z_Z**2 -1)*0.25)**2 )
  zee_Ans(:,m,-l) = zee_Ans(:,-m,l)

  Ans = 0.5*IntZYX_zyx(zyx_VX**2 + zyx_VY**2 + zyx_VZ**2)

  zee_Spectrum=zee_PoloidalEnergySpectrum_tee(tee_zyx(zyx_Polvel))

  call AssertEqual(&
    message='zee_PoloidalEnergySpectrum_tee',                     &
    answer = zee_Ans,                                             &
    check = zee_SPectrum,                                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='zee_PoloidalEnergySpectrum_tee(total)',              &
    answer = Ans,                                                 &
    check = (xmax-xmin)*(ymax-ymin)*IntZ_z(sum(sum(zee_Spectrum,3),2)),&
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','tee_module_spectrum_test', &
       'tee_module toroidal poloidal potential spectrum functions tests suceeded!')

end program tee_module_spectrum_test

