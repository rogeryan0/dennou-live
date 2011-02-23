!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_zonal_module_sjpack テストプログラム :: スペクトル関数のテスト
!
!履歴  2009/09/26  竹広真一 
!
program wt_zonal_module_sjpack_spectrum_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wt_zonal_module_sjpack
  implicit none

  integer,parameter  :: im=1, jm=16, km=16   ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

  real(8), dimension(0:im-1,1:jm,0:km) ::  xyz_Torvel
  real(8), dimension(nm+1,0:lm)        ::  wt_Torvel

  real(8), dimension(0:im-1,1:jm,0:km) ::  xyz_Polvel
  real(8), dimension(nm+1,0:lm)        ::  wt_Polvel

  real(8), dimension(0:im-1,1:jm,0:km) ::  xyz_Vlon
  real(8), dimension(0:im-1,1:jm,0:km) ::  xyz_Vlat
  real(8), dimension(0:im-1,1:jm,0:km) ::  xyz_Vrad

  real(8), dimension(0:nm,-nm:nm,0:km) ::  nmz_EkTor
  real(8), dimension(0:nm,-nm:nm,0:km) ::  nmz_EkPol
  real(8), dimension(0:nm,0:km)        ::  nz_EkTor
  real(8), dimension(0:nm,0:km)        ::  nz_EkPol

  real(8), dimension(0:km)             ::  z_Data

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  real(8) :: PI
  integer :: n,m

  PI = 4.0D0*Atan(1.0D0)

  call wt_Initial(im,jm,km,nm,lm,ri,ro)

  call MessageNotify('M','wt_zonal_module_sjpack_spectrum_test',&
                         'wt_zonal_module_sjpack spectrum functions tests.') 

  ! 2Y_1^0 * r
  xyz_Torvel =( 2*sqrt(3.0D0)*sin(xyz_Lat) ) * xyz_Rad

  ! (3*Y_1^0 + 2Y_2^0 ) * r^2
  xyz_Polvel =( 3*sqrt(3.0D0)*sin(xyz_Lat) & 
                + 2*sqrt(5.0D0)*(3.0/2*sin(xyz_Lat)**2-1/2.0) )&
              * xyz_Rad**2

  wt_Torvel = wt_xyz(xyz_Torvel)
  wt_Polvel = wt_xyz(xyz_Polvel)

  !--------- Energy spectrum ---------
  nmz_EkTor=wt_VMiss
  nmz_EkPol=wt_VMiss

  do n=0,nm
     do m=-n,n
        nmz_EkTor(n,m,:) = 0.0D0
        nmz_EkPol(n,m,:) = 0.0D0
     enddo
  enddo

  nmz_EkTor(1,0,:) = 4.0D0 * z_Rad**2 * (4*pi)*z_Rad**2

  nmz_EkPol(1,0,:) = 9.0D0 * (9*z_Rad**4 + 2.0D0*z_Rad**4) * (4*pi)
  nmz_EkPol(2,0,:) = 12.0D0 *(9*z_Rad**4 + 6.0D0*z_Rad**4) * (4*pi)

  call AssertEqual(&
    message='nmz_ToroidalEnergySpectrum_wt',                      &
    answer = nmz_ToroidalEnergySpectrum_wt(wt_Torvel),            &
    check =  nmz_EkTor,                                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='nmz_PoloidalEnergySpectrum_wt',                      &
    answer = nmz_PoloidalEnergySpectrum_wt(wt_Polvel),            &
    check =  nmz_EkPol,                                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  nz_EkTor = 0.0D0
  nz_EkTor(1,:) = 4.0D0 * z_Rad**2 * (4*pi)*z_Rad**2 

  nz_EkPol = 0.0D0
  nz_EkPol(1,:) = (9.0D0 )*(9*z_Rad**4+2*z_Rad**4)*(4*pi)
  nz_EkPol(2,:) = (12.0D0)*(9*z_Rad**4+2*3*z_Rad**4)*(4*pi)

  call AssertEqual(&
    message='nz_ToroidalEnergySpectrum_wt',                       &
    answer = nz_ToroidalEnergySpectrum_wt(wt_Torvel),             &
    check =  nz_EkTor,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='nz_PoloidalEnergySpectrum_wt',                       &
    answer = nz_PoloidalEnergySpectrum_wt(wt_Polvel),             &
    check =  nz_EkPol,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call wt_Potential2Vector(xyz_Vlon,xyz_Vlat,xyz_Vrad,wt_Torvel,wt_Polvel)

  nmz_EkTor = nmz_ToroidalEnergySpectrum_wt(wt_Torvel)
  nmz_EkPol = nmz_PoloidalEnergySpectrum_wt(wt_Polvel)

  z_Data = 0.0D0
  do n=0,nm
     do m=-n,n
        z_Data = z_Data + nmz_EkTor(n,m,:) + nmz_EkPol(n,m,:)
     enddo
  enddo

  call AssertEqual(&
    message='Total energy by nmz-spectrum',                        &
    answer = IntLonLatRad_xyz(xyz_VLon**2+xyz_VLat**2+xyz_VRad**2)/2.0D0, &
    check =  IntRad_z(z_Data/z_Rad**2),                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  nz_EkTor = nz_ToroidalEnergySpectrum_wt(wt_Torvel)
  nz_EkPol = nz_PoloidalEnergySpectrum_wt(wt_Polvel)

  call AssertEqual(&
    message='Total energy by nz-spectrum',                        &
    answer = IntLonLatRad_xyz(xyz_VLon**2+xyz_VLat**2+xyz_VRad**2)/2.0D0, &
    check =   IntRad_z(sum(nz_EkTor,1)/z_Rad**2) &
            + IntRad_z(sum(nz_EkPol,1)/z_Rad**2), &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wt_zonal_module_sjpack_spectrum_test',&
                         'wt_zonal_module_sjpack spectrum functions tests suceeded!') 

end program wt_zonal_module_sjpack_spectrum_test
