!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wq_module テストプログラム :: スペクトル関数のテスト
!
!履歴  2009/12/09  竹広真一 
!
program wq_module_spectrum_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wq_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=31         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ro=1.5               ! 内外半径

  real(8), dimension(0:im-1,1:jm,km) ::  xyr_Torvel
  real(8), dimension((nm+1)**2,0:lm) ::  wq_Torvel

  real(8), dimension(0:im-1,1:jm,km) ::  xyr_Polvel
  real(8), dimension((nm+1)**2,0:lm) ::  wq_Polvel

  real(8), dimension(0:im-1,1:jm,km) ::  xyr_Vlon
  real(8), dimension(0:im-1,1:jm,km) ::  xyr_Vlat
  real(8), dimension(0:im-1,1:jm,km) ::  xyr_Vrad

  real(8), dimension(0:nm,-nm:nm,km) ::  nmr_EkTor
  real(8), dimension(0:nm,-nm:nm,km) ::  nmr_EkPol
  real(8), dimension(0:nm,km)        ::  nr_EkTor
  real(8), dimension(0:nm,km)        ::  nr_EkPol

  real(8), dimension(km)             ::  r_Data

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  real(8) :: PI
  integer :: n,m

  PI = 4.0D0*Atan(1.0D0)

  call wq_Initial(im,jm,km,nm,lm,ro)

  call MessageNotify('M','wq_module_spectrum_test',&
                         'wq_module spectrum functions tests.') 

  ! 2Y_1^0 * r
  xyr_Torvel = 2*sqrt(3.0D0)*sin(xyr_Lat) * xyr_Rad

  ! (2Y_2^0 +Y_2^-2) * r^2
  xyr_Polvel =( 2*sqrt(5.0D0)*(3.0/2*sin(xyr_Lat)**2-1/2.0) &
                - sqrt(2*5.0D0/24)*3.0*cos(xyr_Lat)**2*sin(2*xyr_Lon) ) &
              * xyr_Rad**2

  wq_Torvel = wq_xyr(xyr_Torvel)
  wq_Polvel = wq_xyr(xyr_Polvel)

  !--------- Energy spectrum ---------
  nmr_EkTor=wq_VMiss
  nmr_EkPol=wq_VMiss

  do n=0,nm
     do m=-n,n
        nmr_EkTor(n,m,:) = 0.0D0
        nmr_EkPol(n,m,:) = 0.0D0
     enddo
  enddo

  nmr_EkTor(1,0,:) = 4.0D0 * r_Rad**2 * (4*pi)*r_Rad**2

  nmr_EkPol(2,0,:) = 12.0D0 *(9*r_Rad**4 + 6.0D0*r_Rad**4) * (4*pi)

  nmr_EkPol(2,-2,:)= 3.0D0 * ( 9*r_Rad**4 + 2*3*r_Rad**4 ) * (4*pi)

  call AssertEqual(&
    message='nmr_ToroidalEnergySpectrum_wq',                      &
    answer = nmr_ToroidalEnergySpectrum_wq(wq_Torvel),            &
    check =  nmr_EkTor,                                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='nmr_PoloidalEnergySpectrum_wq',                      &
    answer = nmr_PoloidalEnergySpectrum_wq(wq_Polvel),            &
    check =  nmr_EkPol,                                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  nr_EkTor = 0.0D0
  nr_EkTor(1,:) = 4.0D0 * r_Rad**2 * (4*pi)*r_Rad**2 

  nr_EkPol = 0.0D0
  nr_EkPol(2,:) = 15.0D0*(9*r_Rad**4+2*3*r_Rad**4)*(4*pi)

  call AssertEqual(&
    message='nr_ToroidalEnergySpectrum_wq',                       &
    answer = nr_ToroidalEnergySpectrum_wq(wq_Torvel),             &
    check =  nr_EkTor,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='nr_PoloidalEnergySpectrum_wq',                       &
    answer = nr_PoloidalEnergySpectrum_wq(wq_Polvel),             &
    check =  nr_EkPol,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call wq_Potential2Vector(xyr_Vlon,xyr_Vlat,xyr_Vrad,wq_Torvel,wq_Polvel)

  nmr_EkTor = nmr_ToroidalEnergySpectrum_wq(wq_Torvel)
  nmr_EkPol = nmr_PoloidalEnergySpectrum_wq(wq_Polvel)

  r_Data = 0.0D0
  do n=0,nm
     do m=-n,n
        r_Data = r_Data + nmr_EkTor(n,m,:) + nmr_EkPol(n,m,:)
     enddo
  enddo

  call AssertEqual(&
    message='Total energy by nmr-spectrum',                        &
    answer = IntLonLatRad_xyr(xyr_VLon**2+xyr_VLat**2+xyr_VRad**2)/2.0D0, &
    check =  IntRad_r(r_Data/r_Rad**2),                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  nr_EkTor = nr_ToroidalEnergySpectrum_wq(wq_Torvel)
  nr_EkPol = nr_PoloidalEnergySpectrum_wq(wq_Polvel)

  call AssertEqual(&
    message='Total energy by nr-spectrum',                        &
    answer = IntLonLatRad_xyr(xyr_VLon**2+xyr_VLat**2+xyr_VRad**2)/2.0D0, &
    check =   IntRad_r(sum(nr_EkTor,1)/r_Rad**2) &
            + IntRad_r(sum(nr_EkPol,1)/r_Rad**2), &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wq_module_spectrum_test',&
                         'wq_module spectrum functions tests suceeded!') 

end program wq_module_spectrum_test
