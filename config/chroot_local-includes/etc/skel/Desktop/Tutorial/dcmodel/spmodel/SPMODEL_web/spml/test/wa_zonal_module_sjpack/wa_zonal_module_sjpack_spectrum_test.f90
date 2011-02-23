!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wa_zonal_module_sjpack テストプログラム :: 補間関数のテスト
!
!履歴  2009/09/24  竹広真一
!
program wa_zonal_module_sjpack_spectrum_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wa_zonal_module_sjpack
  implicit none

  integer, parameter :: im=1, jm=8, nm=10, km=2

  real(8), dimension(nm+1,km)        ::  wa_data
  real(8), dimension(0:im-1,1:jm,km) ::  xya_data

  real(8), dimension(0:nm,-nm:nm,km)      ::  nma_data
  real(8), dimension(0:nm,km)        ::  na_data
  real(8), dimension(km)             ::  a_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  integer :: n,m

  call wa_Initial( nm, im, jm, km )

  call MessageNotify('M','wa_zonal_module_sjpack_spectrum_test',&
       'wa_zonal_module_sjpack spectral function tests') 

  ! 2Y_1^0+3Y_2^0
  xya_data(:,:,1) = 2*sqrt(3.0D0)*sin(xy_Lat) &            ! 2Y_1^0+3Y_2^0
                  + 3*sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)

  ! 3*Y_1^0 + 2Y_2^0 + Y_3^0
  xya_data(:,:,2) = 3*sqrt(3.0D0)*sin(xy_Lat) & 
                + 2*sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0) &
                + sqrt(7.0D0)*(5.0/2*sin(xy_Lat)**3-3*sin(xy_Lat)/2.0)

  wa_data = wa_xya(xya_data)

  !--------- Energy spectrum ---------
  nma_data=wa_spectrum_VMiss

  do n=0,nm
     do m=-n,n
        nma_data(n,m,:) = 0.0D0
     enddo
  enddo

  nma_data(1,0,1) = 4.0D0 ; nma_data(2,0,1) = 27.0D0

  nma_data(1,0,2) = 9.0D0 ; nma_data(2,0,2) = 12.0D0
  nma_data(3,0,2) = 6.0D0

  call AssertEqual(&
    message='nma_EnergyFromStreamFunc',                           &
    answer = nma_data,                                            &
    check = nma_EnergyFromStreamfunc_wa(wa_data),                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  na_data = 0.0D0
  na_data(:,1) = 0.0D0
  na_data(1,1) = 4.0D0 ; na_data(2,1) = 27.0D0
  na_data(1,2) = 9.0D0 ; na_data(2,2) = 12.0D0 ; na_data(3,2) = 6.0D0

  call AssertEqual(&
    message='na_EnergyFromStreamFunc',                            &
    answer = na_data,                                             &
    check = na_EnergyFromStreamfunc_wa(wa_data),                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  a_data = 0.0D0
  nma_data = nma_EnergyFromStreamfunc_wa(wa_data)
  do n=0,nm
     do m=-n,n
        a_data = a_data + nma_data(n,m,:)
     enddo
  enddo

  call AssertEqual(&
    message='total energy by nma-spectrum',                       &
    answer =  a_AvrLonLat_xya( &
              0.5*(xya_GradLon_wa(wa_data)**2+xya_GradLat_wa(wa_data)**2)),&
    check = a_data,                                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='total energy by na-spectrum',                         &
    answer =  a_AvrLonLat_xya( &
              0.5*(xya_GradLon_wa(wa_data)**2+xya_GradLat_wa(wa_data)**2)),&
    check = sum(na_EnergyFromStreamfunc_wa(wa_data),1),           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !--------- Enstrophy spectrum ---------
  nma_data=wa_spectrum_VMiss
  do n=0,nm
     do m=-n,n
        nma_data(n,m,:) = 0.0
     enddo
  enddo

  nma_data(1,0,1) = 8.0D0  ; nma_data(2,0,1) = 162.0D0

  nma_data(1,0,2) = 18.0D0 ; nma_data(2,0,2) = 72.0D0
  nma_data(3,0,2) = 72.0D0

  call AssertEqual(&
    message='nma_EnstrophyFromStreamFunc',                        &
    answer = nma_data,                                            &
    check = nma_EnstrophyFromStreamfunc_wa(wa_data),              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  na_data = 0.0
  na_data(1,1) = 8.0D0  ; na_data(2,1) = 162.0D0
  na_data(1,2) = 18.0D0 ; na_data(2,2) = 72.0D0 ; na_data(3,2) = 72.0D0

  call AssertEqual(&
    message='na_EnstrophyFromStreamFunc',                         &
    answer = na_data,                                             &
    check = na_EnstrophyFromStreamfunc_wa(wa_data),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  a_data = 0.0D0
  nma_data = nma_EnstrophyFromStreamfunc_wa(wa_data)
  do n=0,nm
     do m=-n,n
        a_data = a_data + nma_data(n,m,:)
     enddo
  enddo

  call AssertEqual(&
    message='total enstrophy by nma-spectrum',                       &
    answer = a_AvrLonLat_xya( 0.5*(xya_wa(wa_Lapla_wa(wa_data))**2)),&
    check = a_data,                                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='total enstrophy by na-spectrum',                        &
    answer = a_AvrLonLat_xya( 0.5*(xya_wa(wa_Lapla_wa(wa_data))**2)),&
    check = sum(na_EnstrophyFromStreamfunc_wa(wa_data),1),        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wa_zonal_module_sjpack_spectrum_test',&
       'wa_zonal_module_sjpack spectral function tests succeeded!') 

end program wa_zonal_module_sjpack_spectrum_test
