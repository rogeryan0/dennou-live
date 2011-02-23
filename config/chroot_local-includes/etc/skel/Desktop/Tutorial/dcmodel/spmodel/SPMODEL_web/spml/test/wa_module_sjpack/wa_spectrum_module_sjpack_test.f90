!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wa_spectrum_module_sjpack テストプログラム :: スペクトル関数のテスト
!
!履歴  2009/09/06  竹広真一 wa_test_spectrum.f90 より SJPACK 用に改造
!      2009/09/22  竹広真一 nma_* の計算修正
!      2009/09/24  竹広真一 total energy, enstrphy check 追加
!
program wa_spectrum_module_sjpack_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wa_module_sjpack
  implicit none

  integer, parameter :: im=16, jm=8, nm=5, km=2

  real(8), dimension((nm+1)**2,km)   ::  wa_data
  real(8), dimension(0:im-1,1:jm,km) ::  xya_data

  real(8), dimension(0:nm,-nm:nm,km) ::  nma_data
  real(8), dimension(0:nm,km)        ::  na_data
  real(8), dimension(km)             ::  a_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  integer :: n,m

  call wa_Initial( nm, im, jm, km )

  call MessageNotify('M','wa_spectrum_module_sjpack_test',&
                         'wa_spectrum_module_sjpack functions tests.') 

  ! 2Y_1^0+3Y_2^1
  xya_data(:,:,1) = 2*sqrt(3.0D0)*sin(xy_Lat) &
                + 3*sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)! 
  ! 3*Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2
  xya_data(:,:,2) = 3*sqrt(3.0D0)*sin(xy_Lat) & 
                + 2* sqrt(3.0D0/2)*cos(xy_Lat)*cos(xy_Lon) &
                + 2*sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0) &
                - sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*sin(2*xy_Lon)

  wa_data = wa_xya(xya_data)

  !--------- Energy spectrum ---------
  nma_data=wa_spectrum_VMiss

  do n=0,nm
     do m=-n,n
        nma_data(n,m,:) = 0.0D0
     enddo
  enddo

  nma_data(1,0,1) = 4.0D0
  nma_data(2,1,1) = 2*3*(3.0D0/2)**2/2
  nma_data(2,-1,1)= nma_data(2,1,1)
  nma_data(1,0,2) = 9.0D0
  nma_data(1,1,2) = 1*2*(2/2)**2/2
  nma_data(1,-1,2)= nma_data(1,1,2)
  nma_data(2,0,2) = 12.0D0 
  nma_data(2,-2,2)= 2*3*(1.0D0/2)**2/2
  nma_data(2,2,2) = nma_data(2,-2,2)

  call AssertEqual(&
    message='nma_EnergyFromStreamfunc_wa',                        &
    answer = nma_EnergyFromStreamfunc_wa(wa_data),                &
    check =  nma_data,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  na_data = 0.0D0
  na_data(1,1) = 4.0D0 
  na_data(2,1) = 2 * 3 *(3.0D0/2)**2
  na_data(2,-1)= na_data(2,1) 
  na_data(1,2) = 9.0D0 + 1*2*(2/2)**2
  na_data(1,-2)= na_data(1,2)
  na_data(2,2) = 12.0D0 + 2*3*(1.0D0/2)**2
  na_data(2,-2) = na_data(2,2)

  call AssertEqual(&
    message='na_EnergyFromStreamfunc_wa',                         &
    answer = na_EnergyFromStreamfunc_wa(wa_data),                 &
    check =  na_data,                                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  nma_Data = nma_EnergyFromStreamfunc_wa(wa_data)
  a_data = 0.0D0
  do n=0,nm
     do m=-n,n
        a_data = a_data + nma_Data(n,m,:)
     enddo
  enddo

  call AssertEqual(&
    message='total energy by nm-spectrum',                        &
    answer = a_AvrLonLat_xya(                                     &
    0.5*(xya_GradLon_wa(wa_data)**2+xya_GradLat_wa(wa_data)**2) ),&
    check = a_data,                                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  a_data = a_AvrLonLat_xya( 0.5*(xya_GradLon_wa(wa_data)**2+xya_GradLat_wa(wa_data)**2) )
  call AssertEqual(&
    message='total energy with n-spectrum',                       &
    answer = sum(na_EnergyFromStreamfunc_wa(wa_data),1),          &
    check =  a_data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !--------- Enstrophy spectrum ---------
  nma_data=wa_spectrum_VMiss
  do n=0,nm
     do m=-n,n
        nma_data(n,m,:) = 0.0
     enddo
  enddo

  nma_data(1,0,1) = 8.0D0
  nma_data(2,1,1) = 2**2*3**2*(3.0D0/2)**2/2
  nma_data(2,-1,1) = nma_data(2,1,1) 

  nma_data(1,0,2)  = 18.0D0
  nma_data(1,1,2)  = 1**2 * 2**2 * (2/2)**2 / 2
  nma_data(1,-1,2) = nma_data(1,1,2)
  nma_data(2,0,2)  = 72.0D0
  nma_data(2,-2,2) = 2**2 * 3**2 * (1.0D0/2)**2 /2
  nma_data(2,2,2)  = nma_data(2,-2,2)

  call AssertEqual(&
    message='nma_EnstrophyFromStreamfunc_wa',                     &
    answer = nma_EnstrophyFromStreamfunc_wa(wa_data),             &
    check =  nma_data,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  na_data = 0.0
  na_data(1,1) = 8.0D0  ;   na_data(2,1) = 2**2*3**2*(3.0D0/2)**2
  na_data(1,2) = 18.0D0 + 1**2 * 2**2 * (2/2)**2
  na_data(2,2) = 72.0D0 + 2**2 * 3**2 * (1.0D0/2)**2

  call AssertEqual(&
    message='na_EnstrophyFromStreamfunc_wa',                      &
    answer = na_EnstrophyFromStreamfunc_wa(wa_data),              &
    check =  na_data,                                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  nma_Data = nma_EnstrophyFromStreamfunc_wa(wa_data)
  a_data = 0.0D0
  do n=0,nm
     do m=-n,n
        a_data = a_data + nma_Data(n,m,:)
     enddo
  enddo

  call AssertEqual(&
    message='total enstrophy by nm-spectrum',                     &
    answer = a_AvrLonLat_xya( 0.5*(xya_wa(wa_Lapla_wa(wa_data))**2)),&
    check = a_data,                                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  a_data = a_AvrLonLat_xya( 0.5*(xya_wa(wa_Lapla_wa(wa_data))**2))

  call AssertEqual(&
    message='total energy with n-spectrum',                       &
    answer = sum(na_EnstrophyFromStreamfunc_wa(wa_data),1),       &
    check =  a_data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wa_spectrum_module_sjpack_test', &
                     'wa_spectrum_module_sjpack tests succeeded!') 

end program wa_spectrum_module_sjpack_test
