!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_zonal_module テストプログラム :: スペクトル解析関数のテスト
!
!履歴  2009/09/24  竹広真一
!
program w_zonal_module_spectrum_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use w_zonal_module
  implicit none

  integer, parameter :: im=1, jm=8, nm=10

  real(8), dimension(nm+1)        ::  w_data
  real(8), dimension(0:im-1,1:jm) ::  xy_data

  real(8), dimension(0:nm,-nm:nm) ::  nm_data
  real(8), dimension(0:nm)        ::  n_data
  real(8)                         ::  data

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  integer :: n,m

  call w_Initial( nm, im, jm )

  call MessageNotify('M','w_zonal_module_spectrum_test',&
                         'w_zonal_module spectrum function tests') 

  !================= 2Y_1^0+3Y_2^0 のテスト ================
  xy_data = 2*sqrt(3.0D0)*sin(xy_Lat) &                  ! 2Y_1^0+3Y_2^0
          + 3*sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)
  w_data = w_xy(xy_data)

  !--------- Energy spectrum ---------
  nm_data=w_spectrum_VMiss
  do n=0,nm
     do m=-n,n
        nm_data(n,m) = 0.0
     enddo
  enddo
  nm_data(1,0) = 4.0D0 ;   nm_data(2,0) = 27.0D0

  call AssertEqual(&
    message='nm_EnergyFromStreamFunc',                            &
    answer = nm_data,                                             &
    check = nm_EnergyFromStreamfunc_w(w_data),                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  n_data = 0.0; n_data(1) = 4.0D0 ;   n_data(2) = 27.0D0

  call AssertEqual(&
    message='n_EnergyFromStreamFunc',                             &
    answer = n_data,                                              &
    check = n_EnergyFromStreamfunc_w(w_data),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  data = 0.0D0
  nm_data = nm_EnergyFromStreamfunc_w(w_data)
  do n=0,nm
     do m=-n,n
        data = data + nm_data(n,m)
     enddo
  enddo

  call AssertEqual(&
    message='total energy by nm-spectrum',                       &
    answer =  AvrLonLat_xy( &
              0.5*(xy_GradLon_w(w_data)**2+xy_GradLat_w(w_data)**2) ),&
    check = data,                                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='total energy by n-spectrum',                        &
    answer =  AvrLonLat_xy( &
              0.5*(xy_GradLon_w(w_data)**2+xy_GradLat_w(w_data)**2) ),&
    check = sum(n_EnergyFromStreamfunc_w(w_data)),                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !--------- Enstrophy spectrum ---------
  nm_data=w_spectrum_VMiss
  do n=0,nm
     do m=-n,n
        nm_data(n,m) = 0.0
     enddo
  enddo

  nm_data(1,0) = 8.0D0 ;   nm_data(2,0) = 162.0D0

  call AssertEqual(&
    message='nm_EnstrophyFromStreamFunc',                         &
    answer = nm_data,                                             &
    check = nm_EnstrophyFromStreamfunc_w(w_data),                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  n_data = 0.0; n_data(1) = 8.0D0 ;   n_data(2) = 162.0D0

  call AssertEqual(&
    message='n_EnstrophyFromStreamFunc',                          &
    answer = n_data,                                              &
    check = n_EnstrophyFromStreamfunc_w(w_data),                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  data = 0.0D0
  nm_data = nm_EnstrophyFromStreamfunc_w(w_data)
  do n=0,nm
     do m=-n,n
        data = data + nm_data(n,m)
     enddo
  enddo

  call AssertEqual(&
    message='total enstrophy by nm-spectrum',                     &
    answer = AvrLonLat_xy( 0.5*(xy_w(w_Lapla_w(w_data))**2)),     &
    check = data,                                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='total enstrophy by n-spectrum',                      &
    answer = AvrLonLat_xy( 0.5*(xy_w(w_Lapla_w(w_data))**2)),     &
    check = sum(n_EnstrophyFromStreamfunc_w(w_data)),             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','w_zonal_module_spectrum_test',&
                         'w_zonal_module spectrum function tests succeeded!')

end program w_zonal_module_spectrum_test
