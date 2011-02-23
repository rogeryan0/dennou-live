!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_spectrum_mpi_module テストプログラム :: スペクトル解析関数のテスト
!
!履歴  2009/09/24  竹広真一   w_spectrum_module_test.f90 を改造
!
program w_spectrum_mpi_module_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use w_mpi_module
  implicit none
  include 'mpif.h'

  integer, parameter :: im=32, jm=16, nm=10


  real(8), dimension((nm+1)**2)   ::  w_data
  real(8), dimension(0:im-1,1:jm) ::  xy_data

  real(8), dimension(0:nm,-nm:nm) ::  nm_data
  real(8), dimension(0:nm)        ::  n_data
  real(8)                         ::  data

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  integer :: n,m
  integer :: iproc, np, ierr

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call w_mpi_Initial( nm, im, jm )

  call MessageNotify('M','w_spectrum_mpi_module_test',&
                         'w_spectrum_mpi_module tests') 

  !================= 2Y_1^0+3Y_2^1 のテスト ================
  xy_data = 2*sqrt(3.0D0)*sin(xy_Lat) &        ! 2Y_1^0+3Y_2^1
       + 3*sqrt(2*5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)! 
  w_data = w_xy(xy_data)

  !--------- Energy spectrum ---------
  nm_data=w_spectrum_VMiss
  do n=0,nm
     do m=-n,n
        nm_data(n,m) = 0.0
     enddo
  enddo
  nm_data(1,0) = 4.0D0 ;   nm_data(2,1) = 27.0D0
  call AssertEqual(&
    message='nm_EnergyFromStreamfunc_w with 2Y_1^0+3Y_2^1 ',      &
    answer = nm_data,                                             &
    check = nm_EnergyFromStreamfunc_w(w_data),                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  n_data = 0.0; n_data(1) = 4.0D0 ;   n_data(2) = 27.0D0
  call AssertEqual(&
    message='n_EnergyFromStreamfunc_w with 2Y_1^0+3Y_2^1 ',       &
    answer = n_data,                                              &
    check = n_EnergyFromStreamfunc_w(w_data),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  nm_Data = nm_EnergyFromStreamfunc_w(w_data)
  data = 0.0D0
  do n=0,nm
     do m=-n,n
        data = data + nm_Data(n,m)
     enddo
  enddo

  call AssertEqual(&
    message='total energy by nm-spectrum with 2Y_1^0+3Y_2^1 ',    &
    answer = AvrLonLat_xy(xy_w(w_xv(                              &
       0.5*(xv_GradLon_w(w_data)**2+xv_GradLat_w(w_data)**2) ))), &
    check = data,                                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  data = AvrLonLat_xy(xy_w(w_xv( &
             0.5*(xv_GradLon_w(w_data)**2+xv_GradLat_w(w_data)**2) )))
  call AssertEqual(&
    message='total energy by n-spectrum with 2Y_1^0+3Y_2^1 ',     &
    answer = data,                                                &
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
  nm_data(1,0) = 8.0D0 ;   nm_data(2,1) = 162.0D0
  call AssertEqual(&
    message='nm_EnstrophyFromStreamfunc_w with 2Y_1^0+3Y_2^1 ',   &
    answer = nm_data,                                             &
    check = nm_EnstrophyFromStreamfunc_w(w_data),                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  n_data = 0.0; n_data(1) = 8.0D0 ;   n_data(2) = 162.0D0
  call AssertEqual(&
    message='n_EnstrophyFromStreamfunc_w with 2Y_1^0+3Y_2^1 ',    &
    answer = n_data,                                              &
    check = n_EnstrophyFromStreamfunc_w(w_data),                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  nm_Data = nm_EnstrophyFromStreamfunc_w(w_data)
  data = 0.0D0
  do n=0,nm
     do m=-n,n
        data = data + nm_Data(n,m)
     enddo
  enddo

  call AssertEqual(&
    message='total enstrophy by nm-spectrum with 2Y_1^0+3Y_2^1 ', &
    answer =  AvrLonLat_xy( 0.5*(xy_w(w_Lapla_w(w_data))**2)),    &
    check = data,                                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  data = AvrLonLat_xy( 0.5*(xy_w(w_Lapla_w(w_data))**2))
  call AssertEqual(&
    message='total enstrophy by n-spectrum with 2Y_1^0+3Y_2^1 ',  &
    answer = data,                                                &
    check = sum(n_EnstrophyFromStreamfunc_w(w_data)),             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !======== 3*Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2 のテスト ========
  xy_data = 3*sqrt(3.0D0)*sin(xy_Lat) &        ! 2Y_1^0+3Y_2^1
          + 2* sqrt(2*3.0D0/2)*cos(xy_Lat)*cos(xy_Lon) &
          + 2*sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0) &
          - sqrt(2*5.0D0/24)*3.0*cos(xy_Lat)**2*sin(2*xy_Lon)
  w_data = w_xy(xy_data)

  !--------- Energy spectrum ---------
  nm_data=w_spectrum_VMiss
  do n=0,nm
     do m=-n,n
        nm_data(n,m) = 0.0
     enddo
  enddo
  nm_data(1,0) = 9.0D0 ; nm_data(1,1)  = 4.0D0
  nm_data(2,0) = 12.0D0 ; nm_data(2,-2) = 3.0D0
  call AssertEqual(&
    message='nm_EnergyFromStreamfunc_w with 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2',&
    answer = nm_data,                                             &
    check = nm_EnergyFromStreamfunc_w(w_data),                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  n_data = 0.0; n_data(1) = 13.0D0 ;   n_data(2) = 15.0D0
  call AssertEqual(&
    message='n_EnergyFromStreamfunc_w with 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2', &
    answer = n_data,                                              &
    check = n_EnergyFromStreamfunc_w(w_data),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  nm_Data = nm_EnergyFromStreamfunc_w(w_data)
  data = 0.0D0
  do n=0,nm
     do m=-n,n
        data = data + nm_Data(n,m)
     enddo
  enddo

  call AssertEqual(&
    message='total energy by nm-spectrum with 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2', &
    answer = AvrLonLat_xy(xy_w(w_xv( &
             0.5*(xv_GradLon_w(w_data)**2+xv_GradLat_w(w_data)**2) ))), 
    check = data,                                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  data = AvrLonLat_xy(xy_w(w_xv( &
             0.5*(xv_GradLon_w(w_data)**2+xv_GradLat_w(w_data)**2) )))
  call AssertEqual(&
    message='total energy by n-spectrum with 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2', &
    answer = data,                                                &
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
  nm_data(1,0) = 18.0D0 ; nm_data(1,1)  = 8.0D0
  nm_data(2,0) = 72.0D0 ; nm_data(2,-2) = 18.0D0
  call AssertEqual(&
    message='nm_EnstrophyFromStreamfunc_w with 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2', &
    answer = nm_data,                                             &
    check = nm_EnstrophyFromStreamfunc_w(w_data),                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  n_data = 0.0; n_data(1) = 26.0D0 ;   n_data(2) = 90.0D0
  call AssertEqual(&
    message='n_EnstrophyFromStreamfunc_w with 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2', &
    answer = n_data,                                              &
    check = n_EnstrophyFromStreamfunc_w(w_data),                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  nm_Data = nm_EnstrophyFromStreamfunc_w(w_data)
  data = 0.0D0
  do n=0,nm
     do m=-n,n
        data = data + nm_Data(n,m)
     enddo
  enddo

  call AssertEqual(&
    message='total enstrophy by nm-spectrum with 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2',&
    answer =  AvrLonLat_xy( 0.5*(xy_w(w_Lapla_w(w_data))**2)),    &
    check = data,                                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  data = AvrLonLat_xy( 0.5*(xy_w(w_Lapla_w(w_data))**2))
  call AssertEqual(&
    message='total enstrophy by n-spectrum with 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2',&
    answer = data,                                                &
    check = sum(n_EnstrophyFromStreamfunc_w(w_data)),             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','w_spectrum_mpi_module_test', &
                         'w_spectrum_mpi_module tests succeeded!') 

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)      

end program w_spectrum_mpi_module_test
