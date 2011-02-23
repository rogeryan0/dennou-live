!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_spectrum_module テストプログラム :: 補間関数のテスト
!
!履歴  2008/05/26  竹広真一
!
program w_spectrum_mpi_test

  use dc_message, only : MessageNotify
  use w_mpi_module
  implicit none
  include 'mpif.h'

  integer, parameter :: im=16, jm=8, nm=10

  real(8), dimension((nm+1)**2) ::  w_data
  real(8), dimension(im,jm)     ::  xy_data

  real(8), dimension(0:nm,-nm:nm) ::  nm_data
  real(8), dimension(0:nm)        ::  n_data
  real(8)                         ::  data

  real(8), parameter            ::  eps = 1.0D-10

  real(8) :: error
  integer :: n,m
  integer :: iproc, np, ierr

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call w_mpi_Initial( nm, im, jm )

  call MessageNotify('M','w_spectrum_mpi_test','w_spectrum_module tests') 

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
  error = maxval(abs(nm_EnergyFromStreamfunc_w(w_data)-nm_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of nm_EnergyFromStreamFunc',&
                        '2Y_1^0+3Y_2^1 Energy error too large') 
  endif
  call MessageNotify('M','Test of nm_EnergyFromStreamFunc', &
       'Test of 2Y_1^0+3Y_2^1 succeeded!') 

  n_data = 0.0; n_data(1) = 4.0D0 ;   n_data(2) = 27.0D0
  error = maxval(abs(n_EnergyFromStreamfunc_w(w_data)-n_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of n_EnergyFromStreamFunc',&
                        '2Y_1^0+3Y_2^1 Energy error too large') 
  endif
  call MessageNotify('M','Test of n_EnergyFromStreamFunc', &
       'Test of 2Y_1^0+3Y_2^1 succeeded!') 

  data = AvrLonLat_xy(xy_w(w_xv(0.5*(xv_GradLon_w(w_data)**2+xv_GradLat_w(w_data)**2) )))
  error = abs(sum(n_EnergyFromStreamfunc_w(w_data))-data)
  if ( error > eps ) then
     call MessageNotify('E','Test of total energy',&
                        '2Y_1^0+3Y_2^1 total energy error too large') 
  endif
  call MessageNotify('M','Test of total energy', &
       'Test of 2Y_1^0+3Y_2^1 succeeded!') 

  !--------- Enstrophy spectrum ---------
  nm_data=w_spectrum_VMiss
  do n=0,nm
     do m=-n,n
        nm_data(n,m) = 0.0
     enddo
  enddo
  nm_data(1,0) = 8.0D0 ;   nm_data(2,1) = 162.0D0
  error = maxval(abs(nm_EnstrophyFromStreamfunc_w(w_data)-nm_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of nm_EnstrophyFromStreamFunc',&
                        '2Y_1^0+3Y_2^1 Enstrophy error too large') 
  endif
  call MessageNotify('M','Test of nm_EnstrophyFromStreamFunc', &
       'Test of 2Y_1^0+3Y_2^1 succeeded!') 

  n_data = 0.0; n_data(1) = 8.0D0 ;   n_data(2) = 162.0D0
  error = maxval(abs(n_EnstrophyFromStreamfunc_w(w_data)-n_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of n_EnstrophyFromStreamFunc',&
                        '2Y_1^0+3Y_2^1 Enstrophy error too large') 
  endif
  call MessageNotify('M','Test of n_EnstrophyFromStreamFunc', &
       'Test of 2Y_1^0+3Y_2^1 succeeded!') 

  data = AvrLonLat_xy( 0.5*(xy_w(w_Lapla_w(w_data))**2))
  error = abs(sum(n_EnstrophyFromStreamfunc_w(w_data))-data)
  if ( error > eps ) then
     call MessageNotify('E','Test of total enstrophy',&
                        '2Y_1^0+3Y_2^1 total enstrophy error too large') 
  endif
  call MessageNotify('M','Test of total enstrophy', &
       'Test of 2Y_1^0+3Y_2^1 succeeded!') 


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
  error = maxval(abs(nm_EnergyFromStreamfunc_w(w_data)-nm_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of nm_EnergyFromStreamFunc',&
                        '3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2 Energy error too large') 
  endif
  call MessageNotify('M','Test of nm_EnergyFromStreamFunc', &
       'Test of 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2 succeeded!') 

  n_data = 0.0; n_data(1) = 13.0D0 ;   n_data(2) = 15.0D0
  error = maxval(abs(n_EnergyFromStreamfunc_w(w_data)-n_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of n_EnergyFromStreamFunc',&
             '3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2 Energy error too large') 
  endif
  call MessageNotify('M','Test of n_EnergyFromStreamFunc', &
       'Test of 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2 succeeded!') 

  data = AvrLonLat_xy(xy_w(w_xv(0.5*(xv_GradLon_w(w_data)**2+xv_GradLat_w(w_data)**2) )))
  error = abs(sum(n_EnergyFromStreamfunc_w(w_data))-data)
  if ( error > eps ) then
     call MessageNotify('E','Test of total energy',&
             '3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2 total energy error too large') 
  endif
  call MessageNotify('M','Test of total energy', &
       'Test of 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2 succeeded!') 

  !--------- Enstrophy spectrum ---------
  nm_data=w_spectrum_VMiss
  do n=0,nm
     do m=-n,n
        nm_data(n,m) = 0.0
     enddo
  enddo
  nm_data(1,0) = 18.0D0 ; nm_data(1,1)  = 8.0D0
  nm_data(2,0) = 72.0D0 ; nm_data(2,-2) = 18.0D0
  error = maxval(abs(nm_EnstrophyFromStreamfunc_w(w_data)-nm_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of nm_EnstrophyFromStreamFunc',&
          '3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2 Enstrophy error too large') 
  endif
  call MessageNotify('M','Test of nm_EnstrophyFromStreamFunc', &
       'Test of 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2 succeeded!') 

  n_data = 0.0; n_data(1) = 26.0D0 ;   n_data(2) = 90.0D0
  error = maxval(abs(n_EnstrophyFromStreamfunc_w(w_data)-n_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of n_EnstrophyFromStreamFunc',&
          '3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2 Enstrophy error too large') 
  endif
  call MessageNotify('M','Test of n_EnstrophyFromStreamFunc', &
       'Test of 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2 succeeded!') 

  data = AvrLonLat_xy( 0.5*(xy_w(w_Lapla_w(w_data))**2))
  error = abs(sum(n_EnstrophyFromStreamfunc_w(w_data))-data)
  if ( error > eps ) then
     call MessageNotify('E','Test of total enstrophy',&
          '3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2 total enstrophy error too large') 
  endif
  call MessageNotify('M','Test of total enstrophy', &
       'Test of 3Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2 succeeded!') 

  call MessageNotify('M','w_test_spectrum', &
                     'w_spectrum_module tests succeeded!') 

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)      

end program w_spectrum_mpi_test
