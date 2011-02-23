!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wa_spectrum_module テストプログラム :: 補間関数のテスト
!
!履歴  2008/05/29  竹広真一
!
program wa_spectrum_mpi_test

  use dc_message, only : MessageNotify
  use wa_mpi_module
  implicit none
  include 'mpif.h'

  integer, parameter :: im=16, jm=8, nm=10, km=2

  real(8), dimension((nm+1)**2,km)   ::  wa_data
  real(8), allocatable               ::  xva_data(:,:,:)

  real(8), dimension(0:nm,-nm:nm,km) ::  nma_data
  real(8), dimension(0:nm,km)        ::  na_data
  real(8), dimension(km)             ::  a_data

  real(8), parameter            ::  eps = 1.0D-10

  real(8) :: error
  integer :: n,m
  integer :: iproc, np, ierr

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call wa_mpi_Initial( nm, im, jm, km )

  call MessageNotify('M','wa_spectrum_mpi_test','wa_spectrum_module tests') 

  allocate(xva_data(0:im-1,jc,km))

  ! 2Y_1^0+3Y_2^1
  xva_data(:,:,1) = 2*sqrt(3.0D0)*sin(xv_Lat) &
                + 3*sqrt(2*5.0D0/6)*3.0*sin(xv_Lat)*cos(xv_Lat) * cos(xv_Lon)! 
  ! 3*Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2
  xva_data(:,:,2) = 3*sqrt(3.0D0)*sin(xv_Lat) & 
                + 2* sqrt(2*3.0D0/2)*cos(xv_Lat)*cos(xv_Lon) &
                + 2*sqrt(5.0D0)*(3.0/2*sin(xv_Lat)**2-1/2.0) &
                - sqrt(2*5.0D0/24)*3.0*cos(xv_Lat)**2*sin(2*xv_Lon)

  wa_data = wa_xva(xva_data)

  !--------- Energy spectrum ---------
  nma_data=wa_spectrum_VMiss
  do n=0,nm
     do m=-n,n
        nma_data(n,m,:) = 0.0
     enddo
  enddo

  nma_data(1,0,1) = 4.0D0 ;   nma_data(2,1,1) = 27.0D0

  nma_data(1,0,2) = 9.0D0  ; nma_data(1,1,2)  = 4.0D0
  nma_data(2,0,2) = 12.0D0 ; nma_data(2,-2,2) = 3.0D0

  error = maxval(abs(nma_EnergyFromStreamfunc_wa(wa_data)-nma_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of nma_EnergyFromStreamFunc',&
                        'Energy error too large') 
  endif
  call MessageNotify('M','Test of nma_EnergyFromStreamFunc','Test succeeded!') 

  na_data(:,1) = 0.0
  na_data(1,1) = 4.0D0 ;   na_data(2,1) = 27.0D0
  na_data(1,2) = 13.0D0 ;   na_data(2,2) = 15.0D0

  error = maxval(abs(na_EnergyFromStreamfunc_wa(wa_data)-na_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of na_EnergyFromStreamFunc',&
                        'Energy error too large') 
  endif
  call MessageNotify('M','Test of na_EnergyFromStreamFunc','Test succeeded!') 

  a_data = a_AvrLonLat_xya(xya_wa(wa_xva(0.5*(xva_GradLon_wa(wa_data)**2+xva_GradLat_wa(wa_data)**2) )))
  error = maxval(abs(sum(na_EnergyFromStreamfunc_wa(wa_data),1)-a_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of total energy',&
                        'total energy error too large') 
  endif
  call MessageNotify('M','Test of total energy','Test succeeded!') 

  !--------- Enstrophy spectrum ---------
  nma_data=wa_spectrum_VMiss
  do n=0,nm
     do m=-n,n
        nma_data(n,m,:) = 0.0
     enddo
  enddo

  nma_data(1,0,1) = 8.0D0 ;   nma_data(2,1,1) = 162.0D0

  nma_data(1,0,2) = 18.0D0 ; nma_data(1,1,2)  = 8.0D0
  nma_data(2,0,2) = 72.0D0 ; nma_data(2,-2,2) = 18.0D0

  error = maxval(abs(nma_EnstrophyFromStreamfunc_wa(wa_data)-nma_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of nma_EnstrophyFromStreamFunc',&
                        '2Y_1^0+3Y_2^1 Enstrophy error too large') 
  endif
  call MessageNotify('M','Test of nma_EnstrophyFromStreamFunc', &
       'Test of 2Y_1^0+3Y_2^1 succeeded!') 

  na_data = 0.0
  na_data(1,1) = 8.0D0  ;   na_data(2,1) = 162.0D0
  na_data(1,2) = 26.0D0 ;   na_data(2,2) = 90.0D0

  error = maxval(abs(na_EnstrophyFromStreamfunc_wa(wa_data)-na_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of na_EnstrophyFromStreamFunc',&
                        'Enstrophy error too large') 
  endif
  call MessageNotify('M','Test of na_EnstrophyFromStreamFunc','Test succeeded!') 

  a_data = a_AvrLonLat_xya( 0.5*(xya_wa(wa_Lapla_wa(wa_data))**2))
  error = maxval(abs(sum(na_EnstrophyFromStreamfunc_wa(wa_data),1)-a_data))
  if ( error > eps ) then
     call MessageNotify('E','Test of total enstrophy',&
                        'total enstrophy error too large') 
  endif
  call MessageNotify('M','Test of total enstrophy', 'Test succeeded!') 

  call MessageNotify('M','wa_spectrum_mpi_test', &
                     'wa_spectrum_module tests succeeded!') 

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)

end program wa_spectrum_mpi_test
