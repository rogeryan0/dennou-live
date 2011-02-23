!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wa_interpolate_module テストプログラム :: 補間関数のテスト
!
!履歴  2008/05/29  竹広真一
!
program wa_interpolate01_mpi_test

  use dc_message, only : MessageNotify
  use wa_mpi_module
  implicit none
  include 'mpif.h'

  integer, parameter :: im=16, jm=8, nm=10, km=3

  real(8), dimension((nm+1)**2,km) ::  wa_data
  real(8), dimension(im,jm,km)     ::  xya_data

  real(8), parameter            ::  eps = 1.0D-10

  real(8) :: Alon
  real(8) :: Alat(2)
  real(8), dimension(2,km) :: a_data_sol
  real(8) :: error
  real(8) :: pi
  integer :: iproc, np, ierr

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  pi = atan(1.0D0)*4.0D0
  Alon = pi/5 ; Alat = (/pi/3,-pi/4/)

  call wa_mpi_Initial( nm, im, jm, km )

  call MessageNotify('M','wa_Interpolate01_mpi_test', &
                         'wa_interpolate_module interpolate(01) tests') 


  !---- Y_1^* のテスト ----
  xya_data(:,:,1) = sqrt(3.0D0)*sin(xy_Lat)                      ! Y_1^0
  a_data_sol(:,1) = sqrt(3.0D0)*sin(ALat)

  xya_data(:,:,2) = sqrt(3.0D0/2)*cos(xy_Lat)*cos(xy_Lon)          ! Y_1^1
  a_data_sol(:,2) = sqrt(3.0D0/2)*cos(ALat)*cos(ALon)

  xya_data(:,:,3)   = -sqrt(3.0D0)*cos(xy_Lat)*sin(xy_Lon)         ! Y_1^{-1}
  a_data_sol(:,3) = -sqrt(3.0D0)*cos(ALat)*sin(ALon)

  wa_data = wa_xya(xya_data)
  error = maxval(abs(a_Interpolate_wa(wa_data,alon,alat)-a_data_sol))
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of Y_1^*','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of Y_1^*', &
       'Interpolate test of Y_1^* succeeded!') 

  !---- Y_2^* のテスト ----
  xya_data(:,:,1) = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)            ! Y_2^0
  a_data_sol(:,1) = sqrt(5.0D0)*(3.0/2*sin(alat)**2-1.0/2)
  xya_data(:,:,2) = sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)!Y_2^1
  a_data_sol(:,2) = sqrt(5.0D0/6)*3.0*sin(ALat)*cos(ALat) * cos(ALon)
  xya_data(:,:,3) = -sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*sin(2*xy_Lon)    ! Y_2^-2
  a_data_sol(:,3) = -sqrt(5.0D0/24)*3.0*cos(ALat)**2*sin(2*ALon)

  wa_data = wa_xya(xya_data)
  error = maxval(abs(a_Interpolate_wa(wa_data,alon,alat)-a_data_sol))
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of Y_2^*','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of Y_2^*', &
       'Interpolate test of Y_2^* succeeded!') 


  !---- 一般的なのテスト ----
  xya_data(:,:,1) = cos(2*xy_Lon-pi/3) &
       *(sin(xy_Lat)-1)**2*(sin(xy_Lat)-0.5)*(sin(xy_Lat)+1)
  a_data_sol(:,1) = cos(2*ALon-pi/3) &
       *(sin(ALat)-1)**2*(sin(ALat)-0.5)*(sin(ALat)+1)
  xya_data(:,:,2) = sin(2*xy_Lon-pi/3)*(sin(xy_Lat)-1)**2*(sin(xy_Lat)+1)**2
  a_data_sol(:,2) = sin(2*aLon-pi/3)*(sin(aLat)-1)**2*(sin(aLat)+1)**2
  xya_data(:,:,3) = sin(2*xy_Lon+pi/3)*(sin(xy_Lat)-1)**3*(sin(xy_Lat)+1)
  a_data_sol(:,3) = sin(2*aLon+pi/3)*(sin(aLat)-1)**3*(sin(aLat)+1)

  wa_data = wa_xya(xya_data)
  error = maxval(abs(a_Interpolate_wa(wa_data,alon,alat)-a_data_sol))
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of general func.','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of general func.', &
       'Interpolate test of general func succeeded!') 

  call MessageNotify('M','wa_Interpolate01_mpi_test', &
                      'wa_interpolate_module interpolate(01) tests succeeded') 

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)

end program wa_interpolate01_mpi_test
