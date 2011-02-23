!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_base_module テストプログラム :: 基本変換関数のテスト
!
!履歴  2007/11/01  竹広真一
!
program wa_base_mpi_test

  use dc_message, only : MessageNotify
  use wa_mpi_module
  implicit none
  include 'mpif.h'

  integer, parameter :: im=32, jm=16, nm=10, km=3

  real(8), allocatable ::  wa_data(:,:)
  real(8), allocatable ::  wa_data2(:,:)
  real(8), allocatable ::  xva_data(:,:,:)
  real(8), parameter   ::  eps = 1.0D-10
  real(8) :: pi
  integer :: iproc, np, ierr

  integer :: n, k

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','wa_base_mpi_test', &
                         'wa_base_mpi_module functions tests') 

  call wa_mpi_Initial( nm, im, jm, km )

  allocate(wa_data((nm+1)**2,km))
  allocate(xva_data(0:im-1,jc,km))


  !---- Y_1^* のテスト ----
  xva_data(:,:,1) = sqrt(3.0D0)*sin(xv_Lat)                   ! Y_1^0
  wa_data= 0.0D0 ; wa_data(l_nm(1,0),1)=1.0D0
  xva_data(:,:,2) = sqrt(3.0D0/2)*cos(xv_Lat)*cos(xv_Lon)     ! Y_1^1
  wa_data(l_nm(1,1),2)=1.0D0/sqrt(2.0D0)
  xva_data(:,:,3) = -sqrt(3.0D0/2)*cos(xv_Lat)*sin(xv_Lon)    ! Y_1^{-1}
  wa_data(l_nm(1,-1),3)=1.0D0/sqrt(2.0D0)

  if ( maxval(abs(wa_xva(xva_data)-wa_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^*','Spectral transform error too large') 
  endif
  if ( maxval(abs(xva_wa(wa_xva(xva_data))-xva_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^*',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_1^*','Test of Y_1^* succeeded!') 

  !---- Y_2^* のテスト ----
  xva_data(:,:,1) = sqrt(5.0D0)*(3.0/2*sin(xv_Lat)**2-1/2.0)           ! Y_2^0
  wa_data= 0.0D0 ; wa_data(l_nm(2,0),1)=1.0D0
  xva_data(:,:,2) = sqrt(5.0D0/6)*3.0*sin(xv_Lat)*cos(xv_Lat)*cos(xv_Lon)!Y_2^1
  wa_data(l_nm(2,1),2)=1.0D0/sqrt(2.0D0)
  xva_data(:,:,3) = -sqrt(5.0D0/24)*3.0*cos(xv_Lat)**2*sin(2*xv_Lon)   ! Y_2^-2
  wa_data(l_nm(2,-2),3)=1.0D0/sqrt(2.0D0)

  if ( maxval(abs(wa_xva(xva_data)-wa_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^*',&
                         'Spectral transform error too large') 
  endif
  if ( maxval(abs(xva_wa(wa_xva(xva_data))-xva_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^*',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_2^*','Test of Y_2^* succeeded!') 

  !---- 一般的関数のテスト ----
  xva_data(:,:,1) = cos(2*xv_Lon-pi/3) &
       *(sin(xv_Lat)-1)**2*(sin(xv_Lat)-0.5)*(sin(xv_Lat)+1)
  xva_data(:,:,2) = sin(2*xv_Lon+pi/2) &
       *(sin(xv_Lat)-1)**2*(sin(xv_Lat)+1)**2
  xva_data(:,:,3) = cos(2*xv_Lon+pi/3) &
       *(sin(xv_Lat)-1)*(sin(xv_Lat)-0.5)**2*(sin(xv_Lat)+1)

  if ( maxval(abs(xva_wa(wa_xva(xva_data))-xva_data)) > eps ) then
     write(6,*) maxval(abs(xva_wa(wa_xva(xva_data))-xva_data))
     call MessageNotify('E','Test of general function',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of genral function', &
       'Test of general function succeeded!') 

  call MessageNotify('M','wa_base_mpi_test', &
                         'wa_base_mpi_module functions tests succeeded!') 

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)

end program wa_base_mpi_test
