!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_mpi_module テストプログラム :: 基本変換関数のテスト
!
!履歴  2008/05/30  竹広真一
!
program wt_mpi_base_test

  use dc_message, only : MessageNotify
  use wt_mpi_module
  implicit none
  include 'mpif.h'

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径

  real(8), allocatable ::  wt_data(:,:)
  real(8), allocatable ::  xvz_data(:,:,:)
  real(8), allocatable ::  xvz_xi(:,:,:)
  real(8), parameter                 ::  eps = 1.0D-10
  real(8) :: pi
  integer :: iproc, np, ierr

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','wt_mpi_base_test', &
                         'wt_mpi_module basic transformation functions tests') 

  call wt_mpi_Initial(im,jm,km,nm,lm,ri,ro)

  allocate(wt_data((nm+1)**2,0:lm))
  allocate(xvz_data(0:im-1,jc,0:km))
  allocate(xvz_xi(0:im-1,jc,0:km))

  xvz_xi = (xvz_Rad - (ro+ri)/2 )*2/(ro-ri)

  !---- Y_1^* のテスト ----
  xvz_data = sqrt(3.0D0)*sin(xvz_Lat)*xvz_xi            ! Y_1^0 T_1
  wt_data= 0.0D0 ; wt_data(l_nm(1,0),1)=1.0D0

  if ( maxval(abs(wt_xvz(xvz_data)-wt_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^0 T_1', &
          'Spectral transform error too large') 
  endif
  if ( maxval(abs(xvz_wt(wt_xvz(xvz_data))-xvz_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^0 T_1',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_1^0 T_1', &
                     'transformation tests succeeded!') 

  xvz_data = sqrt(3.0D0/2)*cos(xvz_Lat)*cos(xvz_Lon)     ! Y_1^1 T_0
  wt_data= 0.0D0 ;  wt_data(l_nm(1,1),0)=1.0D0/sqrt(2.0D0)*2
  if ( maxval(abs(wt_xvz(xvz_data)-wt_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^1 T_0', &
          'Spectral transform error too large') 
  endif
  if ( maxval(abs(xvz_wt(wt_xvz(xvz_data))-xvz_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^1 T_0',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_1^1 T_0', &
                     'transformation tests succeeded!') 


  xvz_data = -sqrt(3.0D0/2)*cos(xvz_Lat)*sin(xvz_Lon)*(2*xvz_xi**2-1) !Y_1^{-1}T_2
  wt_data= 0.0D0 ;  wt_data(l_nm(1,-1),2)=1.0D0/sqrt(2.0D0)
  if ( maxval(abs(wt_xvz(xvz_data)-wt_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^-1 T_2', &
          'Spectral transform error too large') 
  endif
  if ( maxval(abs(xvz_wt(wt_xvz(xvz_data))-xvz_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^-1 T_2',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_1^-1 T_2', &
                     'transformation tests succeeded!') 

  !---- Y_2^* のテスト ----
  ! Y_2^0 T_3
  xvz_data = sqrt(5.0D0)*(3.0/2*sin(xvz_Lat)**2-1/2.0)*(4*xvz_xi**3-3*xvz_xi) 
  wt_data= 0.0D0 ; wt_data(l_nm(2,0),3)=1.0D0
  if ( maxval(abs(wt_xvz(xvz_data)-wt_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^0 T_3', &
          'Spectral transform error too large') 
  endif
  if ( maxval(abs(xvz_wt(wt_xvz(xvz_data))-xvz_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^0 T_3',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_2^0 T_3', &
                     'transformation tests succeeded!') 

  !Y_2^1 T_4
  xvz_data = sqrt(5.0D0/6)*3.0*sin(xvz_Lat)*cos(xvz_Lat)*cos(xvz_Lon) &
            *(8*xvz_xi**4 - 8*xvz_xi**2 + 1 )
  wt_data= 0.0D0 ; wt_data(l_nm(2,1),4)=1.0D0/sqrt(2.0D0)
  if ( maxval(abs(wt_xvz(xvz_data)-wt_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^1 T_4', &
          'Spectral transform error too large') 
  endif
  if ( maxval(abs(xvz_wt(wt_xvz(xvz_data))-xvz_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^1 T_4',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_2^1 T_4', &
                     'transformation tests succeeded!') 

  ! Y_2^-2
  xvz_data = -sqrt(5.0D0/24)*3.0*cos(xvz_Lat)**2*sin(2*xvz_Lon) &
            *(16*xvz_xi**5-20*xvz_xi**3+5*xvz_xi)
  wt_data= 0.0D0 ; wt_data(l_nm(2,-2),5)=1.0D0/sqrt(2.0D0)
  if ( maxval(abs(wt_xvz(xvz_data)-wt_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^-2 T_5', &
          'Spectral transform error too large') 
  endif
  if ( maxval(abs(xvz_wt(wt_xvz(xvz_data))-xvz_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^-2 T_5',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_2^-2 T_5', &
                     'transformation tests succeeded!') 

  !---- 一般的関数のテスト ----
  xvz_data = cos(2*xvz_Lon-pi/3) &
       *(sin(xvz_Lat)-1)**2*(sin(xvz_Lat)-0.5)*(sin(xvz_Lat)+1) &
       *exp(xvz_Rad)
  if ( maxval(abs(xvz_wt(wt_xvz(xvz_data))-xvz_data)) > eps ) then
     write(6,*) maxval(abs(xvz_wt(wt_xvz(xvz_data))-xvz_data))
     call MessageNotify('E','Test of general function',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of genral function', &
       'Test of general function succeeded!') 

  call MessageNotify('M','wt_base_mpi_test', &
                         'wt_mpi_base_module functions tests succeeded!') 

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)

end program wt_mpi_base_test
