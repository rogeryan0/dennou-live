!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_integrale_module テストプログラム :: 積分・平均関数のテスト
!
!履歴  2008/06/06  竹広真一
!
program w_integral_mpi_test

  use dc_message, only : MessageNotify
  use w_mpi_module
  implicit none
  include 'mpif.h'

  integer, parameter :: im=128, jm=64, nm=42

  real(8), dimension(0:im-1,jm) ::  xy_data            ! 元の関数
  real(8), dimension(0:im-1)    ::  x_idata            ! 積分・平均の正解
  real(8), dimension(jm)        ::  y_idata            ! 積分・平均の正解
  real(8)                       ::  idata              ! 積分・平均の正解

  real(8), allocatable          ::  xv_data(:,:)       ! 元の関数
  real(8), allocatable          ::  v_idata(:)         ! 積分・平均の正解
  real(8), parameter            ::  eps = 1.0D-10
  real(8) :: pi
  integer :: iproc, np, ierr

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','w_test_integral', &
                         'w_integral_module function tests') 

  call w_mpi_Initial( nm, im, jm )

  allocate(xv_data(0:im-1,jc))
  allocate(v_idata(jc))

  !---- 積分のテスト(全領域) ----
  xy_data = sin(xy_Lat)**2*(1-cos(xy_Lon)**2)

  idata = 2*pi/3       ! IntLonLat_xy
  if ( abs(IntLonLat_xy(xy_data)-idata) > eps ) then
     call MessageNotify('E','Test of IntLonLat_xy','Error too large') 
  endif
  call MessageNotify('M','Test of IntLonLat_xy', 'Test succeeded!') 

  y_idata = pi*sin(y_Lat)**2       ! y_IntLon_xy
  if ( maxval(abs(y_IntLon_xy(xy_data)-y_idata)) > eps ) then
     call MessageNotify('E','Test of y_IntLon_xy','Error too large') 
  endif
  call MessageNotify('M','Test of y_IntLon_xy','Test succeeded!') 

  if ( abs(IntLat_y(y_idata)-idata) > eps ) then
     call MessageNotify('E','Test of IntLat_y','Error too large') 
  endif
  call MessageNotify('M','Test of IntLat_y','Test succeeded!') 

  x_idata = 2.0D0/3*(1-cos(x_Lon)**2)       ! x_IntLat_xy
  if ( maxval(abs(x_IntLat_xy(xy_data)-x_idata)) > eps ) then
     call MessageNotify('E','Test of x_IntLat_xy','Error too large') 
  endif
  call MessageNotify('M','Test of x_Intat_xy','Test succeeded!') 

  if ( abs(IntLon_x(x_idata)-idata) > eps ) then
     call MessageNotify('E','Test of IntLon_x','Error too large') 
  endif
  call MessageNotify('M','Test of IntLon_x','Test succeeded!') 

  !---- 平均のテスト(全領域) ----
  xy_data = sin(xy_Lat)**2*(1-cos(xy_Lon)**2)

  idata = 1.0D0/6       ! AvrLonLat_xy
  if ( abs(AvrLonLat_xy(xy_data)-idata) > eps ) then
     call MessageNotify('E','Test of AvrLonLat_xy','Error too large') 
  endif
  call MessageNotify('M','Test of AvrLonLat_xy', 'Test succeeded!') 

  y_idata = 0.5*sin(y_Lat)**2       ! y_AvrLon_xy
  if ( maxval(abs(y_AvrLon_xy(xy_data)-y_idata)) > eps ) then
     call MessageNotify('E','Test of y_AvrLon_xy','Error too large') 
  endif
  call MessageNotify('M','Test of y_AvrLon_xy','Test succeeded!') 

  if ( abs(AvrLat_y(y_idata)-idata) > eps ) then
     call MessageNotify('E','Test of AvrLat_y','Error too large') 
  endif
  call MessageNotify('M','Test of AvrLat_y','Test succeeded!') 

  x_idata = 1.0D0/3*(1-cos(x_Lon)**2)       ! x_AvrLat_xy
  if ( maxval(abs(x_AvrLat_xy(xy_data)-x_idata)) > eps ) then
     call MessageNotify('E','Test of x_AvrLat_xy','Error too large') 
  endif
  call MessageNotify('M','Test of x_Avrat_xy','Test succeeded!') 

  if ( abs(AvrLon_x(x_idata)-idata) > eps ) then
     call MessageNotify('E','Test of AvrLon_x','Error too large') 
  endif
  call MessageNotify('M','Test of AvrLon_x','Test succeeded!') 


  call MessageNotify('M','w_test_integral', &
                         'w_integral_module function tests succeded!') 

  !---- 積分のテスト(分割領域) ----
  xv_data = sin(xv_Lat)**2*(1-cos(xv_Lon)**2)

  idata = 2*pi/3       ! IntLonLat_xv
  if ( abs(IntLonLat_xv(xv_data)-idata) > eps ) then
     call MessageNotify('E','Test of IntLonLat_xv','Error too large') 
  endif
  call MessageNotify('M','Test of IntLonLat_xv', 'Test succeeded!') 

  v_idata = pi*sin(v_Lat)**2       ! v_IntLon_xv
  if ( maxval(abs(v_IntLon_xv(xv_data)-v_idata)) > eps ) then
     call MessageNotify('E','Test of v_IntLon_xv','Error too large') 
  endif
  call MessageNotify('M','Test of v_IntLon_xv','Test succeeded!') 

  if ( abs(IntLat_v(v_idata)-idata) > eps ) then
     call MessageNotify('E','Test of IntLat_v','Error too large') 
  endif
  call MessageNotify('M','Test of IntLat_v','Test succeeded!') 

  x_idata = 2.0D0/3*(1-cos(x_Lon)**2)       ! x_IntLat_xv
  if ( maxval(abs(x_IntLat_xv(xv_data)-x_idata)) > eps ) then
     call MessageNotify('E','Test of x_IntLat_xv','Error too large') 
  endif
  call MessageNotify('M','Test of x_Intat_xv','Test succeeded!') 

  !---- 平均のテスト(分割領域) ----
  xv_data = sin(xv_Lat)**2*(1-cos(xv_Lon)**2)

  idata = 1.0D0/6       ! AvrLonLat_xv
  if ( abs(AvrLonLat_xv(xv_data)-idata) > eps ) then
     call MessageNotify('E','Test of AvrLonLat_xv','Error too large') 
  endif
  call MessageNotify('M','Test of AvrLonLat_xv', 'Test succeeded!') 

  v_idata = 0.5*sin(v_Lat)**2       ! v_AvrLon_xv
  if ( maxval(abs(v_AvrLon_xv(xv_data)-v_idata)) > eps ) then
     call MessageNotify('E','Test of v_AvrLon_xv','Error too large') 
  endif
  call MessageNotify('M','Test of v_AvrLon_xv','Test succeeded!') 

  if ( abs(AvrLat_v(v_idata)-idata) > eps ) then
     call MessageNotify('E','Test of AvrLat_v','Error too large') 
  endif
  call MessageNotify('M','Test of AvrLat_v','Test succeeded!') 

  x_idata = 1.0D0/3*(1-cos(x_Lon)**2)       ! x_AvrLat_xv
  if ( maxval(abs(x_AvrLat_xv(xv_data)-x_idata)) > eps ) then
     call MessageNotify('E','Test of x_AvrLat_xv','Error too large') 
  endif
  call MessageNotify('M','Test of x_Avrat_xv','Test succeeded!') 

  call MessageNotify('M','w_test_integral', &
                         'w_integral_module function tests succeded!') 

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)      

end program w_integral_mpi_test
