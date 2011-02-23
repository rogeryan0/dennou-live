!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_integrale_module テストプログラム :: 積分・平均関数のテスト
!
!履歴  2008/05/29  竹広真一
!
program wa_integral_mpi_test

  use dc_message, only : MessageNotify
  use wa_mpi_module
  implicit none
  include 'mpif.h'

  integer, parameter :: im=128, jm=64, nm=42, km=2

  real(8), dimension(0:im-1,jm,km) ::  xya_data            ! 元の関数
  real(8), dimension(im,km)        ::  xa_idata            ! 積分・平均の正解
  real(8), dimension(jm,km)        ::  ya_idata            ! 積分・平均の正解
  real(8), dimension(km)           ::  a_idata             ! 積分・平均の正解
  real(8), allocatable             ::  xva_data(:,:,:)     ! 元の関数
  real(8), allocatable             ::  va_idata(:,:)       ! 積分・平均の正解
  real(8), parameter               ::  eps = 1.0D-10
  real(8) :: pi
  integer :: iproc, np, ierr

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','wa_integral_mpi_test', &
                         'wa_integral_module function tests') 

  call wa_mpi_Initial( nm, im, jm, km )

  allocate(xva_data(0:im-1,jc,km))
  allocate(va_idata(jc,km))

  !---- 積分のテスト(全領域) ----
  xya_data(:,:,1) = sin(xy_Lat)**2*(1-cos(xy_Lon)**2)
  xya_data(:,:,2) = sin(xy_Lat)**4*(1-sin(xy_Lon)**2)

  a_idata(1) = 2*pi/3       ! a_IntLonLat_xya
  a_idata(2) = 2*pi/5       ! a_IntLonLat_xya
  if ( maxval(abs(a_IntLonLat_xya(xya_data)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_IntLonLat_xya','Error too large') 
  endif
  call MessageNotify('M','Test of a_IntLonLat_xya', 'Test succeeded!') 

  ya_idata(:,1) = pi*sin(y_Lat)**2       ! y_IntLon_xya
  ya_idata(:,2) = pi*sin(y_Lat)**4       ! y_IntLon_xya
  if ( maxval(abs(ya_IntLon_xya(xya_data)-ya_idata)) > eps ) then
     call MessageNotify('E','Test of ya_IntLon_xya','Error too large') 
  endif
  call MessageNotify('M','Test of ya_IntLon_xya','Test succeeded!') 

  if ( maxval(abs(a_IntLat_ya(ya_idata)-a_idata)) > eps ) then
     call MessageNotify('E','Test of IntLat_y','Error too large') 
  endif
  call MessageNotify('M','Test of a_IntLat_ya','Test succeeded!') 

  xa_idata(:,1) = 2.0D0/3*(1-cos(x_Lon)**2)       ! x_IntLat_xya
  xa_idata(:,2) = 2.0D0/5*(1-sin(x_Lon)**2)       ! x_IntLat_xya
  if ( maxval(abs(xa_IntLat_xya(xya_data)-xa_idata)) > eps ) then
     call MessageNotify('E','Test of x_IntLat_xya','Error too large') 
  endif
  call MessageNotify('M','Test of x_Intat_xya','Test succeeded!') 

  if ( maxval(abs(a_IntLon_xa(xa_idata)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_IntLon_xa','Error too large') 
  endif
  call MessageNotify('M','Test of a_IntLon_xa','Test succeeded!') 

  !---- 平均のテスト(全領域) ----
  xya_data(:,:,1) = sin(xy_Lat)**2*(1-cos(xy_Lon)**2)
  xya_data(:,:,2) = sin(xy_Lat)**4*(1-sin(xy_Lon)**2)

  a_idata(1) = 1.0D0/6       ! AvrLonLat_xya
  a_idata(2) = 1.0D0/10      ! AvrLonLat_xya
  if ( maxval(abs(a_AvrLonLat_xya(xya_data)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_AvrLonLat_xya','Error too large') 
  endif
  call MessageNotify('M','Test of a_AvrLonLat_xya', 'Test succeeded!') 

  ya_idata(:,1) = 0.5*sin(y_Lat)**2       ! y_AvrLon_xya
  ya_idata(:,2) = 0.5*sin(y_Lat)**4       ! y_AvrLon_xya
  if ( maxval(abs(ya_AvrLon_xya(xya_data)-ya_idata)) > eps ) then
     call MessageNotify('E','Test of y_AvrLon_xya','Error too large') 
  endif
  call MessageNotify('M','Test of y_AvrLon_xya','Test succeeded!') 

  if ( maxval(abs(a_AvrLat_ya(ya_idata)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_AvrLat_ya','Error too large') 
  endif
  call MessageNotify('M','Test of a_AvrLat_ya','Test succeeded!') 

  xa_idata(:,1) = 1.0D0/3*(1-cos(x_Lon)**2)       ! x_AvrLat_xya
  xa_idata(:,2) = 1.0D0/5*(1-sin(x_Lon)**2)       ! x_AvrLat_xya
  if ( maxval(abs(xa_AvrLat_xya(xya_data)-xa_idata)) > eps ) then
     call MessageNotify('E','Test of xa_AvrLat_xya','Error too large') 
  endif
  call MessageNotify('M','Test of xaa_Avrat_xya','Test succeeded!') 

  if ( maxval(abs(a_AvrLon_xa(xa_idata)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_AvrLon_xa','Error too large') 
  endif
  call MessageNotify('M','Test of a_AvrLon_xa','Test succeeded!') 

  !---- 積分のテスト(分割領域) ----
  xva_data(:,:,1) = sin(xv_Lat)**2*(1-cos(xv_Lon)**2)
  xva_data(:,:,2) = sin(xv_Lat)**4*(1-sin(xv_Lon)**2)

  a_idata(1) = 2*pi/3       ! a_IntLonLat_xva
  a_idata(2) = 2*pi/5       ! a_IntLonLat_xva
  if ( maxval(abs(a_IntLonLat_xva(xva_data)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_IntLonLat_xva','Error too large') 
  endif
  call MessageNotify('M','Test of a_IntLonLat_xva', 'Test succeeded!') 

  va_idata(:,1) = pi*sin(v_Lat)**2       ! v_IntLon_xva
  va_idata(:,2) = pi*sin(v_Lat)**4       ! v_IntLon_xva
  if ( maxval(abs(va_IntLon_xva(xva_data)-va_idata)) > eps ) then
     call MessageNotify('E','Test of va_IntLon_xva','Error too large') 
  endif
  call MessageNotify('M','Test of va_IntLon_xva','Test succeeded!') 

  if ( maxval(abs(a_IntLat_va(va_idata)-a_idata)) > eps ) then
     call MessageNotify('E','Test of IntLat_v','Error too large') 
  endif
  call MessageNotify('M','Test of a_IntLat_va','Test succeeded!') 

  xa_idata(:,1) = 2.0D0/3*(1-cos(x_Lon)**2)       ! x_IntLat_xva
  xa_idata(:,2) = 2.0D0/5*(1-sin(x_Lon)**2)       ! x_IntLat_xva
  if ( maxval(abs(xa_IntLat_xva(xva_data)-xa_idata)) > eps ) then
     call MessageNotify('E','Test of x_IntLat_xva','Error too large') 
  endif
  call MessageNotify('M','Test of x_Intat_xva','Test succeeded!') 

  !---- 平均のテスト(分割領域) ----
  xva_data(:,:,1) = sin(xv_Lat)**2*(1-cos(xv_Lon)**2)
  xva_data(:,:,2) = sin(xv_Lat)**4*(1-sin(xv_Lon)**2)

  a_idata(1) = 1.0D0/6       ! AvrLonLat_xva
  a_idata(2) = 1.0D0/10      ! AvrLonLat_xva
  if ( maxval(abs(a_AvrLonLat_xva(xva_data)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_AvrLonLat_xva','Error too large') 
  endif
  call MessageNotify('M','Test of a_AvrLonLat_xva', 'Test succeeded!') 

  va_idata(:,1) = 0.5*sin(v_Lat)**2       ! v_AvrLon_xva
  va_idata(:,2) = 0.5*sin(v_Lat)**4       ! v_AvrLon_xva
  if ( maxval(abs(va_AvrLon_xva(xva_data)-va_idata)) > eps ) then
     call MessageNotify('E','Test of v_AvrLon_xva','Error too large') 
  endif
  call MessageNotify('M','Test of v_AvrLon_xva','Test succeeded!') 

  if ( maxval(abs(a_AvrLat_va(va_idata)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_AvrLat_va','Error too large') 
  endif
  call MessageNotify('M','Test of a_AvrLat_va','Test succeeded!') 

  xa_idata(:,1) = 1.0D0/3*(1-cos(x_Lon)**2)       ! x_AvrLat_xva
  xa_idata(:,2) = 1.0D0/5*(1-sin(x_Lon)**2)       ! x_AvrLat_xva
  if ( maxval(abs(xa_AvrLat_xva(xva_data)-xa_idata)) > eps ) then
     call MessageNotify('E','Test of xa_AvrLat_xva','Error too large') 
  endif
  call MessageNotify('M','Test of xaa_Avrat_xva','Test succeeded!') 

  call MessageNotify('M','wa_integral_mpi_test', &
                         'wa_integral_module function tests succeded!') 

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)

end program wa_integral_mpi_test
