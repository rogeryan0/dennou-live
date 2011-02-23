!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_deriv_mpi_module テストプログラム :: 微分関数のテスト
!
!履歴  2008/05/26  竹広真一
!
program w_deriv_mpi_test

  use dc_message, only : MessageNotify
  use w_mpi_module
  implicit none
  include 'mpif.h'

  integer, parameter :: im=128, jm=64, nm=42

  real(8), allocatable  ::  xv_data1(:,:)              ! 元の関数
  real(8), allocatable  ::  xv_data2(:,:)              ! 元の関数
  real(8), allocatable  ::  xv_ddata(:,:)              ! 微分の正解
  real(8), allocatable  ::  mu(:,:)                    ! μ=sinφ
  real(8), parameter            ::  eps = 1.0D-10
  real(8)                       ::  pi
  integer :: iproc, np, ierr

  pi = atan(1.0D0)*4.0D0

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call MessageNotify('M','w_deriv_mpi_test', &
                         'w_deriv_mpi_module function tests') 

  call w_mpi_Initial( nm, im, jm )

  !------------ 変数割付け -------------
  allocate(xv_data1(0:im-1,jc))
  allocate(xv_data2(0:im-1,jc))
  allocate(xv_ddata(0:im-1,jc))
  allocate(mu(0:im-1,jc))

  !---- Y_1^{-1} のテスト ----
  xv_data1 = -cos(xv_Lat)*sin(xv_Lon)         ! Y_1^{-1}

  xv_ddata = 2*cos(xv_Lat)*sin(xv_Lon)       ! w_Lapla_w
  if ( maxval(abs(xv_w(w_Lapla_w(w_xv(xv_data1)))-xv_ddata)) > eps ) then
     call MessageNotify('E','Test of w_Lapla_w',&
                        'Y_1^-1 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of w_Lapla_w', &
                         'Test of Laplacian Y_1^-1 succeeded!') 

  xv_ddata = 1.0/2.0*cos(xv_Lat)*sin(xv_Lon)      ! w_LaplaInv_w
  if ( maxval(abs(xv_w(w_LaplaInv_w(w_xv(xv_data1)))-xv_ddata)) > eps ) then
     call MessageNotify('E','Test of w_LaplaInv_w',&
                        'Y_1^-1 Inverse Laplacian error too large') 
  endif
  call MessageNotify('M','Test of w_LaplaInv_w', &
                         'Test of Inverse Laplacian Y_1^-1 succeeded!') 

  xv_ddata = -cos(xv_Lat)*cos(xv_Lon)         ! w_DLon_w
  if ( maxval(abs(xv_w(w_DLon_w(w_xv(xv_data1)))-xv_ddata)) > eps ) then
     call MessageNotify('E','Test of w_DLon_w',&
                        'Y_1^-1 Longitudinal derivative error too large') 
  endif
  call MessageNotify('M','Test of w_DLon_w', &
                         'Test of DLon Y_1^-1 succeeded!') 

  xv_ddata = -cos(xv_Lon)                     ! xv_GradLon_w
  if ( maxval(abs(xv_GradLon_w(w_xv(xv_data1))-xv_ddata)) > eps ) then
     call MessageNotify('E','Test of xv_GradLon_w',&
                        'Y_1^-1 Longitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of xv_GradLon_w', &
                         'Test of GradLon Y_1^-1 succeeded!') 

  xv_ddata = sin(xv_Lat)*sin(xv_Lon)         ! xv_GradLat_w
  if ( maxval(abs(xv_GradLat_w(w_xv(xv_data1))-xv_ddata)) > eps ) then
     write(6,*) maxval(abs(xv_GradLat_w(w_xv(xv_data1))-xv_ddata))
     call MessageNotify('E','Test of xv_GradLat_w',&
                        'Y_1^-1 Latitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of xv_GradLat_w', &
                         'Test of GradLat Y_1^-1 succeeded!') 

  !---- Y_2^1 cosφ のテスト ----
  xv_data1 = sin(xv_Lat)*cos(xv_Lat)**2 * cos(xv_Lon) ! Y_2^1 cosφ

  xv_ddata = -sin(xv_Lat)*cos(xv_Lat)*sin(xv_Lon)    ! w_DivLon_xv
  if ( maxval(abs(xv_w(w_DivLon_xv(xv_data1))-xv_ddata)) > eps ) then
     call MessageNotify('E','Test of w_DivLon_xv',&
                        'Y_2^1 cosφ Longitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of xv_DivLon_w', &
                         'Test of DivLon Y_2^1 cosθ succeeded!') 

  xv_ddata = cos(xv_Lat)*(1-4*sin(xv_Lat)**2)*cos(xv_Lon)      ! w_DivLat_w
  if ( maxval(abs(xv_w(w_DivLat_xv(xv_data1))-xv_ddata)) > eps ) then
     call MessageNotify('E','Test of w_DivLat_xv',&
                        'Y_2^1 cosφ Latitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of w_DivLat_xv', &
                         'Test of DivLat Y_2^1 cosθ succeeded!') 

  !---- Jacobian のテスト ----
  xv_data1 = -cos(xv_Lat)*sin(xv_Lon)                ! Y_1^{-1}
  xv_data2 = sin(xv_Lat)*cos(xv_Lat) * cos(xv_Lon)   ! Y_2^1

  xv_ddata = 0.0
  if ( maxval(abs(xv_w(w_JacobianMPI_w_w(w_xv(xv_data1),w_xv(xv_data1))) &
                  -xv_ddata)) > eps ) then
     call MessageNotify('E','Test of w_JacobianMPI_w_w',&
                        'Y_1^-1 Jacobian error too large') 
  endif
  call MessageNotify('M','Test of w_Jacobian_w_w', &
                         'Test of w_Jacobian_w_w Y_1^-1 succeeded!') 

  xv_ddata = sin(xv_Lat)**2 - cos(xv_Lat)**2*cos(xv_Lon)**2
  if ( maxval(abs(xv_w(w_JacobianMPI_w_w(w_xv(xv_data1),w_xv(xv_data2))) &
                  -xv_ddata)) > eps ) then
     call MessageNotify('E','Test of w_JacobianMPI_w_w',&
                        'Y_1^-1 and Y_2^1 Jacobian error too large') 
  endif
  call MessageNotify('M','Test of w_JacobianMPI_w_w', &
                         'Test of w_JacobianMPI_w_w Y_1^-1 and Y_2^1 succeeded!') 

  !============== 微分計算 (λ,μ座標系用) のテスト ==============
  mu = sin(xv_Lat)

  !----- Y_2^0 のテスト -----
  xv_data1 = 3*mu**2-1                              ! Y_2^0

  xv_ddata = 0.0
  if ( maxval(abs(xv_GradLambda_w(w_xv(xv_data1)) -xv_ddata)) > eps ) then
     call MessageNotify('E','Test of GradLambda',&
                        'Y_2^0 xv_GradLambda_w error too large') 
  endif
  call MessageNotify('M','Test of xv_GradLambda_w', &
                         'Test of xv_GradLambda_w Y_2^0 succeeded!') 

  xv_ddata = 6*mu*(1-mu**2)
  if ( maxval(abs(xv_GradMu_w(w_xv(xv_data1)) -xv_ddata)) > eps ) then
     call MessageNotify('E','Test of GradMu',&
                        'Y_2^0 xv_GradMu_w error too large') 
  endif
  call MessageNotify('M','Test of xv_GradMu_w', &
                         'Test of xv_GradMu_w Y_2^0 succeeded!') 

  !----- Y_2^0 cosφ のテスト -----
  xv_data1 = (3*mu**2-1)*(1-mu**2)                ! Y_2^0 (1-μ^2)

  xv_ddata = 0.0
  if ( maxval(abs(xv_w(w_DivLambda_xv(xv_data1)) -xv_ddata)) > eps ) then
     call MessageNotify('E','Test of DivLambda',&
                        'Y_2^0 (1-μ^2) xv_DivLambda_w error too large') 
  endif
  call MessageNotify('M','Test of xv_DivLambda_w', &
                         'Test of xv_DivLambda_w Y_2^0 (1-μ^2) succeeded!') 

  xv_ddata = (2-3*mu**2)*4*mu
  if ( maxval(abs(xv_w(w_DivMu_xv(xv_data1)) -xv_ddata)) > eps ) then
     call MessageNotify('E','Test of DivMu',&
                        'Y_2^0 (1-μ^2) xv_DivMu_w error too large') 
  endif
  call MessageNotify('M','Test of xv_DivMu_w', &
                         'Test of xv_DivMu_w Y_2^0 (1-μ^2)succeeded!') 


  call MessageNotify('M','w_test_deriv', &
                         'w_deriv_module function tests succeeded!') 

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)      

end program w_deriv_mpi_test
