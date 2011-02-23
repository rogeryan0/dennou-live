!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  l_module テストプログラム :: 微分関数のテスト
!
!履歴  2008/12/23  竹広真一
!
program l_test_deriv

  use dc_message, only : MessageNotify
  use l_module
  implicit none

  integer, parameter :: jm=16, nm=10

  real(8), dimension(1:jm)  ::  y_data1              ! 元の関数
  real(8), dimension(1:jm)  ::  y_ddata              ! 微分の正解
  real(8), dimension(1:jm)  ::  mu                   ! μ=sinφ
  real(8), parameter        ::  eps = 1.0D-10

  call MessageNotify('M','l_test_deriv', &
                         'l_module tests of derivative functions') 

  call l_Initial( nm, jm )

  !============== 微分計算 (φ座標系用) のテスト ==============

  !---- P_1 のテスト ----
  y_data1 = sqrt(3.0D0)*sin(y_Lat)         ! P_1

  y_ddata = -2*sqrt(3.0D0)*sin(y_Lat)      ! l_Lapla_l
  if ( maxval(abs(y_l(l_Lapla_l(l_y(y_data1)))-y_ddata)) > eps ) then
     call MessageNotify('E','Test of l_Lapla_l',&
                        'P_1 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of l_Lapla_l', &
                         'Test of Laplacian P_1 succeeded!') 

  y_ddata = -0.5*sqrt(3.0D0)*sin(y_Lat)      ! l_Lapla_l
  if ( maxval(abs(y_l(l_LaplaInv_l(l_y(y_data1)))-y_ddata)) > eps ) then
     call MessageNotify('E','Test of l_LaplaInv_l',&
                        'P_1 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of l_LaplaInv_l', &
                         'Test of Inverse Laplacian P_1 succeeded!') 

  y_ddata = sqrt(3.0D0)*cos(y_Lat)         ! y_GradLat_l
  if ( maxval(abs(y_GradLat_l(l_y(y_data1))-y_ddata)) > eps ) then
     write(6,*) maxval(abs(y_GradLat_l(l_y(y_data1))-y_ddata))
     call MessageNotify('E','Test of y_GradLat_l',&
                        'P_1 Latitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of y_GradLat_l', &
                         'Test of GradLat P_1 succeeded!') 

  !---- P_1 cosφ のテスト ----
  y_data1 = sqrt(3.0D0)*sin(y_Lat)*cos(y_Lat)               ! P_1

  y_ddata = sqrt(3.0D0)*(cos(y_Lat)**2-2*sin(y_Lat)**2)     ! p_DivLat_y
  if ( maxval(abs(y_l(l_DivLat_y(y_data1))-y_ddata)) > eps ) then
     call MessageNotify('E','Test of l_DivLat_y',&
                        'P_1 cos phi Latitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of l_DivLat_y', &
                         'Test of DivLat P_1 cos phi succeeded!') 

  !---- P_2 のテスト ----
  y_data1 = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)       ! P_2

  y_ddata = -6*y_data1                                    ! l_Lapla_l
  if ( maxval(abs(y_l(l_Lapla_l(l_y(y_data1)))-y_ddata)) > eps ) then
     call MessageNotify('E','Test of l_Lapla_l',&
                        'P_2 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of l_Lapla_l', &
                         'Test of Laplacian P_2 succeeded!') 

  y_ddata = -y_data1/6.0D0                                ! l_Lapla_l
  if ( maxval(abs(y_l(l_LaplaInv_l(l_y(y_data1)))-y_ddata)) > eps ) then
     call MessageNotify('E','Test of l_LaplaInv_l',&
                        'P_2 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of l_LaplaInv_l', &
                         'Test of Inverse Laplacian P_2 succeeded!') 

  y_ddata = sqrt(5.0D0)*3.0*sin(y_Lat)*cos(y_Lat)         ! y_GradLat_l
  if ( maxval(abs(y_GradLat_l(l_y(y_data1))-y_ddata)) > eps ) then
     write(6,*) maxval(abs(y_GradLat_l(l_y(y_data1))-y_ddata))
     call MessageNotify('E','Test of y_GradLat_l',&
                        'P_2 Latitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of y_GradLat_l', &
                         'Test of GradLat P_2 succeeded!') 

  !---- P_2 cosφ のテスト ----
  y_data1 = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)*cos(y_Lat)    ! P_2 cosφ

  y_ddata = sqrt(5.0D0)*(3.0*(sin(y_Lat)*cos(y_Lat)**2-sin(y_Lat)**3) &
                         +sin(y_Lat))                             ! l_DivLat_y

  if ( maxval(abs(y_l(l_DivLat_y(y_data1))-y_ddata)) > eps ) then
     call MessageNotify('E','Test of l_DivLat_y',&
                        'P_2 cos phi Latitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of l_DivLat_y', &
                         'Test of DivLat P_2 cos phi succeeded!') 

  call MessageNotify('M','l_test_deriv', &
                         'l_module tests of derivative functions succeeded.') 

  !============== 微分計算 (μ座標系用) のテスト ==============
  mu = sin(y_Lat)

  !----- P_2 のテスト -----
  y_data1 = 3*mu**2-1                              ! P_2
  y_ddata = 6*mu*(1-mu**2)                         ! (1-μ^2)∂/∂μ 

  if ( maxval(abs(y_GradMu_l(l_y(y_data1)) - y_ddata)) > eps ) then
     call MessageNotify('E','Test of GradMu',&
                        'P_2 y_GradMu_l error too large') 
  endif
  call MessageNotify('M','Test of y_GradMu_l', &
                         'Test of y_GradMu_l P_2 succeeded!') 

  !----- Y_2^0 (1-μ^2) のテスト -----
  y_data1 = (3*mu**2-1)*(1-mu**2)                ! P_2 (1-μ^2)
  y_ddata = (2-3*mu**2)*4*mu                     ! ∂/∂μ

  if ( maxval(abs(y_l(l_DivMu_y(y_data1)) - y_ddata)) > eps ) then
     call MessageNotify('E','Test of DivMu',&
                        'P_2 (1-μ^2) y_DivMu_l error too large') 
  endif
  call MessageNotify('M','Test of y_DivMu_l', &
                         'Test of y_DivMu_l P_2 (1-μ^2)succeeded!') 


  call MessageNotify('M','w_test_deriv', &
                         'w_deriv_module function tests succeeded!') 

end program l_test_deriv
