!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_zonal_module テストプログラム :: 微分関数のテスト
!
!履歴  2008/12/28  竹広真一
!
program w_zonal_test_deriv

  use dc_message, only : MessageNotify
  use w_zonal_module
  implicit none

  integer, parameter :: im=1, jm=16, nm=10

  real(8), dimension(0:im-1,1:jm)  ::  xy_data1              ! 元の関数
  real(8), dimension(0:im-1,1:jm)  ::  xy_data2              ! 元の関数
  real(8), dimension(0:im-1,1:jm)  ::  xy_ddata              ! 微分の正解
  real(8), dimension(0:im-1,1:jm)  ::  mu                    ! μ=sinφ
  real(8), parameter               ::  eps = 1.0D-10
  real(8)                          ::  pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','w_zonal_test_deriv', &
                         'w_zonal_module derivative function tests') 

  call w_Initial( nm, im, jm )

  !---- Y_1^0 のテスト ----
  xy_data1 = sin(xy_Lat)                         ! Y_1^0

  xy_ddata = -2.0D0*sin(xy_Lat)                  ! w_Lapla_w
  if ( maxval(abs(xy_w(w_Lapla_w(w_xy(xy_data1)))-xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_Lapla_w',&
                        'Y_1^0 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of w_Lapla_w', &
                         'Test of Laplacian Y_1^0 succeeded!') 

  xy_ddata = -1.0D0/2.0D0*sin(xy_Lat)            ! w_LaplaInv_w
  if ( maxval(abs(xy_w(w_LaplaInv_w(w_xy(xy_data1)))-xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_LaplaInv_w',&
                        'Y_1^0 Inverse Laplacian error too large') 
  endif
  call MessageNotify('M','Test of w_LaplaInv_w', &
                         'Test of Inverse Laplacian Y_1^-1 succeeded!') 

  xy_ddata = 0.0D0                           ! w_DLon_w
  if ( maxval(abs(xy_w(w_DLon_w(w_xy(xy_data1)))-xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_DLon_w',&
                        'Y_1^0 Longitudinal derivative error too large') 
  endif
  call MessageNotify('M','Test of w_DLon_w', &
                         'Test of DLon Y_1^0 succeeded!') 

  xy_ddata = 0.0D0                           ! xy_GradLon_w
  if ( maxval(abs(xy_GradLon_w(w_xy(xy_data1))-xy_ddata)) > eps ) then
     call MessageNotify('E','Test of xy_GradLon_w',&
                        'Y_1^0 Longitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of xy_GradLon_w', &
                         'Test of GradLon Y_1^0 succeeded!') 

  xy_ddata = cos(xy_Lat)                    ! xy_GradLat_w
  if ( maxval(abs(xy_GradLat_w(w_xy(xy_data1))-xy_ddata)) > eps ) then
     write(6,*) maxval(abs(xy_GradLat_w(w_xy(xy_data1))-xy_ddata))
     call MessageNotify('E','Test of xy_GradLat_w',&
                        'Y_1^0 Latitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of xy_GradLat_w', &
                         'Test of GradLat Y_1^0 succeeded!') 

  !---- Y_2^0 cosφ のテスト ----
  xy_data1 = (3.0D0/2.0D0*sin(xy_Lat)**2 - 1.0D0/2.0D0)*cos(xy_Lat)
  ! Y_2^0 cosφ

  xy_ddata = 0.0D0                          ! w_DivLon_xy
  if ( maxval(abs(xy_w(w_DivLon_xy(xy_data1))-xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_DivLon_xy',&
                        'Y_2^0 cosφ Longitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of xy_DivLon_w', &
                         'Test of DivLon Y_2^0 cosφ succeeded!') 

  xy_ddata = 3*sin(xy_Lat)*(cos(xy_Lat)**2-sin(xy_Lat)**2) + sin(xy_Lat)
  ! w_DivLat_w
  if ( maxval(abs(xy_w(w_DivLat_xy(xy_data1))-xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_DivLat_xy',&
                        'Y_2^0 cosφ Latitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of w_DivLat_xy', &
                         'Test of DivLat Y_2^0 cosφ succeeded!') 

  !---- Jacobian のテスト ----
  xy_data1 = sin(xy_Lat)                             ! Y_1^0
  xy_data2 = 3.0D0/2.0D0*sin(xy_Lat)**2-1.0D0/2.0D0  ! Y_2^0

  xy_ddata = 0.0
  if ( maxval(abs(xy_w(w_Jacobian_w_w(w_xy(xy_data1),w_xy(xy_data1))) &
                  -xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_Jacobian_w_w',&
                        'Y_1^-1 Jacobian error too large') 
  endif
  call MessageNotify('M','Test of w_Jacobian_w_w', &
                         'Test of w_Jacobian_w_w Y_1^0 succeeded!') 

  xy_ddata = 0.0D0
  if ( maxval(abs(xy_w(w_Jacobian_w_w(w_xy(xy_data1),w_xy(xy_data2))) &
                  -xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_Jacobian_w_w',&
                        'Y_1^0 and Y_2^0 Jacobian error too large') 
  endif
  call MessageNotify('M','Test of w_Jacobian_w_w', &
                         'Test of w_Jacobian_w_w Y_1^0 and Y_2^0 succeeded!') 

  !============== 微分計算 (λ,μ座標系用) のテスト ==============
  mu = sin(xy_Lat)

  !----- Y_2^0 のテスト -----
  xy_data1 = 3*mu**2-1                              ! Y_2^0

  xy_ddata = 0.0
  if ( maxval(abs(xy_GradLambda_w(w_xy(xy_data1)) -xy_ddata)) > eps ) then
     call MessageNotify('E','Test of GradLambda',&
                        'Y_2^0 xy_GradLambda_w error too large') 
  endif
  call MessageNotify('M','Test of xy_GradLambda_w', &
                         'Test of xy_GradLambda_w Y_2^0 succeeded!') 

  xy_ddata = 6*mu*(1-mu**2)
  if ( maxval(abs(xy_GradMu_w(w_xy(xy_data1)) -xy_ddata)) > eps ) then
     call MessageNotify('E','Test of GradMu',&
                        'Y_2^0 xy_GradMu_w error too large') 
  endif
  call MessageNotify('M','Test of xy_GradMu_w', &
                         'Test of xy_GradMu_w Y_2^0 succeeded!') 

  !----- Y_2^0 cosφ のテスト -----
  xy_data1 = (3*mu**2-1)*(1-mu**2)                ! Y_2^0 (1-μ^2)

  xy_ddata = 0.0
  if ( maxval(abs(xy_w(w_DivLambda_xy(xy_data1)) -xy_ddata)) > eps ) then
     call MessageNotify('E','Test of DivLambda',&
                        'Y_2^0 (1-μ^2) xy_DivLambda_w error too large') 
  endif
  call MessageNotify('M','Test of xy_DivLambda_w', &
                         'Test of xy_DivLambda_w Y_2^0 (1-μ^2) succeeded!') 

  xy_ddata = (2-3*mu**2)*4*mu
  if ( maxval(abs(xy_w(w_DivMu_xy(xy_data1)) -xy_ddata)) > eps ) then
     call MessageNotify('E','Test of DivMu',&
                        'Y_2^0 (1-μ^2) xy_DivMu_w error too large') 
  endif
  call MessageNotify('M','Test of xy_DivMu_w', &
                         'Test of xy_DivMu_w Y_2^0 (1-μ^2)succeeded!') 


  call MessageNotify('M','w_test_deriv', &
                         'w_deriv_module function tests succeeded!') 

end program w_zonal_test_deriv
