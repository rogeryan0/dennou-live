!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_deriv_module テストプログラム :: 微分関数のテスト
!
!履歴  2007/10/30  竹広真一
!      2008/06/28  佐々木洋平  配列の宣言を変更
!
program w_test_deriv

  use dc_message, only : MessageNotify
  use w_module
  implicit none

  integer, parameter :: im=128, jm=64, nm=42

  real(8), dimension(0:im-1,1:jm)  ::  xy_data1              ! 元の関数
  real(8), dimension(0:im-1,1:jm)  ::  xy_data2              ! 元の関数
  real(8), dimension(0:im-1,1:jm)  ::  xy_ddata              ! 微分の正解
  real(8), dimension(0:im-1,1:jm)  ::  mu                    ! μ=sinφ
  real(8), parameter               ::  eps = 1.0D-10
  real(8)                          ::  pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','w_test_deriv', &
                         'w_deriv_module function tests') 

  call w_Initial( nm, im, jm )

  !---- Y_1^{-1} のテスト ----
  xy_data1 = -cos(xy_Lat)*sin(xy_Lon)         ! Y_1^{-1}

  xy_ddata = 2*cos(xy_Lat)*sin(xy_Lon)       ! w_Lapla_w
  if ( maxval(abs(xy_w(w_Lapla_w(w_xy(xy_data1)))-xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_Lapla_w',&
                        'Y_1^-1 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of w_Lapla_w', &
                         'Test of Laplacian Y_1^-1 succeeded!') 

  xy_ddata = 1.0/2.0*cos(xy_Lat)*sin(xy_Lon)      ! w_LaplaInv_w
  if ( maxval(abs(xy_w(w_LaplaInv_w(w_xy(xy_data1)))-xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_LaplaInv_w',&
                        'Y_1^-1 Inverse Laplacian error too large') 
  endif
  call MessageNotify('M','Test of w_LaplaInv_w', &
                         'Test of Inverse Laplacian Y_1^-1 succeeded!') 

  xy_ddata = -cos(xy_Lat)*cos(xy_Lon)         ! w_DLon_w
  if ( maxval(abs(xy_w(w_DLon_w(w_xy(xy_data1)))-xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_DLon_w',&
                        'Y_1^-1 Longitudinal derivative error too large') 
  endif
  call MessageNotify('M','Test of w_DLon_w', &
                         'Test of DLon Y_1^-1 succeeded!') 

  xy_ddata = -cos(xy_Lon)                     ! xy_GradLon_w
  if ( maxval(abs(xy_GradLon_w(w_xy(xy_data1))-xy_ddata)) > eps ) then
     call MessageNotify('E','Test of xy_GradLon_w',&
                        'Y_1^-1 Longitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of xy_GradLon_w', &
                         'Test of GradLon Y_1^-1 succeeded!') 

  xy_ddata = sin(xy_Lat)*sin(xy_Lon)         ! xy_GradLat_w
  if ( maxval(abs(xy_GradLat_w(w_xy(xy_data1))-xy_ddata)) > eps ) then
     write(6,*) maxval(abs(xy_GradLat_w(w_xy(xy_data1))-xy_ddata))
     call MessageNotify('E','Test of xy_GradLat_w',&
                        'Y_1^-1 Latitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of xy_GradLat_w', &
                         'Test of GradLat Y_1^-1 succeeded!') 

  !---- Y_2^1 cosφ のテスト ----
  xy_data1 = sin(xy_Lat)*cos(xy_Lat)**2 * cos(xy_Lon) ! Y_2^1 cosφ

  xy_ddata = -sin(xy_Lat)*cos(xy_Lat)*sin(xy_Lon)    ! w_DivLon_xy
  if ( maxval(abs(xy_w(w_DivLon_xy(xy_data1))-xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_DivLon_xy',&
                        'Y_2^1 cosφ Longitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of xy_DivLon_w', &
                         'Test of DivLon Y_2^1 cosθ succeeded!') 

  xy_ddata = cos(xy_Lat)*(1-4*sin(xy_Lat)**2)*cos(xy_Lon)      ! w_DivLat_w
  if ( maxval(abs(xy_w(w_DivLat_xy(xy_data1))-xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_DivLat_xy',&
                        'Y_2^1 cosφ Latitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of w_DivLat_xy', &
                         'Test of DivLat Y_2^1 cosθ succeeded!') 

  !---- Jacobian のテスト ----
  xy_data1 = -cos(xy_Lat)*sin(xy_Lon)                ! Y_1^{-1}
  xy_data2 = sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)   ! Y_2^1

  xy_ddata = 0.0
  if ( maxval(abs(xy_w(w_Jacobian_w_w(w_xy(xy_data1),w_xy(xy_data1))) &
                  -xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_Jacobian_w_w',&
                        'Y_1^-1 Jacobian error too large') 
  endif
  call MessageNotify('M','Test of w_Jacobian_w_w', &
                         'Test of w_Jacobian_w_w Y_1^-1 succeeded!') 

  xy_ddata = sin(xy_Lat)**2 - cos(xy_Lat)**2*cos(xy_Lon)**2
  if ( maxval(abs(xy_w(w_Jacobian_w_w(w_xy(xy_data1),w_xy(xy_data2))) &
                  -xy_ddata)) > eps ) then
     call MessageNotify('E','Test of w_Jacobian_w_w',&
                        'Y_1^-1 and Y_2^1 Jacobian error too large') 
  endif
  call MessageNotify('M','Test of w_Jacobian_w_w', &
                         'Test of w_Jacobian_w_w Y_1^-1 and Y_2^1 succeeded!') 

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

end program w_test_deriv
