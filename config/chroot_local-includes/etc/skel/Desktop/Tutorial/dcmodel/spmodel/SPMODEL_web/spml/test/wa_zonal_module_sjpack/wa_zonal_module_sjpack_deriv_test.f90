!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wa_zonal_module_sjpack テストプログラム :: 微分関数のテスト
!
!履歴  2000/09/23  竹広真一
!
program wa_zonal_module_sjpack_deriv_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wa_zonal_module_sjpack
  implicit none

  integer, parameter :: im=1, jm=16, nm=10, km=2

  real(8), dimension(0:im-1,1:jm,km)     ::  xya_data1              ! 元の関数
  real(8), dimension(0:im-1,1:jm,km)     ::  xya_data2              ! 元の関数
  real(8), dimension(0:im-1,1:jm,km)     ::  xya_ddata              ! 微分の正解
  real(8), dimension(0:im-1,1:jm)        ::  xy_mu                  ! μ=sinφ

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','wa_zonal_module__sjpack_deriv_test', &
                         'wa_zonal_module_sjpack derivative functions test') 

  call wa_Initial( nm, im, jm, km )

  !---- Y_1^-1 Y_2^1 のテスト ----
  xya_data1(:,:,1) = sqrt(3.0D0)*sin(xy_Lat)                   ! Y_1^0
  xya_data1(:,:,2) = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)  ! Y_2^0

  xya_ddata(:,:,1) = -2*sqrt(3.0D0)*sin(xy_Lat)                   ! wa_Lapla_wa
  xya_ddata(:,:,2) = -6*sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)  ! 

  call AssertEqual(&
    message='wa_Lapla_wa',                                        &
    answer = xya_ddata,                                           &
    check = xya_wa(wa_Lapla_wa(wa_xya(xya_data1))),               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = -1.0D0/2.0D0*sqrt(3.0D0)*sin(xy_Lat)        ! wa_Lapla_wa
  xya_ddata(:,:,2) = -1.0D0/6.0D0*sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)

  call AssertEqual(&
    message='wa_LaplaInv_wa',                                     &
    answer = xya_ddata,                                           &
    check = xya_wa(wa_LaplaInv_wa(wa_xya(xya_data1))),            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = 0.0D0         ! wa_DLon_wa
  xya_ddata(:,:,2) = 0.0D0

  call AssertEqual(&
    message='wa_DLon_wa',                                         &
    answer = xya_ddata,                                           &
    check = xya_wa(wa_DLon_wa(wa_xya(xya_data1))),                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = 0.0D0                     ! xya_GradLon_wa
  xya_ddata(:,:,2) = 0.0D0

  call AssertEqual(&
    message='xya_GradLon_wa',                                     &
    answer = xya_ddata,                                           &
    check = xya_GradLon_wa(wa_xya(xya_data1)),                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = sqrt(3.0D0)*cos(xy_Lat)         ! xya_GradLat_wa
  xya_ddata(:,:,2) = sqrt(5.0D0)*3.0*sin(xy_Lat)*cos(xy_Lat)

  call AssertEqual(&
    message='xya_GradLat_wa',                                     &
    answer = xya_ddata,                                           &
    check = xya_GradLat_wa(wa_xya(xya_data1)),                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_1^0 cosφ, Y_2^0 cosφ のテスト ----
  xya_data1(:,:,1) = sqrt(3.0D0)*sin(xy_Lat)*cos(xy_Lat)        
  ! Y_1^0 cosφ
  xya_data1(:,:,2) = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)*cos(xy_Lat)
  ! Y_2^0 cosφ

  xya_ddata(:,:,1) = 0.0D0   ! wa_DivLon_xya
  xya_ddata(:,:,2) = 0.0D0   ! wa_DivLon_xya

  call AssertEqual(&
    message='wa_DivLon_xya',                                      &
    answer = xya_ddata,                                           &
    check = xya_wa(wa_DivLon_xya(xya_data1)),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = sqrt(3.0D0)*(cos(xy_Lat)**2-2*sin(xy_Lat)**2)
  ! wa_DivLat_xy
  xya_ddata(:,:,2) = sqrt(5.0D0) &
       *(3.0*(sin(xy_Lat)*cos(xy_Lat)**2-sin(xy_Lat)**3)+sin(xy_Lat)) 
  ! wa_DivLat_xya

  call AssertEqual(&
    message='wa_DivLat_xya',                                      &
    answer = xya_ddata,                                           &
    check = xya_wa(wa_DivLat_xya(xya_data1)),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Jacobian のテスト ----
  xya_data1(:,:,1) = sin(xy_Lat)                             ! Y_1^0
  xya_data1(:,:,2) = 3.0D0/2.0D0*sin(xy_Lat)**2-1.0D0/2.0D0  ! Y_2^0
  xya_data2(:,:,1) = sin(xy_Lat)                             ! Y_1^0
  xya_data2(:,:,2) = 3.0D0/2.0D0*sin(xy_Lat)**2-1.0D0/2.0D0  ! Y_2^0

  xya_ddata(:,:,1) = 0.0D0
  xya_ddata(:,:,2) = 0.0D0
  call AssertEqual(&
    message='wa_Jacobian_wa_wa',                                  &
    answer = xya_ddata,                                           &
    check = xya_wa(wa_Jacobian_wa_wa(wa_xya(xya_data1),wa_xya(xya_data2))),  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !============== 微分計算 (λ,μ座標系用) のテスト ==============
  xy_mu = sin(xy_Lat)

  !----- Y_2^0, Y_1^0 のテスト -----
  xya_data1(:,:,1) = 3*xy_mu**2-1                                 ! Y_2^0
  xya_data1(:,:,2) = xy_mu                                        ! Y_1^1

  xya_ddata(:,:,1) = 0.0
  xya_ddata(:,:,2) = 0.0D0

  call AssertEqual(&
    message='xya_GradLambda_wa',                                  &
    answer = xya_ddata,                                           &
    check = xya_GradLambda_wa(wa_xya(xya_data1)),                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = 6*xy_mu*(1-xy_mu**2)
  xya_ddata(:,:,2) = 1-xy_mu**2

  call AssertEqual(&
    message='xya_GradMu_wa',                                      &
    answer = xya_ddata,                                           &
    check = xya_GradMu_wa(wa_xya(xya_data1)),                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !----- Y_2^0(1-μ^2), Y_1^0 (1-μ^2) のテスト -----
  xya_data1(:,:,1) = (3*xy_mu**2-1)*(1-xy_mu**2)       ! Y_2^0 (1-μ^2)
  xya_data1(:,:,2) = xy_mu*(1-xy_mu**2)                ! Y_1^0 (1-μ^2)

  xya_ddata(:,:,1) = 0.0D0
  xya_ddata(:,:,2) = 0.0D0

  call AssertEqual(&
    message='wa_DivLambda_xya',                                   &
    answer = xya_ddata,                                           &
    check = xya_wa(wa_DivLambda_xya(xya_data1)),                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xya_ddata(:,:,1) = (2-3*xy_mu**2)*4*xy_mu
  xya_ddata(:,:,2) = (1-xy_mu**2) - 2*xy_mu**2

  call AssertEqual(&
    message='wa_DivMu_xya',                                       &
    answer = xya_ddata,                                           &
    check = xya_wa(wa_DivMu_xya(xya_data1)),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wa_zonal_module__sjpack_deriv_test', &
       'wa_zonal_module_sjpack derivative functions test succeeded!') 

end program wa_zonal_module_sjpack_deriv_test
